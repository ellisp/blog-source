library(tidyverse)
library(scales)
library(foreign)   # for importing SPSS data
library(survey)    # for survey weighting and analysis
library(forcats)   # for manipulating factor levels
library(mgcv)      # for generalized additive models
library(glmnet)    # for elastic net regularisation
library(mice)      # for imputation
library(testthat)
library(broom)     # for reshaping model outputs
library(stringr)   # for str_wrap
library(boot)
library(doParallel)
library(gridExtra)
library(nzelect)   # for party colours
library(lme4)      # for mixed effects / multilevel modelling

#-------------convenience functions--------------
camel_to_english <- function(camelCase){
   return(gsub("([a-z])([A-Z])", "\\1 \\L\\2", camelCase, perl = TRUE))
}

# Convert five category membership question (for trade unions, business
# associations, etc) into Yes or No.
membership <- function(x){
   tmp <- fct_recode(x,
                     Yes = "I belong, but no one else in the house does",
                     Yes = "I do, plus another in the house",
                     Yes = "Another person belongs, but not me",
                     No  = "No, no one belongs",
                     No  = "Don't know") 
   tmp <- ifelse(tmp == "Yes", 1, 0)
   # Uncomment the next line if we want to turn NA into 0.
   # tmp <- ifelse(is.na(tmp), 0, tmp)
   return(tmp)
}

# Draw confidence interval segment chart for two types of models:
# those with variance-covariance matrices, and hence can use confint()
# on them; and pooled models created by with.mice() after multiple
# imputation.
sum_plot <- function(model, 
                     title = NULL, 
                     party = gsub("PartyVote", "", as.character(model$formula)[2]),
                     colour = "#000000",
                     type = c("vcov", "pool")){
   type <- match.arg(type)
   if(type == "vcov"){
      conf_ints <- cbind(tidy(confint(model)), coef(model))
   } else {
      conf_ints <- tidy(summary(pool(model))) %>%
         select(.rownames, lo.95, hi.95, est)
   }
   
   tmp <- conf_ints %>%
      filter(.rownames != "(Intercept)")
   names(tmp)    <- c("var", "lower", "upper", "point")

   p <- tmp %>%
      mutate(var = camel_to_english(var),
             var = fct_reorder(var, point)) %>%
      ggplot(aes(y = var))  +
      geom_vline(xintercept= 0) +
      geom_segment(aes(x = lower, xend = upper, yend = var), 
                   size = 3, colour = colour, alpha = 0.5) +
      geom_point(aes(x = point)) +
      ggtitle(title) +
      labs(caption = "New Zealand Election Survey, analysed at https://ellispgithub.io",
           x = paste("Impact on log odds of voting", party),
           y = str_wrap("Compared to 'no religion', ''age30-55', 'high household income', 'school qualification', 'working full time", 50))
   return(p)
}


#------------------data download-----------------

# Data downloaded from http://www.nzes.org/exec/show/data and because
# they want you to fill in a form to know who is using the data, I
# won't re-publish it myself
unzip("D:/Downloads/NZES2014GeneralReleaseApril16.sav.zip")

nzes_orig <- read.spss("NZES2014GeneralReleaseApril16.sav", 
                  to.data.frame = TRUE, trim.factor.names = TRUE)
varlab <- cbind(attributes(nzes_orig)$variable.labels)

# DT::datatable(varlab)

# note - party vote v verified party vote


#============rationalised version of feature creation===========
# This is a single 100 line command to aggregate various answers into
# simpler cateogrisations, because we don't have enough data to 
# analyse the original granular detail.
nzes <- nzes_orig %>%
   
   # party vote:
   mutate(dpartyvote2 = ifelse(is.na(dpartyvote), "Did not vote", as.character(dpartyvote)),
          dpartyvote2 = gsub("M.ori", "Maori", dpartyvote2),
          dpartyvote2 = gsub("net.Man", "net-Man", dpartyvote2),
          dpartyvote2 = fct_infreq(dpartyvote2)) %>%
   
   mutate(PartyVoteNZFirst = (dpartyvote2 == "NZ First"),
          PartyVoteLabour =  (dpartyvote2 == "Labour"),
          PartyVoteNational = (dpartyvote2 == "National"),
          PartyVoteGreen    = (dpartyvote2 == "Green")) %>%
   # voted at all (needed for weighting):
   mutate(Voted = 1 * (ddidvote == 1)) %>%
   
   # Two degrees of freedom for ethnicity:
   mutate(NotEuropean = 1 - dethnicity_e,
          Maori = dethnicity_m) %>%
   
   # Two degrees of freedom for income (lower, higher, don't know):
   mutate(HHIncome = fct_recode(dhhincome,
                                Lower = "No income",
                                Lower = "$NZ23,000 or less",
                                Lower = "$NZ23,001-$NZ31,000",
                                Lower = "$NZ31,001-$NZ39,800",
                                Lower = "$NZ39,801-$NZ55,000",
                                Higher = "$NZ55,001-$NZ76,100",
                                Higher = "$NZ76,101-$NZ110,800",
                                Higher = "$NZ110,801-$NZ147,699",
                                Higher = "$NZ147,700 or over",
                                `Don't know / NA` = "Don't know"),
          HHIncome = ifelse(is.na(HHIncome), "Don't know / NA", as.character(HHIncome)),
          HHIncome = fct_infreq(HHIncome)) %>%
   
   ## Two - four degrees of freedom for associations?
   mutate(HHMemberTradeUnion = membership(dtradeunion),
          HHMemberProfAssoc = membership(dprofassoc)) %>%
   
   ## One degree of freedom for born in NZ
   mutate(NZBorn = ifelse(dnzborn == "New Zealand", 1, 0)
          # uncomment the next line if you want to turn NA into zero:
          # , NZBorn = ifelse(is.na(NZBorn), 0, NZBorn)
   ) %>%
   
   ## One for sex
   mutate(Male = 1 * (dsex == "Male"),
          Male = ifelse(dsex == "Transsexual or transgender", NA, Male)) %>%
   
   ## Two-four for age
   mutate(Age = fct_collapse(as.character(dage),
                             `18-29` = as.character(18:29),
                             `30-55` = as.character(30:55),
                             `56+` = as.character(56:100)),
          # Uncomment the next line if you want to explicitly code the NAs
          # Age = ifelse(is.na(dage), "unknown", as.character(Age)),
          Age = fct_relevel(Age, "30-55")
   ) %>%
   
   ## One for housing.  Note there is also an alternative question "do you or any family member own a residence"
   mutate(OwnHouseOrFlat = 1 * grepl("Own house or flat", dhousing)) %>%
   
   # Two for religion
   mutate(Religion = fct_lump(dreligion, 2)) %>%
   
   # One for marital status
   mutate(Marital = 1 * (dmarital == "Married, in a civil union, or living with a partner")) %>%
   
   # One for self-identified class
   mutate(IdentifyWorkingClass = 1 * (dclass == "Working class")) %>%
   
   ## Two for education (University, None, Other)
   mutate(HighestQual = fct_collapse(dedcons, University = c("Undergraduate Degree", "Postgraduate degree", "PhD")),
          HighestQual = fct_lump(HighestQual, 2),
          HighestQual = ifelse(dedcons == "Not known", NA, as.character(HighestQual)),
          HighestQual = fct_relevel(HighestQual, "Other")
   ) %>%
   
   ## Two for working status
   mutate(WorkStatus = ifelse(!is.na(dwkpt), "Part time", NA),
          WorkStatus = ifelse(!is.na(dwkft), "Full time", WorkStatus),
          WorkStatus = ifelse(is.na(WorkStatus), "Other or unknown", WorkStatus),
          WorkStatus = fct_infreq(WorkStatus),
          Student = 1 * (!is.na(dwksch))) %>%
   
   ## One for occupation
   mutate(SuperviseAnyone = 1 * (dsupervise == "Yes")) %>%
   # Note there is detailed occupation information (for respondent and partner)
   # but I don't think we hav eneough data to use this in the model.
   
   ## None for industry.
   # We have industry of employhment for respondent and partner
   # but I don't think we have enough data to use this.  Plus many people
   # don't know what industry they are in anyway.
   #
   
   ## One degree of freedom for area lived in?
   # Five nice categories here, not enough data so we'll split into one
   mutate(City = 1 * (dregsize == "A major city (over 100,000 population)")) 

#============filter and stockcheck missing data===========
# nzes_subset is a subset of all the columns in the original nzes.
# we want a dataframe with only the variables we intend to use;
# will use it down the track for imputation.
nzes_subset <- nzes %>%
   select(PartyVoteNZFirst,
          PartyVoteNational,
          PartyVoteLabour,
          PartyVoteGreen,
          NotEuropean, Maori,
          HHIncome, 
          OwnHouseOrFlat,
          HHMemberTradeUnion, HHMemberProfAssoc,
          NZBorn,
          Religion,
          Marital,
          HighestQual,
          IdentifyWorkingClass,
          WorkStatus,
          Student,
          SuperviseAnyone,
          City,
          Male,
          Age,
          dage,
          dwtfin,
          Voted)

# 29% rows missing at least one value:
sum(complete.cases(nzes_subset)) / nrow(nzes_subset)

#===========modelling===========

# For convenience, here is a model formulation we'll be using several times:
form <- as.formula(PartyVoteNZFirst ~ 
                      NotEuropean + Maori + 
                      HHIncome + 
                      OwnHouseOrFlat +
                      HHMemberTradeUnion + HHMemberProfAssoc +
                      NZBorn +
                      Religion +
                      Marital + 
                      HighestQual +
                      IdentifyWorkingClass +
                      WorkStatus +
                      Student +
                      SuperviseAnyone +
                      City +
                      Male +
                      Age)

#---------glm v svyglm with knocking out the NAs--------------------
model_naive <- glm(form, data = nzes, family = "binomial")

svg("../img/0096-glm-simple.svg", 8, 7)
sum_plot(model_naive, "Generalized linear model, no weights or imputation")
dev.off()

nzes_svy1 <- svydesign(~1, data = nzes, weights = ~dwtfin)

model_svy1 <- svyglm(form, design = nzes_svy1, family = "quasibinomial")
svg("../img/0096-svyglm.svg", 8, 7)
sum_plot(model_svy1, "Survey-specific `svyglm`, survey weights, no imputation")
dev.off()

model_glmw <- glm(form, data = nzes, family = "quasibinomial", weights = dwtfin)
svg("../img/0096-glm-weights.svg", 8, 7)
sum_plot(model_glmw, "'Usual' `glm`, survey weights, no imputation")
dev.off()

#---------------GAM with weights------------------------------
model_gam <- gam(PartyVoteNZFirst ~ 
                    NotEuropean + Maori + 
                    HHIncome + 
                    OwnHouseOrFlat +
                    HHMemberTradeUnion + HHMemberProfAssoc +
                    NZBorn +
                    Religion +
                    Marital + 
                    HighestQual +
                    IdentifyWorkingClass +
                    WorkStatus +
                    Student +
                    SuperviseAnyone +
                    City +
                    Male +
                    s(dage),
              data = nzes, family = "quasibinomial", weights = dwtfin)
summary(model_gam)
# confint doesn't work with gam see eg this unanswered question:
# http://grokbase.com/t/r/r-help/123agwjxr6/with-confidence-intervals-for-gam-model-using-mgcv

svg("../img/0096-gam.svg", 8, 7)
plot(model_gam, shade = TRUE, main = "Non-linear impact of age on voting for NZ First")
dev.off()

#------------------------Elastic net regularisation, with imputation-------------------
X <- model.matrix(PartyVoteNZFirst ~ 
                     NotEuropean + Maori + 
                     HHIncome + 
                     OwnHouseOrFlat +
                     HHMemberTradeUnion + HHMemberProfAssoc +
                     NZBorn +
                     Religion +
                     Marital + 
                     HighestQual +
                     IdentifyWorkingClass +
                     WorkStatus +
                     Student +
                     SuperviseAnyone +
                     City +
                     Male +
                     Age,
                  data = nzes_imp)

model_net <- cv.glmnet(y = nzes_imp$PartyVoteNZFirst, x = X[ , -1], alpha = 0,
                       family = "binomial")

# all are shrunk practically to zero (and exactly to zero if use the lasso, alpha = 1, instead):
coef(model_net)

#------------------regional multilevel model-----------------------
table(nzes$kregcon, useNA = "always") # too many missing regions to use
table(nzes$delect, useNA = "always")  # only four missing electorates

# Note that glmer interprets "weights" as meaning number of trials
# for a binomial glm, so I'm ignoring them

nzes <- nzes %>%
   mutate(Electorate = ifelse(is.na(delect), "unknown", delect))

model_ml_null <- glmer(PartyVoteNZFirst ~ (1 | Electorate), 
                       data = nzes, family = "binomial")
summary(model_ml_null) # standard deviation of 0.3297, quite noticeable


model_ml <- glmer(PartyVoteNZFirst ~ NotEuropean + Maori + HHIncome + OwnHouseOrFlat + 
                     HHMemberTradeUnion + HHMemberProfAssoc + NZBorn + Religion + 
                     Marital + HighestQual + IdentifyWorkingClass + WorkStatus + 
                     Student + SuperviseAnyone + City + Male + Age + (1 | Electorate), 
                  data = nzes, family = "binomial")

summary(model_ml)
# but "electorate effect" completely disappears to nearly zero when individual variables were included
# - at least for NZ First.  For other parties there is a) problem converging and b)
# there does seem to be a residual electorate effect



#=============bootstrap, imputation, recalibrate weights, svyglm===============
# First we want to explore the weights
# Calculate population totals for non-vote, age, gender and education

# Some exploration of the weights:
mod <- lm(dwtfin ~ . -dage, data = nzes_subset)

svg("../img/0096-weights.svg", 8, 7)
tidy(mod) %>%
   filter(term != "(Intercept)") %>%
   mutate(term = fct_reorder(term, estimate)) %>%
   ggplot(aes(x = estimate, y = term)) + 
   geom_point() +
   labs(x = "Impact on weight") +
   ggtitle("Relative weights of different demographics, NZ Election Study 2014",
           "Voters, M\u0101ori, women, older, and university qualified people are over-sampled\nand have low weights to compensate")
dev.off()
# The survey description says weights control for age, gender, education and Maori-ness and whether voted

#--------------define population marginal totals that we will force weights to add up to-------------
age_pop <- nzes_subset %>%
   group_by(Age) %>%
   summarise(Freq = sum(dwtfin)) %>%
   filter(!is.na(Age))

male_pop <- nzes_subset %>%
   group_by(Male) %>%
   summarise(Freq = sum(dwtfin)) %>%
   filter(!is.na(Male))

qual_pop <- nzes_subset %>%
   group_by(HighestQual) %>%
   summarise(Freq = sum(dwtfin)) %>%
   filter(!is.na(HighestQual))

maori_pop <- nzes_subset %>%
   group_by(Maori) %>%
   summarise(Freq = sum(dwtfin)) %>%
   filter(!is.na(Maori))

voted_pop <- nzes_subset %>%
   group_by(Voted) %>%
   summarise(Freq = sum(dwtfin)) 

#---------------define a bootstrap function-------------
# Function that does imputation, calibration of weights, and fits
# a survey GLM to the result:
imp_cal_fit <- function(x, i){
   # for dev:
   # x <- nzes_subset; i <- sample(1:nrow(x), nrow(x), replace = TRUE)
   
   # Resample the data
   nzes_res <- x[i, ]
   
   # Create a single complete, imputed version of the data
   nzes_imp <- complete(mice(nzes_res, 1, printFlag = FALSE))
   attributes(nzes_imp$Religion)$contrasts    <- NULL
   attributes(nzes_imp$WorkStatus)$contrasts  <- NULL
   attributes(nzes_imp$HHIncome)$contrasts    <- NULL
   attributes(nzes_imp$HighestQual)$contrasts <- NULL
   attributes(nzes_imp$Age)$contrasts         <- NULL
   
   # Set up as a survey object
   nzes_svy <- svydesign(~1, data = nzes_imp, weights = ~dwtfin)
   
   # force the marginal totals to match those from the original weighting
   nzes_svy_cal <- calibrate(nzes_svy, 
                         list(~Maori, ~Age, ~HighestQual, ~Male, ~Voted), 
                         list(maori_pop, age_pop, qual_pop, male_pop, voted_pop))
   
   # Fit the model
   model_svy_cal <- svyglm(form, design = nzes_svy_cal, family = "quasibinomial")
   
   
   # Return the results
   return(coef(model_svy_cal))
}

# Set up a cluster for parallel computing
cluster <- makeCluster(7) # only any good if you have at least 7 processors :)
registerDoParallel(cluster)

clusterEvalQ(cluster, {
   library(survey)
   library(mice)
})

# to mimic the original deliberate over-sampling of Maori, I make the bootstrap
# resampling stratify itself on Maori too.
# Note that this takes about 90 minutes, even with the parallel computing.  Each resample
# has to do the iterative imputation from scratch, then calibrate survey weights.
system.time({
nzes_booted <- boot(nzes_subset, imp_cal_fit, 
                    R = 999, strata = nzes_subset$Maori, 
                    parallel = "snow", cl = cluster)
})

# It's not common to do it this way.  More usually you make a set of many replicate
# weights and calibrate them as a once-off job, then use those weights for any future
# estimation.  But this doesn't conveniently fit in a workflow where we are doing a 
# new imputation for each bootstrap resample (which is usually admitted to be good 
# practice, but very rarely done, precisely because it's inconvenient for workflow).


# extract the 95% confidence intervals
k <- ncol(nzes_booted$t)
results <- as.data.frame(t(sapply(1:k, function(i){boot.ci(nzes_booted, type = "perc", index = i)$percent[4:5]})))
names(results) <- c("lower", "upper")
results$term <- names(coef(model_naive))

pb <- results %>%
   filter(term != "(Intercept)") %>%
   mutate(term = camel_to_english(term),
          mid = (lower + upper ) / 2,
          term = fct_reorder(term, mid)) %>%
   ggplot(aes(y = term)) +
   geom_vline(xintercept = 0) +
   geom_segment(aes(x = lower, xend = upper, yend = term), 
                colour = parties_v["NZ First"], alpha = 0.5, size = 3) +
   ggtitle("Party vote for New Zealand First in the 2014 election",
      "Bootstrap, imputation, recalibrated survey weights, svyglm") +
   labs(caption = "New Zealand Election Survey, analysed at https://ellispgithub.io",
        x = "Impact on log odds of voting New Zealand First",
        y = str_wrap("Compared to 'no religion', 'age30-55', 'high household income', 'school qualification', 'working full time'", 50))

svg("../img/0096-svyglm-boot.svg", 8, 7)
print(pb)
dev.off()



#============multiple imputation all four main parties===============
# see http://r-survey.r-forge.r-project.org/survey/svymi.html for an alternative way to do this
# Also see https://stats.stackexchange.com/questions/149053/questions-on-multiple-imputation-with-mice-for-a-multigroup-sem-analysis-inclu

nzes_mi <- mice(nzes_subset)

attributes(nzes_mi$data$Age)$contrasts <- NULL
attributes(nzes_mi$data$Religion)$contrasts <- NULL
attributes(nzes_mi$data$HighestQual)$contrasts <- NULL
attributes(nzes_mi$data$WorkStatus)$contrasts <- NULL
attributes(nzes_mi$data$HHIncome)$contrasts <- NULL

responses <- paste0("PartyVote", c("National", "Labour", "NZFirst", "Green"))
colours <- parties_v[c("National", "Labour", "NZ First", "Green")]
p <- list()

form_gen <- "XXX ~ NotEuropean + Maori + HHIncome + OwnHouseOrFlat +
HHMemberTradeUnion + HHMemberProfAssoc + NZBorn + Religion +
Marital + HighestQual + IdentifyWorkingClass + WorkStatus +
Student + SuperviseAnyone + City + Male + Age"

for(i in 1:length(responses)){
   form2 <- gsub("XXX", responses[i], form_gen)
   model_mi <- with(data = nzes_mi,
                    glm(as.formula(form2), 
                        family = "quasibinomial", weights = dwtfin))
   p[[i]] <- sum_plot(model_mi, 
                      title = paste(gsub("PartyVote", "Party vote for ", responses[i]),
                                    "compared to rest of population"),
                      colour = colours[i],
                      type = "pool",
                      party = gsub("PartyVote", "", responses[i]))
}

svg("../img/0096-all-parties.svg", 15, 14)
grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]])
dev.off()

convert_pngs("0096")

thankr::shoulders() %>%
   mutate(maintainer = gsub("<.*>", "", maintainer)) %>%
   group_by(maintainer) %>%
   summarise(no_packages = sum(no_packages)) %>%
   arrange(desc(no_packages)) %>%
   as.data.frame()
