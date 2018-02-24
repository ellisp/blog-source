library(tidyverse)
library(scales)
library(foreign)   # for importing SPSS data
library(survey)    # for survey weighting and analysis
library(forcats)   # for manipulating factor levels
library(mice)

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

#------------------data download-----------------

# Data downloaded from http://www.nzes.org/exec/show/data and because
# they want you to fill in a form to know who is using the data, I
# won't re-publish it myself
unzip("D:/Downloads/NZES2014GeneralReleaseApril16.sav.zip")

nzes_orig <- read.spss("NZES2014GeneralReleaseApril16.sav", 
                       to.data.frame = TRUE, trim.factor.names = TRUE)
varlab <- cbind(attributes(nzes_orig)$variable.labels)

#============rationalised version of feature creation===========
# This is a single 100 line command to aggregate various answers into
# simpler cateogrisations, because we don't have enough data to 
# analyse the original granular detail.
nzes <- nzes_orig %>%
  
  # Two degrees of freedom for ethnicity:
  # Oddly, this has changed since a few months ago.  An upgrade to foreign (I think)
  # means that the SPSS data comes in with its labels.  So this bit of code is slightly
  # different to some I'd previously blogged about:
  mutate(NotEuropean = 1 - (dethnicity_e == "NZ European"),
         Maori = 1 * (as.numeric(dethnicity_m) == 2)) %>%
  
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
  
  ## One degree of freedom for area lived in?
  # Five nice categories here, not enough data so we'll split into one
  mutate(City = 1 * (dregsize == "A major city (over 100,000 population)")) %>%
  
  mutate(Voted = 1 * (ddidvote == "Did cast a vote"))

# Mysterious Code 9 from outer space (in the original, see http://www.jackvowles.com/SectionD2014.htm)
is.na(nzes$Voted) <- (nzes$ddidvote == "9")

#==============descriptive stats==================
# create a survey design object that understand the weights:
nzes_svy <- svydesign(~1, weights = ~dwtfin, data = nzes)


#' Function to estimate a survey cross tab and draw a mosaic plot, comparing
#' a given variable to whether the person voted (excluding those who said "9"
#' in response to the question on whether they voted)
mp <- function(variable, ylab = ""){
  svg(paste0("../img/0104-mosaic-", variable, ".svg"), 8.5, 7)
  form <- as.formula(paste("~", variable, "+ ddidvote"))
  tab <- svytable(form, nzes_svy)
  oldpar <- par(font.main = 1)
  par(family = "myfont")
  mosaicplot(t(tab[ , -4]), shade = TRUE, las = 2, 
             main = "New Zealand Election Study 2014",
             xlab = "D2: Did you vote or not vote?",
             ylab = ylab)
  dev.off()
}

mp("dinterest", "A1: how interested in politics")
mp("ddiffpower", "A10: does it make a difference who is in power")
mp("ddemo", "A12: how satisfied with how democracy works in NZ")
mp("ddiffvoting", "A11: does voting make any difference to what happens")
mp("dbigbus", "C7m: big business in NZ has too much power")
mp("dhhincome", "F26: household income between 1 April 2013 and 31 March 2014")


# too much missing data - nearly a third of rows missing a column - for this to be any use, but as a taster:
mod1 <- svyglm(Voted ~ NotEuropean + Maori + HHIncome + HHMemberTradeUnion + HHMemberProfAssoc +
                 NZBorn + Male + Age + OwnHouseOrFlat + Religion + Marital +
                 IdentifyWorkingClass + HighestQual + WorkStatus + 
                 Student + SuperviseAnyone + City, 
               design = nzes_svy, family = "quasibinomial")
summary(mod1)
anova(mod1)


#============with imputation===============
# create a dataset of just the variables we're using:
nzes_subset <- nzes %>%
  select(Voted, NotEuropean, Maori, HHIncome, HHMemberTradeUnion,
         HHMemberProfAssoc, NZBorn, Male, Age, OwnHouseOrFlat,
         Religion, Marital, IdentifyWorkingClass, HighestQual, WorkStatus,
         Student, SuperviseAnyone, City, dwtfin)

# check how many complete cases there are:
sum(complete.cases(nzes_subset))
nrow(nzes_subset)

# create 5 different datasets with (different) imputed values where missing
nzes_imp <- mice(nzes_subset)

# note that these contrasts are now in the $data object - some time in the 
# last year this changed, compared to what worked in my 6 May 2017 blog.
attributes(nzes_imp$data$Religion)$contrasts    <- NULL
attributes(nzes_imp$data$WorkStatus)$contrasts  <- NULL
attributes(nzes_imp$data$HHIncome)$contrasts    <- NULL
attributes(nzes_imp$data$HighestQual)$contrasts <- NULL
attributes(nzes_imp$data$Age)$contrasts         <- NULL

# fit model
mod2 <- with(nzes_imp, 
             glm(Voted ~ NotEuropean + Maori + HHIncome + HHMemberTradeUnion + HHMemberProfAssoc +
                              NZBorn + Male + Age + OwnHouseOrFlat + Religion + Marital +
                              IdentifyWorkingClass + HighestQual + WorkStatus + 
                              Student + SuperviseAnyone + City, 
                 family = "quasibinomial", weights = dwtfin))

# turn into a graphic:
coefs <- as.data.frame(summary(pool(mod2)))
coefs$variable <- camel_to_english(row.names(coefs))

svg("../img/0104-model-results.svg", 11, 8)
coefs %>%
  slice(-1) %>%
  mutate(variable = fct_reorder(variable, t)) %>%
  ggplot(aes(x = `lo 95`, xend = `hi 95`, y = variable, yend = variable)) +
  geom_vline(xintercept = 0, size = 2.5, colour = "orange") +
  geom_segment(size = 5, colour = "steelblue", alpha = 0.8) +
  ggtitle("Who voted in New Zealand's Election 2014?",
          "Of those enrolled to vote, when controlling for other variables, which variables were associated with voting.
Width of blue bars indicates uncertainty associated with sampling and imputation.") +
  labs(x = "Less likely to vote                        -----------------                            More likely to vote                           ",
       y = "Compared to enrollees of no religion, aged 30-55, 
high household income, school qualification, 
working full time",
       caption = "Source: New Zealand Election Study; analysis at http://ellisp.github.io")
dev.off()


#=================who is Maori=======================
mod3 <- with(nzes_imp, 
             glm(Maori ~ NotEuropean + HHIncome + HHMemberTradeUnion + HHMemberProfAssoc +
                   NZBorn + Male + Age + OwnHouseOrFlat + Religion + Marital +
                   IdentifyWorkingClass + HighestQual + WorkStatus + 
                   Student + SuperviseAnyone + City, 
                 family = "quasibinomial", weights = dwtfin))


coefs3 <- as.data.frame(summary(pool(mod3)))
coefs3$variable <- camel_to_english(row.names(coefs3))

coefs3 %>%
  arrange(t)

convert_pngs("0104")
