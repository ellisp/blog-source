# Data prep for shiny app of modelled individual party vote in the
# New Zealand general election.
# Output is at https://ellisp.shinyapps.io/individual-vote-nzes/ 

# Overall strategy is to create a cut down version of the NZ Election Study data,
# fit a statistical model to it, and save it in a folder where a shiny app can access it.
# Source code for the shiny app is at: 
# https://github.com/ellisp/ellisp.github.io/tree/source/_working/0097

library(tidyverse)
library(foreign)
library(forcats)
library(mice)
library(h2o)
library(testthat)
library(ranger)
library(nnet)


#==========data prep===============
# Function to convert five category membership question (for trade unions, business
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

# See previous blog posts for where this comes from:
unzip("D:/Downloads/NZES2014GeneralReleaseApril16.sav.zip")

nzes_orig <- read.spss("NZES2014GeneralReleaseApril16.sav", 
                       to.data.frame = TRUE, trim.factor.names = TRUE)


#============rationalised version of feature creation===========
nzes <- nzes_orig %>%
   
   # party vote:
   mutate(dpartyvote2 = ifelse(is.na(dpartyvote), "Did not vote", as.character(dpartyvote)),
          dpartyvote2 = gsub("M.ori", "Maori", dpartyvote2),
          dpartyvote2 = gsub("net.Man", "net-Man", dpartyvote2),
          dpartyvote2 = fct_infreq(dpartyvote2)) %>%
   
   mutate(dpartyvote2 = fct_lump(dpartyvote2, 5)) %>%
   
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
   mutate(City = 1 * (dregsize == "A major city (over 100,000 population)")) %>%

      select(dpartyvote2,
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
             dage,
             dwtfin)


#------reweight to match actual party vote----------
# I want to tweak the survey weights so the percentage party vote
# for each party in total matches the official results, for maximum
# calibration of the percentages people will see to actual results.
# see http://www.elections.org.nz/news-media/new-zealand-2014-general-election-official-results
# for the source data.
# 

actual_vote <- data_frame(
   dpartyvote2 = c("National", "Labour", "Green", "NZ First", "Other", "Did not vote"),
   freq = c(1131501, 604534, 257356, 208300, 
            31850 + 16689 + 5286 + 95598 + 34095 + 10961 + 5113 + 1730 + 1096 + 872 + 639,
   NA)
)

# calculate the did not vote, from the 77.9 percent turnout
actual_vote[6, 2] <- (100 / 77.9 - 1) * sum(actual_vote[1:5, 2])

# check I did the turnout sums right:
expect_equal(0.779 * sum(actual_vote[ ,2]), sum(actual_vote[1:5, 2]))

reweight_ratios <- nzes %>%
   group_by(dpartyvote2) %>%
   summarise(survey = sum(dwtfin)) %>%
   left_join(actual_vote, by = "dpartyvote2") %>%
   mutate(ratio = freq / survey)

nzes <- nzes %>%
   left_join(reweight_ratios[, c("dpartyvote2", "ratio")]) %>%
   ungroup() %>%
   mutate(weight = dwtfin * ratio) %>%
   mutate(weight = weight / mean(weight)) %>%
   select(-dwtfin, -ratio)

#============more pre-processing===============
# identify which rows will be in test and training sets.  This was used when
# I was experimenting with neural networks and random forests in H2O, but in
# the end I fit the models that are actually used in the shiny app with
# just the full data set.
nzes$dataset <- sample(c("test", "train"), prob = c(0.2, 0.8), replace = TRUE, size =nrow(nzes))

# expand out to 10 times the size, number of repetitions based on the survey weight
# (note an alternative approach would be to use weights_column in H2O, but then we can't
# do the multiple imputation thing)
nzes_expanded <- nzes[rep(1:nrow(nzes), times = round(10 * nzes$weight)), ] %>%
   select(-weight) %>%
   mutate(dpartyvote2 = as.factor(dpartyvote2))

# add some random noise.  Probably a more R-native way of doing this than using 
# a loop!
#
# But why am I adding noise?  As a way, in combination with my imputation strategy,
# of approaching the "dropout" of data; and of better simulating the real
# noise we would get with a larger data set; and generally being less stuck
# with the relatively small sample we've got.  Rembember ultimately the aim
# is to have a set of probabilities.  Extra noise in the source data will
# flatten those probabilities, so is not dissimilar to regularisation.
n <- nrow(nzes_expanded)
for(i in 1:n){
   nzes_expanded[i, sample(2:18, 1)] <- NA
}

# split into test and training sets
nzes_test <- filter(nzes_expanded, dataset == "test")
nzes_train <- filter(nzes_expanded, dataset == "train")

# Impute the missing values back.  Note that this creates a single, complete
# data set (for each of test and train), but because there are multiple copies
# of each row as a result of the expansion by weight, the effect is similar
# to doing multiple imputation
nzes_test <- complete(mice(nzes_test, m = 1))
nzes_train <- complete(mice(nzes_train, m = 1))
nzes_full <- complete(mice(nzes_expanded, m = 1))

x <- names(nzes_train)
x <- x[!x %in% c("dpartyvote2", "dataset")]



#=============ranger random forest==========
form <- as.formula("dpartyvote2 ~ NotEuropean + Maori + HHIncome + OwnHouseOrFlat + 
                    HHMemberTradeUnion + HHMemberProfAssoc + NZBorn + Religion + 
                    Marital + HighestQual + IdentifyWorkingClass + WorkStatus + 
                    Student + SuperviseAnyone + City + Male + dage")

mod_rr <- ranger(form, data = nzes_full, probability = TRUE, importance = "impurity")

mod_mn <- multinom(form, data = nzes_full)



#============shiny prep=============
nzes_skeleton <- nzes_full[1, ]
save(nzes_skeleton, file = "0097/nzes_skeleton.rda")

save(mod_rr, mod_mn, file = "0097/models.rda")

WorkStatuses <- as.character(unique(nzes_full$WorkStatus))
HighestQuals <- as.character(unique(nzes_full$HighestQual))
Religions <- as.character(unique(nzes_full$Religion))
HHIncomes <- as.character(unique(nzes_full$HHIncome))

save(WorkStatuses, HighestQuals, Religions, HHIncomes, file = "0097/dimensions.rda")

# rsconnect::deployApp("0097", appName = "individual-vote-nzes", account = "ellisp")

#==================unused - H2O experiments=================
# Code below here was not needed for building the Shiny app.
# The neural network was a very good model but a little slow for the Shiny user
# and I was also worried about complications when it came to installing on shinyapps.io.


# Fire up h2o cluster and load data onto it:
h2o.init(nthreads = -1, max_mem_size = "8G")
nzes_test_h2o <- as.h2o(nzes_test)
nzes_train_h2o <- as.h2o(nzes_train)
nzes_full_h2o <- as.h2o(nzes_full)


#------------------neural network / deep learning-------------------
mod_grid <- h2o.grid("deeplearning", x = x, y = "dpartyvote2",
                     training_frame = nzes_train_h2o,
                     validation_frame = nzes_test_h2o,
                     search_criteria = list(strategy = "RandomDiscrete", max_models = 42),
                     hyper_params = list(
                        
                        epochs = 100,
                        hidden = list(c(20, 20), c(40, 40), c(80, 80), c(160, 160), c(320, 320)),      # default is c(200, 200)
                        rate = c(0, 0.005, 0.01, 0.02),                                                # default is 0.005
                        rate_annealing = c(1e-8, 1e-7, 1e-6),                                          # default is 1e-6
                        activation = c("RectifierWithDropout", "TanhWithDropout", "MaxoutWithDropout"),
                        hidden_dropout_ratios = list(c(0, 0), c(0.1, 0.1),  c(0.2, 0.2), c(0.5, 0.5)), # default is 0.5, 0.5
                        input_dropout_ratio = c(0, 0.1),                                               # default is 0
                        l1 = c(0, 1e-04, 1e-05),                                                       # default is 0
                        l2 = c(0, 1e-04, 1e-05)                                                        # default is 0
                     ))
# Hyper-Parameter Search Summary: ordered by increasing logloss
# activation epochs   hidden hidden_dropout_ratios input_dropout_ratio     l1     l2  rate rate_annealing
# 1      TanhWithDropout  100.0 [40, 40]            [0.5, 0.5]                 0.0    0.0 1.0E-4  0.01         1.0E-6
# 2 RectifierWithDropout  100.0 [40, 40]            [0.5, 0.5]                 0.1 1.0E-4 1.0E-5  0.02         1.0E-7
# 3      TanhWithDropout  100.0 [80, 80]            [0.5, 0.5]                 0.0 1.0E-4 1.0E-5   0.0         1.0E-8
# 4 RectifierWithDropout  100.0 [20, 20]            [0.5, 0.5]                 0.0 1.0E-4    0.0 0.005         1.0E-7
# 5 RectifierWithDropout  100.0 [40, 40]            [0.5, 0.5]                 0.0    0.0 1.0E-5 0.005         1.0E-7

# model_ids            logloss
# 1 Grid_DeepLearning_nzes_train_model_R_1494636618877_4_model_26   1.44116807364652
# 2 Grid_DeepLearning_nzes_train_model_R_1494636618877_4_model_12 1.4511058590061174
# 3 Grid_DeepLearning_nzes_train_model_R_1494636618877_4_model_34 1.4522481658109039
# 4 Grid_DeepLearning_nzes_train_model_R_1494636618877_4_model_40 1.4544414896356737
# 5  Grid_DeepLearning_nzes_train_model_R_1494636618877_4_model_3  1.459363296904898

# From the first grid it's clear the larger networks (hidden layers with more than 100 neruons)
# aren't as good as the smaller. 40 or 80 seem the best.
# Also, the higher hidden drop out ratio (0.5) is better.
# MaxoutWithDropout wasn't in any of the top rating models, but both the other activation
# methods were.  Nothing obvious to see with the input dropout ratio

mod_grid2 <- h2o.grid("deeplearning", x = x, y = "dpartyvote2",
                      training_frame = nzes_train_h2o,
                      validation_frame = nzes_test_h2o,
                      input_dropout_ratio = 0,
                      activation = "TanhWithDropout",
                      search_criteria = list(strategy = "RandomDiscrete", max_models = 42),
                      hyper_params = list(
                         
                         epochs = c(20, 50, 100, 200),
                         hidden = list(c(25, 25), c(40, 40), c(60, 60), c(80, 80)),
                         rate = c(0, 0.005, 0.01, 0.02),                                                
                         rate_annealing = c(1e-8, 1e-7, 1e-6),                                          
                         hidden_dropout_ratios = list(c(0.3, 0.3),  c(0.5, 0.5), c(0.7, 0.7)), 
                         l1 = c(0, 1e-04, 1e-05),                                                       
                         l2 = c(0, 1e-04, 1e-05)                                                        
                      ))

# Full model with the best parameters:
mod <- h2o.deeplearning(x, "dpartyvote2", 
                        training_frame = nzes_full_h2o,
                        input_dropout_ratio =0,
                        activation = "TanhWithDropout",
                        epochs = 1000,
                        hidden = c(60, 60),
                        adaptive_rate = FALSE,
                        rate = 0.02,
                        rate_annealing = 1e-8,
                        hidden_dropout_ratios = c(0.7, 0.7),
                        l1 = 1e-04,
                        l2 = 1e-04
)

h2o.confusionMatrix(mod)

#-----------------------------h2o random forest---------------------
mod_grid3 <- h2o.grid("randomForest", x = x, y = "dpartyvote2",
                      training_frame = nzes_train_h2o,
                      validation_frame = nzes_test_h2o,
                      hyper_params = list(
                         ntrees = c(50, 100, 200),
                         max_depth = c(5, 10, 20, 30)
                      ))
# depth of 5 clearly best, log loss of 1.52.  Note not as good as the 
# deep learning best result of 1.44

mod_grid4 <- h2o.grid("randomForest", x = x, y = "dpartyvote2",
                      training_frame = nzes_train_h2o,
                      validation_frame = nzes_test_h2o,
                      ntrees = 200, 
                      hyper_params = list(
                         max_depth = c(3, 4, 5, 6, 7, 8)
                      ))
# depth of 3, 4 or 5 very similar

h2o.shutdown()
