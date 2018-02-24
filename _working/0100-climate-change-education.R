# see http://grist.org/climate-energy/education-increases-belief-in-climate-change-everywhere-except-in-the-u-s/
# and https://twitter.com/ForecasterEnten/status/870459922890280963 


library(tidyverse)
library(foreign)
library(forcats)
library(testthat)
library(nzelect)

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
   mutate(MoreEnvironment = 1 * dspenviro %in% c("More", "Much more"))

#=========================

# free text write-in:
unique(nzes_orig$dimpissue)

issues <- as.character(nzes_orig$dimpissue)
issues[grepl("CLIMATE", issues)]


unique(nzes_orig$DIMPI0)

unique(nzes_orig$dissuegen)

# "C4i: should there be more or less public spending on the environment"
table(nzes_orig$dspenviro)

# "C8: where you stand between protecting the environment and promoting economic development"
table(nzes_orig$decovsenv)
matplot(with(nzes_orig, table(dspenviro, decovsenv)))

# people who want to spend more on the environment are younger
ggplot(nzes, aes(x = dspenviro, y = dage)) +
   geom_boxplot() +
   facet_wrap(~dpartyvote2) 


ggplot(nzes, aes(x = dage, y = MoreEnvironment)) +
   geom_jitter() +
   geom_smooth(method = "lm")

ggplot(nzes, aes(x = dage, y = MoreEnvironment)) +
   geom_jitter(height = 0.1) +
   geom_smooth(method = "lm") +
   facet_wrap(~HighestQual)


mod1 <- glm(MoreEnvironment ~ dage + HighestQual, family = "binomial", data = nzes)
summary(mod1)

mod2 <- glm(MoreEnvironment ~ HighestQual, family = "binomial", data = nzes)
summary(mod2)

anova(mod1, test = "Chi")
