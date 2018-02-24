# This excerpt of code used to be in 0096-nz-first.R.  It is basically all the exploration 
# of individual variables, decisions about how to include which variables and in how
# many categories, etc.

#=============Party Vote============
table(nzes$dpartyvote, useNA = "always")

nzes <- nzes %>%
   mutate(dpartyvote2 = ifelse(is.na(dpartyvote), "Did not vote", as.character(dpartyvote)),
          dpartyvote2 = gsub("M.ori", "Maori", dpartyvote2),
          dpartyvote2 = gsub("net.Man", "net-Man", dpartyvote2),
          dpartyvote2 = fct_infreq(dpartyvote2))


nzes_svy <- svydesign(~1, data = nzes, weights = ~dwtfin)
tmp <- cbind(
   svytable(~dpartyvote2, nzes_svy, round = TRUE),
   table(nzes$dpartyvote2)
) 
colnames(tmp) <- c("Weighted", "Unweighted")
tmp

mean(nzes$dwtfin) # nearly 1
# "did not vote" are undersampled and hence show up as more prominent
# in the weighted version than the raw counts.

# With about 200 NZ First voters we can use at most 10-20 degrees of freedom in modelling them.
# ie 10-20 continuous variables, or 2-4 factor variables with six levels each (5 degrees of freedom
# per factor in that case).  So we want to be careful what we choose

nzes <- nzes %>%
   mutate(PartyVoteNZFirst = (dpartyvote2 == "NZ First"),
          PartyVoteLabour =  (dpartyvote2 == "Labour"),
          PartyVoteNational = (dpartyvote2 == "National"),
          PartyVoteGreen    = (dpartyvote2 == "Green"))

#=====some standard demographic and SES variables==========
nzes <- nzes_orig %>%
   mutate(Voted = 1 * (ddidvote == 1))


## Two degrees of freedom for ethnicity
svytable( ~ dethnicity_e + dethnicity_m, nzes_svy) 
# Maori by European.  Note 197 who say both.
# So two explanatory variables will be these dummies, total of 2 df
nzes <- nzes %>%
   mutate(NotEuropean = 1 - dethnicity_e,
          Maori = dethnicity_m)


svytable(~dethnicfinal, nzes_svy)                  
# consolidated ethnicity has 51 levels, too many for our purpose

## Two degrees of freedom for income
svytable(~dhhincome, nzes_svy) 
# 10 levels including "no income" and "don't know".  
# Several ways we could proceed.  Median HH income in NZ is about $46k.
# If we split everyone into three categories of Higher, Lower, and don't know / NA
# we use two degrees of freedom

# so many people are missing income that I want to code it explicitly
nzes <- nzes %>%
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
          HHIncome = fct_infreq(HHIncome))




## Two - four degrees of freedom for associations?
svytable(~dtradeunion, nzes_svy) # about 410
svytable(~dbusassoc, nzes_svy)   # about 180
svytable(~dfarmassoc, nzes_svy)  # about 100
svytable(~dprofassoc, nzes_svy)  # about 460
# I decide I can spare a degree of freedom each for the trade union and professional association,
# but not for business and farm associations.  I decide to wrap "don't know" into "no one belongs"

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

nzes <- nzes %>%
   mutate(HHMemberTradeUnion = membership(dtradeunion),
          HHMemberProfAssoc = membership(dprofassoc))

## One degree of freedom for born in NZ
svytable(~dnzborn, nzes_svy) # about 30 categories, too many
nzes <- nzes %>%
   mutate(NZBorn = ifelse(dnzborn == "New Zealand", 1, 0)
          # uncomment the next line if you want to turn NA into zero:
          # , NZBorn = ifelse(is.na(NZBorn), 0, NZBorn)
   )

## One for sex
table(nzes$dsex, useNA = "always")
# can only afford one degree of freedom, so will make the transgender people NA,
# which will knock them out of naive modelling, and mean they get imputed different values
# different times during the more sophisticated imputation-bootstrap cycle
nzes <- nzes %>%
   mutate(Male = 1 * (dsex == "Male"),
          Male = ifelse(dsex == "Transsexual or transgender", NA, Male))

## Two-four for age
# We have age in years.  If we could use a gam we would apply a spline, but we can't
# do that with svyglm.
svytable(~dage, nzes_svy)
nzes <- nzes %>%
   mutate(Age = fct_collapse(as.character(dage),
                             `18-29` = as.character(18:29),
                             `30-55` = as.character(30:55),
                             `56+` = as.character(56:100)),
          # Uncomment the next line if you want to explicitly code the NAs
          # Age = ifelse(is.na(dage), "unknown", as.character(Age)),
          Age = fct_relevel(Age, "30-55")
   )

table(nzes$Age, useNA = "always")

## One for housing
table(nzes$dhousing)
nzes <- nzes %>%
   mutate(OwnHouseOrFlat = 1 * grepl("Own house or flat", dhousing))
# Note there is also an alternative question "do you or any family member own a residence"


# Two for religion
table(nzes$dreligion)
nzes <- nzes %>%
   mutate(Religion = fct_lump(dreligion, 2))
table(nzes$Religion, useNA = "always")

# One for marital status
nzes <- nzes %>%
   mutate(Marital = 1 * (dmarital == "Married, in a civil union, or living with a partner"))

# One for self-identified class
nzes <- nzes %>%
   mutate(IdentifyWorkingClass = 1 * (dclass == "Working class"))

## Two for education
table(nzes$dedcons)
nzes <- nzes %>%
   mutate(HighestQual = fct_collapse(dedcons, University = c("Undergraduate Degree", "Postgraduate degree", "PhD")),
          HighestQual = fct_lump(HighestQual, 2),
          HighestQual = ifelse(dedcons == "Not known", NA, as.character(HighestQual)),
          HighestQual = fct_relevel(HighestQual, "Other")
   )
table(nzes$HighestQual, useNA = "always")


## Two for working status
# these seem to have already been set up as dummy variables with values or NAs,
# but there are 7 people who work both part time and full time,
nzes %>%
   group_by(dwkpt, dwkft) %>%
   summarise(count = n()) %>%
   View

nzes <- nzes %>%
   mutate(WorkStatus = ifelse(!is.na(dwkpt), "Part time", NA),
          WorkStatus = ifelse(!is.na(dwkft), "Full time", WorkStatus),
          WorkStatus = ifelse(is.na(WorkStatus), "Other or unknown", WorkStatus),
          WorkStatus = fct_infreq(WorkStatus),
          Student = 1 * (!is.na(dwksch)))
table(nzes$Student)
table(nzes$WorkStatus, useNA = "always")

## One for occupation
table(nzes$dsupervise)
nzes <- nzes %>%
   mutate(SuperviseAnyone = 1 * (dsupervise == "Yes"))

# Note there is detailed occupation information (for respondent and partner)
# but I don't think we hav eneough data to use this in the model.
table(nzes$dresp_isco3dig)

## None for industry?
# We have industry of employhment for respondent and partner
# but I don't think we have enough data to use this.  Plus many people
# don't know what industry they are in anyway.
table(nzes$dresp_industry)

## Two area lived in?
# Five nice categories here, not enough data so we'll split into one
table(nzes$dregsize)
nzes <- nzes %>%
   mutate(City = 1 * (dregsize == "A major city (over 100,000 population)"))

