
library(readr)
library(survey)
library(dplyr)

source("https://github.com/nz-mbie/mbie-r-package-public/raw/master/pkg/R/CountryManip.R") # for CountryGroup
source("https://github.com/nz-mbie/mbie-r-package-public/raw/master/pkg/R/NZTourism.R")    # for rename.levels

# download survey data from the MBIE (Ministry of Business, Innovation and Employment) website
download.file("http://www.mbie.govt.nz/info-services/sectors-industries/tourism/tourism-research-data/ivs/documents-image-library/vw_IVS.zip",
              mode = "wb", destfile = "vw_IVS.zip")

unzip("vw_IVS.zip")

ivs <- read_csv("IVS/vw_IVSSurveyMainHeader.csv")

ivs_sub <- ivs %>%
  mutate(Country = CountryGroup(CORNextYr)) %>%
  select(Country, Year, PopulationWeight, WeightedSpend, POV, Gender, AgeRange, SurveyResponseID) %>%
  rename(Spend = WeightedSpend) %>%
  as_tibble() %>%
  arrange(Year)

write_delim(ivs_sub, path = "ivs-1997-to-2017.txt", delim = "|")

# check the weighted per person spend in the Power BI page: 
ivs_svy <- svydesign(~1, weights = ~PopulationWeight, data = ivs_sub)
mean_spend <- svyby(~Spend, ~Year, design = ivs_svy, FUN = svymean)
tail(round(mean_spend))


ivs_sub %>%
  filter(Year > 2010) %>%
  group_by(Year) %>%
  summarise(total_spend = sum(Spend * PopulationWeight),
            people = sum(PopulationWeight),
            mean_spend = total_spend / people) %>%
  arrange(desc(Year))


sql <-
"SELECT
  SUM(Spend * PopulationWeight)                         AS total_spend,
  SUM(PopulationWeight)                                 AS people,
  SUM(Spend * PopulationWeight) / SUM(PopulationWeight) AS mean_spend,
  Year
FROM ivs_sub
WHERE Year > 2010
GROUP BY Year
ORDER BY Year DESC"

sqldf::sqldf(sql)
