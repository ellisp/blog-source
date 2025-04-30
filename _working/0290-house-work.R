library(tidyverse)
library(httr)
library(curlconverter)
library(janitor)
library(countrycode)
library(rsdmx)
library(readxl)

# this is all httr, I understand httr2 is the  current thing now, but this still works 
request <- "curl -X POST --header 'Content-Type: application/x-www-form-urlencoded' --header 'Accept: application/octet-stream' -d 'seriesCodes=SL_DOM_TSPD' 'https://unstats.un.org/sdgapi/v1/sdg/Series/DataCSV'" |> 
  straighten() |> 
  make_req()

gender_txt <- content(request[[1]](), as = "text")


gender <- read_csv(gender_txt) |> 
  clean_names()

count(gender, sex)
count(gender, age)   # many different ages used for different countries


# should be only one indicator:
stopifnot(length(unique(gender$series_description)) == 1)
# which is 
# Proportion of time spent on unpaid domestic chores and care work, by sex, age and location (%) 

time_chores <- gender |> 
  filter(location == "ALLAREA") |> 
  # we want the ages like 15+, 12+ etc, not those with an upper bound
  filter(grepl("^[0-9]*\\+$", age)) |> 
  # but not the retirees, which some countries
  filter(!age %in% c("65+", "85+", "60+")) |> 
  # preference for ages to use
  group_by(geo_area_name, time_period, age) |> 
  summarise(prop_male = value[sex == 'MALE'] / sum(value[sex == 'MALE'] + value[sex == 'FEMALE'])) |> 
  group_by(geo_area_name, time_period) |> 
  # limit to just one, latest survey per country. If you don't do this, any
  # modelling needs to include a country random effect for the multiple
  # observations per country:
  filter(time_period == max(time_period)) |> 
  # limit to just the best age group, closest to adults, for each country/time:
  mutate(age = factor(age, levels = c("15+", "16+", "18+", "12+", "10+", "6+", "5+", "3+"))) |> 
  arrange(geo_area_name, time_period, age) |> 
  slice(1) |> 
  ungroup() |> 
  mutate(iso3_code = countrycode(geo_area_name, origin = "country.name.en", destination = "iso3c"))

# some coutnries have 10+ and 15+, need to decide which one we want in that case
time_chores  
View(time_chores)


# total fertility rate
download.file("https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Demographic_Indicators_Medium.csv.gz",
              destfile = "wpp2024.csv", mode = "wb")

wpp <- read_csv("wpp2024.gz") |> 
  clean_names() |> 
  select(iso3_code, time_period = time, tfr) |> 
  filter(!is.na(iso3_code))
# 
# combined <- time_chores |> 
#   left_join(wpp, by = c("iso3_code", "time_period"))
# 
# # overall, negative relationship. Expected because income highly correlated with both
# combined |> 
#   ggplot(aes(x = prop_male, y = tfr)) +
#   geom_smooth(method = "lm") +
#   geom_point()

# so income must be a confounder. We can use the IMF WEO data from last week
# (see that blog for how to access it)
if(!exists("d2025")){
  d2025 <- readSDMX("WEOAPR2025/xmlfile_APR2025.xml", isURL = FALSE) |> 
    # this parsing takes a long time:
    as_tibble()
}

ref_areas <-   read_xlsx("WEOAPR2025/WEOPUB_DSD_APR2025.xlsx", sheet = "REF_AREA", skip = 7)[, 1:2] |> 
  rename(REF_AREA = Code,
         country = Description)

gdp <- d2025 |> 
  filter(CONCEPT == "NGDPRPPPPC") |> 
  select(time_period = TIME_PERIOD,
         gdprppppc = OBS_VALUE,
         REF_AREA) |> 
  mutate(gdprppppc = as.numeric(gdprppppc),
         time_period = as.numeric(time_period)) |> 
  filter(!is.na(gdprppppc)) |> 
  left_join(ref_areas, by = "REF_AREA") |> 
  mutate(iso3_code = countrycode(country, origin = "country.name.en", destination = "iso3c")) |> 
  filter(!is.na(iso3_code)) |> 
  select(-REF_AREA)

combined <- time_chores |> 
  left_join(wpp, by = c("iso3_code", "time_period")) |> 
  left_join(gdp, by = c("iso3_code", "time_period")) 

library(GGally)
combined |> 
  mutate(lgdp = log(gdprppppc)) |> 
  select( prop_male, lgdp, tfr) |> 
  ggpairs()



model <- lm(tfr ~ log(prop_male) + log(gdprppppc), data = combined)
summary(model)
par(mfrow = c(2,2))
plot(model)

library(mgcv)
model2 <- gam(tfr ~ log(prop_male) + s(log(gdprppppc)), data = combined)
summary(model2)
plot(model2, pages = TRUE)


# note - no need to have only one observation per country - have multiple
# observations in different years and its a more powerful model with mixed
# effects of course