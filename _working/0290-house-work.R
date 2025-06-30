library(tidyverse)
library(httr)
library(curlconverter)
library(janitor)
library(countrycode)
library(rsdmx)
library(readxl)
library(glue)
library(ggrepel)

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
  group_by(geo_area_name) |> 
  # Label the latest survey per country. Note that any modelling needs to
  # include a country random effect for the multiple observations per country:
  mutate(is_latest = ifelse(time_period == max(time_period), "Most recent", "Earlier")) |> 
  # limit to just the best age group, closest to adults, for each country/time:
  group_by(geo_area_name, time_period) |> 
  mutate(age = factor(age, levels = c("15+", "16+", "18+", "12+", "10+", "6+", "5+", "3+"))) |> 
  arrange(geo_area_name, time_period, age) |> 
  slice(1) |> 
  ungroup() |> 
  mutate(iso3_code = countrycode(geo_area_name, origin = "country.name.en", destination = "iso3c"))



# total fertility rate
if(!file.exists("wpp2024.csv")){
  download.file("https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Demographic_Indicators_Medium.csv.gz",
                destfile = "wpp2024.csv", mode = "wb")
}

wpp <- read_csv("wpp2024.csv") |> 
  clean_names() |> 
  select(iso3_code, time_period = time, tfr) |> 
  filter(!is.na(iso3_code))
 
time_chores |>
  left_join(wpp, by = c("iso3_code", "time_period")) |> 
  ggplot(aes(x = prop_male, y = tfr)) +
  geom_smooth(method = "lm", colour = "white") +
  geom_point(aes(shape = is_latest, colour = time_period), size = 2) +
  geom_path(aes(group = geo_area_name), colour = "grey50") +
  scale_shape_manual(values = c(1, 19)) +
  scale_colour_viridis_c(breaks = c(2000, 2020)) +
  scale_x_continuous(label = percent) +
  scale_y_continuous() +
  labs(x = "Proportion of domestic and care work done by males",
       y ="Total fertility rate",
       colour = "Observation date:",
       shape = "Observation type:",
       title = "Share of domestic work and fertility rate",
       subtitle = "Looking at all countries, relationship between male share of domestic and care work and fertility is actually negative")

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
  left_join(gdp, by = c("iso3_code", "time_period")) |> 
  # missing data for GDP for Cuba and Reunion
  filter(!is.na(gdprppppc)) |> 
  mutate(gdp_cut = cut(gdprppppc, breaks = quantile(gdprppppc, na.rm = TRUE),
                       include.lowest = TRUE,
                       labels = c("Lowest income", "Low income", "Medium income", "High income")))

hlc <- c("Malawi", "Kyrgyzstan", "China", "Egypt", "Brazil", "Oman", "Hungary", "Qatar", "Canada")

combined |> 
  ggplot(aes(x = prop_male, y = tfr))+
  facet_wrap(~gdp_cut) +
  geom_smooth(method = "lm", colour = "white") +
  geom_point(aes(shape = is_latest, colour = time_period), size = 2) +
  geom_path(aes(group = geo_area_name), colour = "grey50") +
  #geom_text(aes(label = geo_area_name)) +
  geom_text_repel(data = filter(combined, geo_area_name %in% hlc & is_latest == "Most recent"),
                  aes(label = glue("{geo_area_name}, {time_period}"),
                      colour = time_period)) +
  scale_shape_manual(values = c(1, 19)) +
  scale_colour_viridis_c(breaks = c(2000, 2020)) +
  scale_x_continuous(label = percent) +
  scale_y_continuous() +
  labs(x = "Proportion of domestic and care work done by males",
       y ="Total fertility rate",
       colour = "Observation date:",
       shape = "Observation type:",
       title = "Share of domestic work and fertility rate",
       subtitle = "Selected countries labelled.")



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

combined$country_fac <- as.factor(combined$geo_area_name)
model2 <- gamm(tfr ~ prop_male + s(log(gdprppppc)) + s(country_fac, bs = 're'), 
              data = combined, family = quasipoisson)
summary(model2$lme)
summary(model2$gam)
plot(model2$gam, pages = TRUE)


model3 <- gam(tfr ~ prop_male + s(log(gdprppppc)) + s(country_fac, bs = 're'), 
               data = combined, family = quasipoisson)
summary(model3)
plot(model3, pages = TRUE)


# note that if you let prop_male be non-linear you get different results from gam and gamm,
# it's basically unstable. If prop_male is linear you get similar point estimates,
# but the standard errors are different, so 'significance' can be seen as different.