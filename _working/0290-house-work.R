library(tidyverse)
library(httr)
remotes::install_github("hrbrmstr/curlconverter")
library(curlconverter)
library(janitor)
library(countrycode)
library(rsdmx)   # for reading the World Economic Outlook data
library(WDI)     # for gettting literacy data from the World Development Indicators
library(readxl)
library(glue)
library(ggrepel)
library(GGally) # for ggpairs
library(mgcv)   # for gam
library(ggdag)


conflicts_prefer(dplyr::lag)

#-----------drawing a DAG (or at least a DG)------------------

dg <- dagify(tfr ~ opp + hw ,
             ge ~ opp + advoc, 
             opp ~ ge + advoc,
             hw ~ ge,
             opp ~ gdp,
             gdp ~ opp,
             
             labels = c(
               "tfr" = "Total fertility rate",
               "hw" = "Men doing housework",
               "opp" = "Opportunities for\nwomen and girls",
               "ge" = "Culture of\ngender equality",
               "gdp" = "Economic growth",
               "advoc" = "Feminist advocacy"
             ),
             outcome = "tfr",
             latent = "ge",
             exposure = "hw"
)

set.seed(124)
d1 <- dg |> 
  #  ggdag(text = FALSE, node = FALSE)  +
  ggplot(aes(x = x, y = y, xend = xend, yend =yend)) +
  geom_dag_node(colour = "grey") +
  geom_dag_edges(edge_colour = "steelblue") +
  geom_dag_label_repel(aes(label = label), col = "peru", fill = "transparent") +
  theme_dag(base_family = "Roboto")

set.seed(123)
d2 <- dg |> 
  ggdag_paths(text = FALSE, use_labels = "label", shadow = TRUE) +
  theme_dag(base_family = "Roboto")


svg_png(d1, "../img/0290-dg", w = 9, h = 6)
svg_png(d2, "../img/0290-paths", w = 12, h = 7)

#-----------downloading some SDG time use data from the UN database-------------
# Note sure this is the best way to do this, it was clunky to work out,
# but it works. Someone should (or have they already?) build an R package.
#
# this is all httr, I understand httr2 is the  current thing now, but this still works 
request <- "curl -X POST --header 'Content-Type: application/x-www-form-urlencoded' --header 'Accept: application/octet-stream' -d 'seriesCodes=SL_DOM_TSPD' 'https://unstats.un.org/sdgapi/v1/sdg/Series/DataCSV'" |> 
  straighten() |> 
  make_req()

gender_txt <- content(request[[1]](), as = "text")


gender <- read_csv(gender_txt) |> 
  clean_names()

count(gender, sex)      # two categories, FEMALE and MALE - no TOTAL
count(gender, age)      # many different ages used for different countries
count(gender, location) # there's ALLAREA, RURAL and URBAN

# should be only one indicator:
stopifnot(length(unique(gender$series_description)) == 1)
# which is 
# Proportion of time spent on unpaid domestic chores and care work, by sex, age and location (%) 

time_chores <- gender |> 
  # we don't want rural and urban, just country total:
  filter(location == "ALLAREA") |> 
  # we want the ages like 15+, 12+ etc, not those like 15-59 with an upper bound
  filter(grepl("^[0-9]*\\+$", age)) |> 
  # but not the retirees, which some countries include. We want the 15+, not 15+
  # and 65+ separately:
  filter(!age %in% c("65+", "85+", "60+")) |> 
  # calculate the male time spent as a proportion of total (male and female) time spent
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


#--------combine time use with fertility data and simple scatter plot----------

# total fertility rate, from the UN Population Projections
if(!file.exists("wpp2024.csv")){
  download.file("https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Demographic_Indicators_Medium.csv.gz",
                destfile = "wpp2024.csv", mode = "wb")
}

wpp <- read_csv("wpp2024.csv") |> 
  clean_names() |> 
  select(iso3_code, time_period = time, tfr) |> 
  filter(!is.na(iso3_code))
 
p1 <- time_chores |>
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
       title = "Gender share of domestic work and fertility rate",
       subtitle = "Looking at all countries, relationship between male share of domestic and care work and fertility is actually negative",
       caption = "Time use data from the UN SDGs database; total fertility rate from the UN population projections. Analysis by freerangestats.info.")

#-----------------add income and do a four facet plot-----------------

# so income must be a confounder. We can use the IMF WEO data from last week
# (see that blog for how to access it, or run 0288-IMF-WEO.R script in this
# same repository)
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
  # get the latest gdp per capita for each country, for stable categorisation 
  # based on just one number (I use latest instead of earliest because if
  # we are going to use just one number, it will be the most recent):
  group_by(geo_area_name) |> 
  arrange(time_period) |> 
  mutate(latest_gdp = gdprppppc[time_period == max(time_period)]) |> 
  ungroup() |> 
  mutate(gdp_cut = cut(gdprppppc, breaks = quantile(unique(latest_gdp), na.rm = TRUE),
                       include.lowest = TRUE,
                       labels = c("Lowest income", "Low income", 
                                  "Medium income", "High income"))) |> 
  # there's an annoying problem with some countries eg China being split
  # and sometimes in "Lowest income", sometimes "low income" because they 
  # are right on the cut's quantile. So we need to fix this:
  group_by(geo_area_name) |> 
  # note that if you used ifelse instead of if_else, it loses the levels of gdp_cut.
  # but this method works:
  mutate(gdp_cut = if_else(geo_area_name != lag(geo_area_name) | is.na(lag(geo_area_name)), gdp_cut, lag(gdp_cut))) |> 
  ungroup()

# check:
select(combined, geo_area_name, time_period, gdprppppc, latest_gdp, gdp_cut)
  
# check:
filter(combined, geo_area_name == "China") |> select(gdp_cut)


# some interesting countries to highlight
hlc <- c("Malawi", "Kyrgyzstan", "China", "Egypt", 
         "Brazil", "Oman", "Hungary", "Qatar", "Canada",
         "Australia", "Switzerland")

combined |> 
  ggplot(aes(x = prop_male, y = tfr))+
  facet_wrap(~gdp_cut, scales = "free") +
  geom_smooth(method = "lm", colour = "white") +
  geom_point(aes(shape = is_latest, colour = time_period), size = 2) +
  geom_path(aes(group = geo_area_name), colour = "grey50") +
  #geom_text(aes(label = geo_area_name)) +
  geom_text_repel(data = filter(combined, geo_area_name %in% hlc & is_latest == "Most recent"),
                  aes(label = glue("{geo_area_name}, {time_period}"),
                      colour = time_period)) +
  scale_shape_manual(values = c(1, 19)) +
  scale_colour_viridis_c(breaks = c(2000, 2020), option = "A", direction = -1) +
  scale_x_continuous(label = percent) +
  scale_y_continuous() +
  labs(x = "Proportion of domestic and care work done by males",
       y ="Total fertility rate",
       colour = "Observation date:",
       shape = "Observation type:",
       title = "Share of domestic work and fertility rate",
       subtitle = "Selected countries labelled.")


#-------------female education----------------------
# higher income is only the most obvious confounder. There's also the general
# sense of female economic opportunities and empowerment. A fair proxy of this
# is probably female literacy as a proportion of male literacy

# WDIsearch("litera") |> View()
# Literacy rate, youth (ages 15-24), gender parity index (GPI)
# Literacy rate, youth female (% of females ages 15-24)
literacy <- WDI(indicator = c(literacy_parity = "SE.ADT.1524.LT.FM.ZS",
                              female_literacy = "SE.ADT.1524.LT.FE.ZS")) |> 
  as_tibble() |> 
  drop_na() |> 
  mutate(time_period = as.numeric(year)) |> 
  select(iso3_code = iso3c,
         time_period,
         literacy_parity,
         female_literacy)

combined <- combined |> 
  select(geo_area_name:gdp_cut) |> 
  left_join(literacy, by = c("iso3_code", "time_period"))

length(unique(combined$geo_area_name))
#-------------modelling--------------------
combined |> 
  mutate(lgdp = log(gdprppppc),
         ltfr = log(tfr),
         lfl  = log(female_literacy)) |> 
  select( prop_male, lgdp, ltfr, literacy_parity, female_literacy) |> 
  ggpairs()


# Here's the simplest thing we should consider. Note that this treats each
# data point as IID, even though many are repeated measures of the same country.
# So still not a great model.
model <- lm(log(tfr) ~ prop_male + log(gdprppppc) + literacy_parity + female_literacy, 
            data = combined)

# the diagnostics are ok:
par(mfrow = c(2,2))
plot(model)

# and the result is straightforward: gdp per capita predicts fertility, domestic chores doesn't:
summary(model)

# note that if we just look at the male housework we get a very strong relationship of
# time use
bad_model <- lm(log(tfr) ~ log(prop_male), data = combined)
summary(bad_model)
# we can be confident that gdp per capita should be kept in as it's clearly not
# a mediator of domestic chores (domestic chores do not act on fertility via GDP),
# nor a collider (fertility impacts on GDP and domestic chores impact on GDP)
# Instead it is a confounder - GDP impacts on both domestic chores and on fertility.

# a better model would be one that takes into account that we have repeated measures
# for each country
combined$country_fac <- as.factor(combined$geo_area_name)
model2 <- gamm(tfr ~ prop_male + s(log(gdprppppc)) + s(country_fac, bs = 're') +
                 female_literacy + literacy_parity, 
              data = combined, family = quasipoisson)
summary(model2$lme)
summary(model2$gam)
plot(model2$gam, pages = TRUE)


model3 <- gam(tfr ~ prop_male + s(log(gdprppppc)) + s(country_fac, bs = 're') +
                female_literacy + literacy_parity, 
               data = combined, family = quasipoisson)
summary(model3)
plot(model3, pages = TRUE)


# note that if you let prop_male be non-linear you get different results from gam and gamm,
# it's basically unstable. If prop_male is linear you get similar point estimates,
# but the standard errors are different, so 'significance' can be seen as different.

