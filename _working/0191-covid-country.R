library(tidyverse)
library(rvest)
library(janitor)
library(GGally)
library(countrycode)
library(WDI)
library(ggrepel)
library(scales)
library(RColorBrewer)
library(glmnet)
library(boot)

#---------------download file of original article and save it------------
url <- "https://hcqtrial.com/"
df <- "hcq-country-study.html"
if(!df %in% list.files()){
  download.file(url, df)
}


#--------extract tables from the Appendix----------
hcq_html <- read_html(df)

tabs <- hcq_html %>%
  html_nodes("table")

# the tables are missing their <tr> table row tags. Borrowed an approach from this SO question:
# https://stackoverflow.com/questions/30989543/r-scraping-an-html-table-with-rvest-when-there-are-missing-tr-tags

#' Read a table missing <tr> tags and convert to a tibble
read_bad_table <- function(the_tab, ncol){
  
  # get the individual elements of the table and pour into a matrix
  m <- the_tab %>%
    html_nodes("td") %>%
    html_text() %>%
    matrix(ncol = ncol, byrow = TRUE)
  
  # convert to a data frame and then to a tibble
  d <- as.data.frame(m[-1, ], stringsAsFactors = FALSE)
  names(d) <- make_clean_names(m[1, ])
  d <- as_tibble(d)
  return(d)
}

# Make a list of all those tables:
all_tables <- list()
table_cols <- c(11, 7, 6, 4, 3) # how many columsn in each table
for(i in 1:5){
  all_tables[[i]] <- read_bad_table(tabs[[i + 2]], ncol = table_cols[i])
}

# Create a single big wide data frame. Start with the first table:
combined <- all_tables[[1]] 
  
# Then add the other 3 tables:
for(i in 2:4){
  combined <- full_join(combined, all_tables[[i]], by = "country")
}

#' Turn a column with mixed formats into numbers
tweak <- function(x){
  x <- gsub("%", "", x, fixed = TRUE)
  x <- ifelse(x == "N/A", NA, x)
  y <- grepl("[Mm]", x)
  z <- grepl("[Bb]", x)
  x <- gsub("[MmBb]", "", x)
  x <- as.numeric(x)
  x <- ifelse(y, x * 1e6, x)
  x <- ifelse(z, x * 1e9, x)
  return(x)
}

# Apply that function to all the columns that should be numbers.
for(j in c(2:25)){
  combined[, j] <- tweak(pull(combined, j))
}

hcq_use <- tribble(~country, ~hcq_usage,
  "Algeria", "Widespread",
  "Belarus", "Mixed",
  "Brazil", "Mixed",
  "Canada", "Limited",
  "Chile", "Mixed",
  "Costa Rica", "Widespread",
  "Cuba", "Widespread",
  "Czech Republic", "Widespread",
  "France", "Limited",
  "Germany", "Mixed",
  "Greece", "Widespread",
  "India", "Widespread",
  "Indonesia", "Widespread",
  "Ireland", "Limited",
  "Israel", "Widespread",
  "Italy", "Limited",
  "Kazakhstan", "Widespread",
  "Malaysia", "Mixed",
  "Mexico", "Limited",
  "Morocco", "Widespread",
  "Netherlands", "Limited",
  "Pakistan", "Mixed",
  "Portugal", "Mixed",
  "Russia", "Widespread",
  "Senegal","Widespread",
  "South Korea", "Widespread",
  "Sweden", "Limited",
  "Switzerland", "Mixed",
  "Tunisia", "Mixed",
  "Turkey", "Widespread",
  "Ukraine", "Widespread",
  "U.K.", "Limited",
  "USA", "Limited"
)

# 14 countries with HCQ usage data but not in the other tables
hcq_use %>% anti_join(combined, by = "country") 

# Of these named exclusions are:
# Senegal too small
# Czech Republic, Malaysia too few people over 80
# South Korea, Czech Republic used masks early

# only unexplained exclusion is Kazakhstan

stopifnot(
  combined %>%
    select(country) %>%
    anti_join(filter(hcq_use, hcq_usage != "Mixed"), by = "country") == 0
)

#------------Data from other sources-------------

# GDP and population data from the world bank
gdp_pc <- WDI(indicator = c(gdp_per_capita = "NY.GDP.PCAP.KD", pop_wdi = "SP.POP.TOTL"))

# This gets us 2018 (Cuba) or 2019 (everyone else) GDP per capita
latest_gdp <- gdp_pc %>%
  filter(!is.na(gdp_per_capita)) %>%
  group_by(iso2c) %>%
  filter(year == max(year)) %>%
  select(iso2c, gdp_per_capita) %>%
  ungroup()

latest_pop <- gdp_pc %>%
  filter(!is.na(pop_wdi)) %>%
  group_by(iso2c) %>%
  filter(year == max(year)) %>%
  select(iso2c, pop_wdi) %>%
  ungroup()

# Covid data from Our World In Data:
download.file("https://covid.ourworldindata.org/data/ecdc/full_data.csv",
              destfile = "covid_full_data.csv")

owid <- read_csv("covid_full_data.csv", col_types = cols()) %>%
  mutate(iso2c = countrycode(location, origin = "country.name", destination = "iso2c")) %>%
  inner_join(latest_pop, by = "iso2c") %>%
  mutate(deaths_per_m = total_deaths / pop_wdi * 1e6,
         deaths_per_c = total_deaths / total_cases) %>%
  group_by(iso2c) %>%
  summarise(date_first_death_per_m = min(date[deaths_per_m > 1], na.rm = TRUE),
            deaths_per_m = deaths_per_m[date == "2020-08-07"],
            total_deaths = total_deaths[date == "2020-08-07"],
            pop_wdi = unique(pop_wdi),
            deaths_per_c = deaths_per_c[date == "2020-08-07"],
            total_cases = total_cases[date == "2020-08-07"]
            ) %>%
  select(iso2c, date_first_death_per_m, total_deaths, deaths_per_m, pop_wdi, deaths_per_c, total_cases)  

# check all of our data is in
# stopifnot(length(combined$iso2c[!combined$iso2c %in% owid$iso2c]) == 0)



all_data <- full_join(combined, hcq_use, by = "country") %>%
  # add a country code for use later in various joins to other data:
  mutate(iso2c = countrycode(country, origin = "country.name", destination = "iso2c")) %>%
  rename(urban_percentage = urban_percentage_urban_percentage,
         population_density = population_density_pop_density,
         population = population_pop) %>%
  full_join(owid, by = "iso2c") %>%
  left_join(latest_gdp, by ="iso2c") %>%
  mutate(hcq_usage = replace_na(hcq_usage, "Unknown")) %>%
  mutate(adjusted_deaths_per_m = deaths_per_m * 
           age_factor * diabetes_factor * obesity_factor * hypertension_factor * gender_factor,
         days_since_bad = as.numeric(as.Date("2020-08-07") - date_first_death_per_m),
         days_since_bad = ifelse(days_since_bad < 0, 0, days_since_bad)) %>%
  # swap order so 'widespread' appears on the left side of legends, better visually:
  mutate(hcq_usage = fct_rev(hcq_usage))
  

#---------Exploratory charts-----------------

# Not shown in blog
# all_data %>%
#   select(hcq_usage, urban_percentage:population_density, life_expectancy, tests_per_thousand, 
#          gdp_per_capita, days_since_bad, adjusted_deaths_per_m) %>%
#   ggpairs()


# Colour palette:
hcq_cols <- brewer.pal(4, "Set1")
names(hcq_cols) <- unique(all_data$hcq_usage)

the_caption <- str_wrap(
  "Source: HCQ policy on usage, and death adjustment factors by diabetes, hypertension, gender and age, 
  from an anonymous article on the web, 'Early treatment with hydroxychloroquine'; 
  Covid-19 death numbers from Our World in Data;  population and GDP
  from the World Bank World Development Indicators. Analysis by http://freerangestats.info.",
  120
)

# Chart of adjusted deaths per million. Only available for countries used in their original analysis:
p1 <- all_data %>%
  filter(!is.na(adjusted_deaths_per_m)) %>%
  ggplot(aes(x = gdp_per_capita, y = adjusted_deaths_per_m, colour = hcq_usage)) +
  geom_smooth(method = "gam", aes(colour = NULL), colour = NA) +
  geom_point() +
  geom_text_repel(aes(label = country)) +
  scale_x_log10(label = dollar_format(accuracy = 1)) +
  scale_y_log10(label = comma) +
  labs(x = "GDP per capita",
       y = "Adjusted Covid-19 deaths per million",
       color = "Reported HCQ usage in early stages of Covid-19:",
       caption = the_caption,
       title = "GDP per capita, reported HCQ use and adjusted Covid-19 death rates",
       subtitle = str_wrap("Of the 19 countries in this HCQ cross-country observational study, 
       nearly all countries described as limited users of HCQ are wealthy. Higher income countries
                           (for whatever reason) also have higher death rates reported, which will 
                           confound any HCQ effect.", 100)) +
  theme(plot.caption = element_text(hjust = 0)) +
  scale_colour_manual(values = hcq_cols)

# Chart with unadjusted deaths per million, available for more 
p2 <- ggplot(all_data, aes(x = gdp_per_capita, y = deaths_per_m, colour = hcq_usage)) +
  geom_point() +
  geom_text_repel(aes(label = country)) +
  scale_x_log10(label = dollar_format(accuracy = 1)) +
  scale_y_log10(label = comma) +
  labs(x = "GDP per capita",
        y = "Un-adjusted Covid-19 deaths per million",
       color = "Reported HCQ usage in early stages of Covid-19:",
       caption = the_caption,
       title = "GDP per capita, HCQ use and Covid-19 death rates",
       subtitle = str_wrap("14 countries were excluded from this HCQ cross-country observational study, 
       because their HCQ usage is 'mixed'. The conclusions would not have changed if they
                           had been left in as an intermediary level of the intervention, but 
                           the HCQ effect would go away if some key categorisations had been different.", 100)) +
  theme(plot.caption = element_text(hjust = 0)) +
  scale_colour_manual(values = hcq_cols)

p3 <- all_data %>%
  filter(!is.na(adjusted_deaths_per_m)) %>%
  ggplot(aes(x = days_since_bad, y = deaths_per_m, colour = hcq_usage)) +
  geom_smooth(method = "gam", aes(colour = NULL), colour = NA) +
  geom_point() +
  geom_text_repel(aes(label = country)) +
  scale_x_continuous(label = comma) +
  scale_y_log10(label = comma) +
  labs(x = "Days since deaths per million passed 1",
       y = "Un-adjusted Covid-19 deaths per million",
       color = "Reported HCQ usage in early stages of Covid-19:",
       caption = the_caption,
  title = "Time since the outbreak became very serious, reported HCQ use and Covid-19 death rates",
  subtitle = str_wrap("Countries described as using HCQ in limited fashion also tend to have
                      had more time pass since the outbreak became serious. That length of time 
                      is correlated with total deaths per million.", 100)) +
  theme(plot.caption = element_text(hjust = 0)) +
  scale_colour_manual(values = hcq_cols)

p4 <- all_data %>%
  ggplot(aes(x = deaths_per_c, y = deaths_per_m, label = country, colour = hcq_usage)) +
  geom_smooth(method = "gam", aes(colour = NULL), colour = NA) +
  geom_point() +
  geom_text_repel() +
  scale_colour_manual(values = hcq_cols) +
  scale_x_log10() +
  scale_y_log10(label = comma) +
  labs(x = "Cumulative deaths per case",
       y = "Cumulative deaths per million population",
       title = "Relationship between deaths per case and deaths per population is straightforward if noisy",
       subtitle = "As at 7 August 2020. Labelled countries are those with data in the original HCQ website.",
       color = "Reported HCQ usage in early stages of Covid-19:",
       caption = "Source: Our World in Data (Covid) and World Bank WDI (population). HCQ categorisation from an anonymous article on the web.")

p5 <- all_data %>%
  filter(hcq_usage %in% c("Widespread", "Limited")) %>%
  ggplot(aes(x = gdp_per_capita, y = deaths_per_c, colour = hcq_usage)) +
  geom_smooth(method = "gam", aes(colour = NULL), colour = NA) +
  geom_point() +
  geom_text_repel(aes(label = country)) +
  scale_x_log10(label = dollar_format(accuracy = 1)) +
  scale_y_log10(label = comma) +
  labs(x = "GDP per capita",
       y = "Covid-19 deaths per case",
       color = "Reported HCQ usage in early stages of Covid-19:",
       caption = the_caption,
       title = "GDP per capita, reported HCQ use and Covid-19 case fatality rates",
       subtitle = str_wrap("Of the 19 countries in this HCQ cross-country observational study, 
       nearly all countries described as limited users of HCQ are wealthy. Higher income countries
                           (for whatever reason) also have higher death rates reported, which will 
                           confound any HCQ effect. The relationship of HCQ usage to deaths per
                           case is less than that to deaths per population, but still present.", 100)) +
  theme(plot.caption = element_text(hjust = 0)) +
  scale_colour_manual(values = hcq_cols)

#-------------"straightforward" modelling with pre-lasso methods-----
# These methods are unlikely to be good because of the small sample size
# see further down the script for a more appropriate method

sub_data <- filter(all_data, hcq_usage %in% c("Limited", "Widespread"))

# model with all 32 observations with HCQ, deaths and population:
mod0 <- glm(cbind(total_deaths, pop_wdi) ~ relevel(hcq_usage, "Limited"), 
            family = quasibinomial(link = log), data = all_data)
exp(confint(mod0)[, ]) # having checked that "Widespread" is the second row in confint(mod0)

# same model but with only 19 observations as per main study:
mod1 <- glm(cbind(total_deaths, pop_wdi) ~ relevel(hcq_usage, "Limited"), 
            family = quasibinomial(link = log), data = sub_data)
exp(coef(mod1))
exp(confint(mod1)[2, ])

mod2 <- glm(cbind(total_deaths, population) ~ relevel(hcq_usage, "Limited") + 
              log(gdp_per_capita) + days_since_bad + 
              obesity_prevalence + diabetes_prevalence + 
              hypertension_prevalence + age_factor + urban_percentage + tests_per_thousand +
              average_intervention_stringency_index + population_density
              , family = quasibinomial(link = log), data = sub_data)
exp(confint(mod2)[2, ])

mod3 <- glm(deaths_per_m ~ relevel(hcq_usage, "Limited") + 
              log(gdp_per_capita) + days_since_bad + 
              obesity_prevalence + diabetes_prevalence + 
              hypertension_prevalence + age_factor + urban_percentage + tests_per_thousand +
              average_intervention_stringency_index + population_density
            , family = quasipoisson, data = sub_data)

mod3b <- glm(adjusted_deaths_per_m ~ relevel(hcq_usage, "Limited") + 
              log(gdp_per_capita) + days_since_bad + 
              obesity_prevalence + diabetes_prevalence + 
              hypertension_prevalence + age_factor + urban_percentage + tests_per_thousand +
              average_intervention_stringency_index + population_density
            , family = quasipoisson, data = sub_data)
cbind(exp(coef(mod3b)))
exp(confint(mod3b)[2, ])


mod4 <- glm(deaths_per_m ~ relevel(hcq_usage, "Limited") + 
              log(gdp_per_capita) + days_since_bad
            , family = quasipoisson, data = all_data)
exp(confint(mod4)[2, ])

# this the model I think is being used by the original authors:
mod5 <- glm(adjusted_deaths_per_m ~ relevel(hcq_usage, "Limited") + 
              urban_percentage + tests_per_thousand +
              average_intervention_stringency_index + population_density
            , family = quasipoisson, data = sub_data)
cbind(exp(coef(mod5)))
exp(confint(mod5)[2, ])

mod6 <- glm(deaths_per_m ~ relevel(hcq_usage, "Limited") + 
              obesity_prevalence + diabetes_prevalence + 
              hypertension_prevalence + age_factor + urban_percentage + tests_per_thousand +
              average_intervention_stringency_index + population_density
            , family = quasipoisson, data = sub_data, weights = pop_wdi)
exp(confint(mod6)[2, ])


mod7 <- glm(deaths_per_c ~ relevel(hcq_usage, "Limited") + 
              obesity_prevalence + diabetes_prevalence + 
              hypertension_prevalence + age_factor + urban_percentage + tests_per_thousand +
              average_intervention_stringency_index + population_density
            , family = quasipoisson, data = sub_data, weights = pop_wdi)
cbind(exp(coef(mod7)))
exp(confint(mod7)[2, ])

#===========bootstrapped lasso=============
# with just the 19 observations used in the original
d1 <- all_data %>%
  filter(hcq_usage %in% c("Widespread", "Limited") & !is.na(urban_percentage)) %>%
  mutate(widespread_hcq = as.numeric(hcq_usage == "Widespread"),
         log_deaths_per_m = log(deaths_per_m),
         log_gdp_per_capita = log(gdp_per_capita)) %>%
  select(response_column = log_deaths_per_m, 
         weight_column = pop_wdi,
         days_since_bad, 
         urban_percentage:hypertension_prevalence, 
         age_factor,
         log_gdp_per_capita, 
         widespread_hcq) 


# function for fitting a glmnet lasso model to data[w, ] - to feed to boot()
boot_glmnet <- function(data, w, return_as_vector = TRUE){
  
  stopifnot(names(data)[1] == "response_column")
  stopifnot(names(data)[2] == "weight_column")
  
  the_data <- data[w, ]
  
  x <- as.matrix(the_data[ , -(1:2)]) # all the data except the first two columns
  y <- pull(the_data, response_column)
  weights <- scale(pull(the_data, weight_column), center = FALSE)

  # use cross validation to try various values of lambda.
  # The coef(mod3) will return the coefficients from the best lambda.
  mod3 <- cv.glmnet(x, y, alpha = 1, family = "gaussian", weights = weights)
  
  if(return_as_vector) {
    output <- as.numeric(coef(mod3))
  } else {
    output <- coef(mod3)
  }
  return(output)
}

set.seed(123)
boot_glmnet(d1, 1:nrow(d1), FALSE)

booted <- boot(d, boot_glmnet, R = 999)

mean(booted$t[, 14] < 0 ) # 95% of time HCQ coefficient is less than zero
mean(booted$t[, 13] > 0 ) # 63% of time GDP coefficient is more than zero
mean(booted$t[, 12] < 0 ) # 24% of time age factor coefficient is less than zero

# Confidence interval for relative risk of HCQ
boot.ci(booted, type = "perc", index = 14)
exp(boot.ci(booted, type = "perc", index = 14)$percent[4:5]) # relative risk ratio 0.08 to 1

#-----------bootstrap lasso with 33 observations-----
# this is a bit less than ideal because I am not forcing 'widespread' and 'mixed' to both
# be in the model together. Also I don't have data on the hypertension, population density
# etc as per the previous models. However, these factors were very rarely in the model
# anyway
d2 <- all_data %>%
  filter(hcq_usage %in% c("Widespread", "Mixed", "Limited")) %>%
  mutate(widespread_hcq = as.numeric(hcq_usage == "Widespread"),
         mixed_hcq = as.numeric(hcq_usage == "Mixed"),
         log_deaths_per_m = log(deaths_per_m),
         log_gdp_per_capita = log(gdp_per_capita)) %>%
  select(response_column = log_deaths_per_m, 
         weight_column = pop_wdi,
         days_since_bad, 
         log_gdp_per_capita, 
         mixed_hcq,
         widespread_hcq) 

boot_glmnet(d2, 1:nrow(d2), FALSE)


booted2 <- boot(d2, boot_glmnet, R = 999)
mean(booted2$t[, 5] < 0 ) # 91% of time 'widespread' HCQ coefficient is less than zero
mean(booted2$t[, 3] > 0 ) # 76% of time GDP coefficient is more than zero
mean(booted2$t[, 4] < 0 ) # 29% of time 'mixed' HCQ coefficient is less than zero
boot.ci(booted2, type = "perc", index = 5)
exp(boot.ci(booted2, type = "perc", index = 5)$percent[4:5]) # relative risk ratio 0.09 to 1.00

#--------------bootstrap lasso with 176 observations------------
# using deaths per *case* as response variable
d3 <- all_data %>%
  filter(hcq_usage %in% c("Widespread", "Mixed", "Limited", "Unknown")) %>%
  filter(!is.na(gdp_per_capita) & deaths_per_c > 0) %>%
  mutate(widespread_hcq = as.numeric(hcq_usage == "Widespread"),
         mixed_hcq = as.numeric(hcq_usage == "Mixed"),
         unknown_hcq = as.numeric(hcq_usage == "Unknown"),
         log_deaths_per_c = log(deaths_per_c),
         log_gdp_per_capita = log(gdp_per_capita)) %>%
  select(response_column = log_deaths_per_c, 
         weight_column = total_cases,
         days_since_bad, 
         log_gdp_per_capita, 
         unknown_hcq,
         mixed_hcq,
         widespread_hcq) 

p6 <- function(){
  d3 %>% 
    rename(log_deaths_per_cases = response_column,
           `cases (weights)` = weight_column) %>%
    rename_all(function(x){str_wrap(gsub("_", " ", x), 12)}) %>%
    ggpairs() %>%
    print()
}

boot_glmnet(d3, 1:nrow(d3), FALSE)


booted3 <- boot(d3, boot_glmnet, R = 999)
mean(booted3$t[, 6] < 0 ) # 48% of time 'widespread' HCQ coefficient is less than zero
mean(booted3$t[, 6] > 0 ) # 0% of time 'widespread' HCQ coefficient is greater than zero
mean(booted3$t[, 2] > 0 ) # 73% of time 'days since bad' coefficient is more than zero
mean(booted3$t[, 3] < 0 ) # 40% of time GDP per capita coefficient is less than zero
mean(booted3$t[, 4] < 0 ) # 53% of time 'unknown'' coefficient is less than zero
mean(booted3$t[, 5] < 0 ) # 38% of time 'mixed' HCQ coefficient is less than zero
boot.ci(booted3, type = "perc", index = 6)
exp(boot.ci(booted3, type = "perc", index = 6)$percent[4:5])

#-------------------saving plots-----------

svg_png(p1, "../img/0191-gdp-adjusted", w = 9, h = 7)
svg_png(p2, "../img/0191-gdp-more-countries", w = 9, h = 7)
svg_png(p3, "../img/0191-time-since-serious", w = 9, h = 7)

svg_png(p4, "../img/0191-per-case-per-pop", w = 9, h = 7)
svg_png(p5, "../img/0191-gdp-per-case", w = 9, h = 7)
svg_png(p6, "../img/0191-pairs", w = 12, h = 11)
