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
library(mgcv)   # for gam
library(ggdag)
library(MASS)   # for rlm
library(RColorBrewer)
library(GGally)
library(lme4)
library(patchwork)
library(marginaleffects)

conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::select)

#-----------drawing a DAG (or at least a DG)------------------

# Full version with all the variables
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
) |> 
  # explicitly call this usually hidden function so we can colour the edges:
  ggdag::tidy_dagitty(seed = 124) |> 
  # colour the edges. Need to specify identity of colour here, not use scale_
  mutate(edge_type = ifelse(to == "tfr" & name == "opp", "darkred", "steelblue"))

lab_col <- "black"

# Draw the full chart
set.seed(124)
d1 <- dg |> 
  ggplot(aes(x = x, y = y, xend = xend, yend =yend)) +
  geom_dag_node(colour = "grey") +
  geom_dag_text_repel(aes(label = label), col = lab_col) +
  geom_dag_edges(aes(edge_colour = edge_type),
                 arrow_directed = grid::arrow(length = unit(12, "pt"), type = "closed")) +
  theme_dag(base_family = "Roboto")

svg_png(d1, "../img/0290-dg", w = 9, h = 6)

# Simplified DAG, just with 3 nodes
dg2 <- dagify(tfr ~ opp + hw ,
             hw ~ opp,

             labels = c(
               "tfr" = "Total fertility rate",
               "hw" = "Men doing housework",
               "opp" = "Opportunities for\nwomen and girls"
             ),
             outcome = "tfr",
             exposure = "hw"
)  |> 
  # explicitly call this usually hidden function so we can colour the edges:
  ggdag::tidy_dagitty(seed = 124) |> 
  # colour the edges. Need to specify identity of colour here, not use scale_
  mutate(edge_type = ifelse(to == "tfr" & name == "opp", "darkred", "steelblue"))


# Draw the simplified causal graph
set.seed(124)
d2 <- dg2 |> 
  ggplot(aes(x = x, y = y, xend = xend, yend =yend)) +
  geom_dag_node(colour = "grey") +
  geom_dag_edges(aes(edge_colour = edge_type), 
                 arrow_directed = grid::arrow(length = unit(12, "pt"), type = "closed")) +
  geom_dag_text_repel(aes(label = label), col = lab_col) +
  theme_dag(base_family = "Roboto")
svg_png(d2, "../img/0290-dg-simplified", w = 9, h = 6)


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


#--------combine time use with fertility data --------------------------

# total fertility rate, from the UN Population Projections
if(!file.exists("wpp2024.csv")){
  download.file("https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Demographic_Indicators_Medium.csv.gz",
                destfile = "wpp2024.csv", mode = "wb")
}

wpp <- read_csv("wpp2024.csv") |> 
  clean_names() |> 
  select(iso3_code, time_period = time, tfr) |> 
  filter(!is.na(iso3_code))
 
#-----------------add income and divide countries into groups-----------------
p1 <- time_chores |>
  left_join(wpp, by = c("iso3_code", "time_period")) |> 
  ggplot(aes(x = prop_male, y = tfr)) +
  geom_smooth(method = "lm", colour = "white") +
  geom_point(aes(shape = is_latest, colour = time_period), size = 2) +
  geom_path(aes(group = geo_area_name), colour = "grey75") +
  scale_shape_manual(values = c(1, 19)) +
  scale_x_continuous(label = percent) +
  scale_y_continuous() +
  guides(colour =  guide_colorbar(display = "rectangles")) +
  theme(legend.key.width = unit(10, "mm")) +
  labs(x = "Proportion of domestic and care work done by males",
       y ="Total fertility rate",
       colour = "Observation date:",
       shape = "Observation type:",
       title = "Gender share of domestic work and fertility rate",
       subtitle = "When including all countries, relationship between male share of domestic and care work and fertility is negative.
Confounding effect of economic and educational opportunities for women and girls has not been controlled for.",
       caption = "Time use data from the UN SDGs database; total fertility rate from the UN World Population Prospects. Analysis by freerangestats.info.")

svg_png(p1, "../img/0290-simple-scatter", w = 9, h = 7)


# so income is almost certainly a confounder. We can use the IMF WEO data from last week
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

time_fert_gdp <- time_chores |> 
  select(-geo_area_name) |> 
  full_join(wpp, by = c("iso3_code", "time_period")) |> 
  full_join(gdp, by = c("iso3_code", "time_period")) |> 
  # missing data for GDP for Cuba and Reunion
  # filter(!is.na(gdprppppc)) |>
  complete(iso3_code, time_period, fill = list(gdprpppc = NA)) |> 
  group_by(iso3_code) |> 
  mutate(latest_gdp = gdprppppc[time_period == max(time_period)],
         gdp_2000 = unique(gdprppppc[time_period == 2000]),
         gdp_min = min(gdprppppc, na.rm = TRUE),
         gdp_for_cut = ifelse(is.na(gdp_2000), gdp_min, gdp_2000)) |> 
  ungroup() |> 
  mutate(gdp_cut = cut(gdp_for_cut, breaks = quantile(unique(gdp_for_cut), na.rm = TRUE),
                       include.lowest = TRUE,
                       labels = c("Lowest income", "Low income", 
                                  "Medium income", "High income")))

dim(time_fert_gdp)

#-------------female empowerment?----------------------
# higher income is only the most obvious confounder. There's also the general
# sense of female economic opportunities and empowerment. A fair proxy of this
# is probably female literacy as a proportion of male literacy

# these didn't have enough data for each year
# WDIsearch("litera") |> View()
# Literacy rate, youth (ages 15-24), gender parity index (GPI)
# Literacy rate, youth female (% of females ages 15-24)

# this gender inequality index (GII), composite index from UNDP taking into
# account info on reproducive health, empowerment and the labour market. see
# https://hdr.undp.org/data-center/thematic-composite-indices/gender-inequality-index#/indicies/GII

# You can download all the HDR components (including GII):
download.file("https://hdr.undp.org/sites/default/files/2025_HDR/HDR25_Composite_indices_complete_time_series.csv",
              destfile = "hdr25.csv")

hdr <- read_csv("hdr25.csv")

gii <- hdr |> 
  select(iso3_code = iso3, contains("gii")) |> 
  select(-contains("rank")) |> 
  gather(variable, gii, -iso3_code) |> 
  drop_na() |> 
  separate(variable, sep = "_", into = c("variable", "time_period")) |> 
  select(-variable) |> 
  mutate(time_period = as.numeric(time_period))

combined <- time_fert_gdp |> 
  left_join(gii, by = c("iso3_code", "time_period"))  |> 
  # factor version of country, needed for some later modelling:
  mutate(country_fac = fct_reorder(country, -gdp_for_cut))

# 193 countries:
length(unique(combined$country))

# Only 79 have all the data we need
combined |> 
  filter(!is.na(gii) & !is.na(gdprppppc) & !is.na(prop_male)) |> 
  distinct(country) |> 
  nrow()

# Used this to work out Oman is missing GII data in 2000 but it is ok in 2008
# so only loses one row
filter(combined, country == "Oman" &!is.na(prop_male)) |> t()

# Only 5 rows of data lost altogether from GII:
combined |> 
  filter(!is.na(gdprppppc) & !is.na(prop_male)) |> 
  filter(is.na(gii)) |> 
  nrow()

# and only one (Cuba) from having no gdp per capita in a year we otherwise
# could include it:
combined |> 
  filter(!is.na(prop_male) & !is.na(gii)) |> 
  filter(is.na(gdprppppc)) |> 
  nrow()

# 172 observations altogether (so will go down to 172 in modelling)
combined |> 
  filter(!is.na(gii) & !is.na(gdprppppc) & !is.na(prop_male)) |> 
  nrow()

#================Exploratory charts==============

# countries that have at least one observation of all four variables:
countries_ok_data <- combined |> 
  filter(!is.na(gii) & !is.na(tfr) & !is.na(gdprppppc) & !is.na(prop_male)) |> 
  distinct(country)

#-----------------some unidimensional ones---------
#
p2 <- combined |> 
  inner_join(countries_ok_data, by = "country") |> 
  filter(!is.na(prop_male)) |> 
  group_by(country_fac) |> 
  arrange(desc(time_period)) |> 
  slice(1) |> 
  ungroup() |> 
  mutate(country_fac = fct_reorder(country_fac, -prop_male, .fun = max)) |> 
  ggplot(aes(y = country_fac, x = prop_male, fill = as.ordered(time_period))) +
  geom_col(position = "dodge", width = 0.7) +
  theme(panel.grid.major.y = element_blank()) +
  facet_wrap(~gdp_cut, ncol = 2, scales = "free_y") +
  scale_x_continuous(label = percent, expand = c(0, 0)) +
  guides(fill = guide_legend(ncol = 15)) +
  labs(x = "Male share of domestic and care work as a proportion of total - most recent survey result",
       y = "",
       fill = "Year",
       title = "Male time on domestic and care work",
       subtitle = "Countries' income categorised by purchasing power parity GDP per capita in 2000. Ordering within facet is by male time on domestic work.",
       caption = "Source: UN Sustainable Development Goals database for time use; IMF World Economic Outlook for PPP GDP per capita")

svg_png(p2, "../img/0290-time-share-bar", w = 13, h = 7)


# A couple of things we'll use a few times in the next few lollipops
st <- "Selected countries where time use, GDP and gender inequality data is also available"
lol_col <- "steelblue"

# Fertility rate
p3 <- combined |> 
  inner_join(countries_ok_data, by = "country") |> 
  filter(time_period == 2025) |> 
  mutate(country_fac = fct_reorder(country_fac, tfr)) |> 
  ggplot(aes(y = country_fac, x = tfr)) +
  geom_segment(aes(xend =0, yend = country_fac), col = lol_col) +
  geom_point(col = lol_col) +
  theme(panel.grid.major.y = element_blank()) +
  scale_x_continuous(expand = c(0,0)) +
  facet_wrap(~gdp_cut, ncol = 2, scales = "free_y") +
  labs(x = "Total fertility rate (average number of children for a woman experiencing current age-specific fertility rates through a hypothetical lifetime)",
       y = "",
       fill = "Year",
       title = "Total fertility rate in 2025",
       subtitle = st,
       caption = "Source: UN Population Prospects for total fertility rate; IMF World Economic Outlookfor PPP GDP per capita")

svg_png(p3, "../img/0290-tfr-lollipop", w = 13, h = 7)

# Gender inequality
p4 <- combined |> 
  inner_join(countries_ok_data, by = "country") |> 
  filter(time_period == 2023) |> 
  mutate(country_fac = fct_reorder(country_fac, gii)) |> 
  ggplot(aes(y = country_fac, x = gii)) +
  geom_segment(aes(xend =0, yend = country_fac), col = lol_col) +
  geom_point(col = lol_col) +
  theme(panel.grid.major.y = element_blank()) +
  facet_wrap(~gdp_cut, ncol = 2, scales = "free_y") +
  scale_x_continuous(label = comma, expand = c(0,0)) +
  labs(x = "Gender Inequality Index",
       y = "",
       fill = "Year",
       title = "Gender Inequality Index",
       subtitle = st)

svg_png(p4, "../img/0290-gii-lollipop", w = 13, h = 7)


# some interesting countries to highlight
hlc <- c("Malawi", "Kyrgyzstan", "China", "Egypt", 
         "Brazil", "Oman", "Qatar", "Canada",
         "Korea", "Lao P.D.R.", "Pakistan", "Estonia")

p5 <- combined |> 
  inner_join(countries_ok_data, by = "country") |> 
  filter(!is.na(time_period) & !is.na(prop_male)) |> 
  ggplot(aes(x = prop_male, y = tfr)) +
  facet_wrap(~gdp_cut, scales = "fixed") +
  geom_smooth(method = "rlm", colour = "white") +
  geom_point(aes(shape = is_latest, colour = time_period), size = 2) +
  geom_path(aes(group = country), colour = "grey50") +
  geom_text_repel(data = filter(combined, country %in% hlc & is_latest == "Most recent"),
                  aes(label = glue("{country}, {time_period}")), colour = "black",
                  seed = 123, size = 2.8) +
#  geom_text(aes(label = country)) +
  scale_shape_manual(values = c(1, 19)) +
  scale_x_continuous(label = percent) +
  scale_y_log10() +
  guides(colour =  guide_colorbar(display = "rectangles")) +
  theme(legend.key.width = unit(10, "mm")) +
  labs(x = "Proportion of adult domestic and care work done by men",
       y ="Total fertility rate",
       colour = "Observation date:",
       shape = "Observation type:",
       title = "Share of domestic work and fertility rate, for countries in different income categories",
       subtitle = "Countries classified into custom groups based on purchasing power parity GDP at the time of first time-use survey. Selected countries labelled.",
       caption = "Time use data from the UN SDGs database; total fertility rate from the UN World Population Prospects; GDP from the IMF World Economic Outlook. Analysis by freerangestats.info.")

svg_png(p5, "../img/0290-facet-scatter", w = 11, h = 7)

model_ready <- combined |> 
  mutate(lgdp = log(gdprppppc),
         ltfr = log(tfr)) |> 
  select( prop_male, lgdp, ltfr, gii, gdprppppc, country_fac, tfr) |> 
  drop_na() 


p6 <- function(){
model_ready |> 
  select(prop_male, lgdp, ltfr, gii) |> 
  ggpairs() |> 
    print()
}

svg_png(p6, "../img/0290-pairs", w = 10, h = 6)


#-------------modelling--------------------

model0 <- lmer(ltfr ~ gii + log(gdprppppc)  + (1 | country_fac), 
                data = model_ready)

model1 <- lmer(ltfr ~ gii + log(gdprppppc) + prop_male + (1 | country_fac), 
            data = model_ready)

model2 <- lmer(ltfr ~ gii + log(gdprppppc) * prop_male + (1 | country_fac), 
               data = model_ready)

post_fit <- model_ready |> 
  mutate(res0 = residuals(model0, type = "pearson"),
         fit0 = exp(fitted(model0))) |> 
  mutate(res0st = res0 / sd(res0))

refsd <- 0.745

# Combined diagram of 4 plots to diagnose model0 and look at possibilities
# for needing model1 or model2
p7 <- post_fit |> 
  ggplot(aes(x = res0st)) +
  geom_density() +
  geom_rug() +
  stat_function(fun = dnorm, args = list(mean = 0, sd = refsd), 
                colour = "red") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                colour = "darkblue") +
  labs(x = "Standardised residual",
       y = "Density",
       subtitle = "A.") +

post_fit |> 
  ggplot(aes(sample = res0st)) +
  stat_qq() +
  geom_abline(slope = 1, intercept = 0, colour = "darkblue") +
  geom_abline(slope = refsd, intercept = 0, colour = "red") +
  labs(x = "Residual expected if normally distributed",
       y = "Actual residual",
       subtitle = "B.") +
  
  
post_fit |>
  ggplot(aes(x = fit0, y = res0st)) +
  geom_hline(yintercept = 0, colour = "red") +
  geom_point() +
  geom_smooth(colour = "white") +
  scale_x_log10() +
  labs(x = "Fitted value of total fertility rate",
       y = "Standardised residual",
       subtitle = "C.") +
  
post_fit |> 
  ggplot(aes(x = prop_male, y = res0, colour = gdprppppc)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", colour = "white") +
  scale_color_viridis_c(label = dollar_format(accuracy = 1), trans = log_trans(),
                        breaks = c(3, 8, 22, 60) * 1000) +
  scale_x_continuous(label = percent) +
  theme(legend.position = "right") +
  labs(subtitle = "D. This plot identifies any residual fertility rate to be explained by housework",
       colour = "PPP GDP
per capita",
       x = "Proportion of housework done by males",
       y = "Total fertility rate (on log scale) 
*not* explained by GDP model") +
  
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  
  plot_layout(nrow = 2) +
  
  plot_annotation(title = "Diagnostic plots for model of log total fertility rate on countries' GDP per capita and Gender Inequality Index",
                  subtitle = "Male share of housework not explictly included in the model")

svg_png(p7, "../img/0290-diagnose-0", w = 13, h = 7)

  
anova(model0, model1)
anova(model0, model1, model2)
anova(model0, model2)

# prop_male about -8.5 so very negatively correlated; but some interaction with
# GDP
summary(model2, cor = FALSE)


# Manual way of building a plot. Not even using predict()
b <- fixef(model2)

#' Predict TFR given those coefficients
calc_tfr <- function(prop_male, gdp, gii = mean(model_ready$gii)){
  exp(b[1] + 
      b[2] * gii + 
      b[3] * log(gdp) + 
      b[4] * prop_male + 
      b[5] * prop_male * log(gdp))
}


p8 <- tibble(prop_male = rep(seq(from = 0.05, to = 0.45, length.out = 50), 3),
       gdp = rep(c(3000, 10000, 80000), each = 50)) |> 
  mutate(tfr = c(calc_tfr(prop_male, gdp)),
         gdp = dollar(gdp),
         gdp = fct_relevel(gdp, "$3,000")) |> 
  ggplot(aes(x = prop_male, colour = gdp, y = tfr)) +
  geom_line(linewidth = 1.5) +
  geom_point(data = model_ready, colour = "black") +
  scale_x_continuous(label = percent) +
  labs(x = "Proportion of adult housework done by men",
       y = "Predicted total fertility rate",
       colour = "PPP GDP per capita")

svg_png(p8, "../img/0290-home-made-preds", w = 10, h = 6)


p9 <- plot_predictions(model2, points = 1, condition = list(
  "prop_male",
  "gdprppppc" = c(3000, 10000, 80000))) +
  scale_y_continuous(trans = transform_exp(),
                     breaks = log(c(2, 4, 6)),
                     label = comma,
                     sec.axis = sec_axis(exp, name = "Total Fertility Rate")) +
  scale_x_continuous(label = percent) +
  labs(y = "log(total fertility rate)",
       colour = "PPP GDP per capita",
       fill = "PPP GDP per capita",
       x = "Proportion of adult housework done by men")

svg_png(p9, "../img/0290-margeff-preds", w = 10, h = 6)


# Check this one is really just like model2, just different estimation
model7 <- gamm(tfr ~ gii + log(gdprppppc) * prop_male +  s(country_fac, bs = 're'), 
               data = model_ready, family = quasipoisson)


summary(model7$lme)$tTable |> 
  as.data.frame() |> 
  mutate(`p-value` = round(`p-value`, 3)) |> 
  mutate(mod2_coefs = fixef(model2)) |> 
  mutate(across(where(is.numeric), round, digits = 2))

# what about with less linearity
model4 <- gamm(tfr ~ s(gii) + s(log(gdprppppc)) + s(country_fac, bs = 're'), 
              data = model_ready, family = quasipoisson)

model5 <- gamm(tfr ~ s(gii) + s(log(gdprppppc)) + s(prop_male) + s(country_fac, bs = 're'), 
               data = model_ready, family = quasipoisson)

model6 <- gamm(tfr ~ s(gii) + s(log(gdprppppc), prop_male) + s(country_fac, bs = 're'), 
               data = model_ready, family = quasipoisson)



p10 <- function(){
  plot(model6$gam, pages = TRUE)
}

svg_png(p10, "../img/0290-gam", w = 13, h = 7)


anova(model4$lme, model5$lme, model6$lme)
anova(model4$lme, model6$lme)

summary(model6$lme)$tTable |> 
  as.data.frame() |> 
  mutate(`p-value` = round(`p-value`, 3)) 

summary(model4$lme)$tTable |> 
  as.data.frame() |> 
  mutate(`p-value` = round(`p-value`, 3)) 


# Basically says if you let the GDP and GII effect be all curved there's no need
# for a prop_male. Is the lesson one about vulnerability to some modelling choices.