library(tidyverse)
library(readxl)
library(mgcv)
library(lme4)
library(sf)
library(GGally)


# county level prevalence of depression at (have to hit the 'download' button)
# https://stacks.cdc.gov/view/cdc/129404
dep <- read_excel("cdc_129404_DS1.xlsx", skip = 1)

fn <- "2024_US_County_Level_Presidential_Results.csv"
if(!file.exists(fn)){
  download.file("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-24/refs/heads/master/2024_US_County_Level_Presidential_Results.csv",
                destfile = fn)
}

votes <- read_csv("2024_US_County_Level_Presidential_Results.csv")

combined <- votes |>
  inner_join(dep, by = c("county_fips" = "CountyFIPS code")) |>
  mutate(cpe = `Crude Prevalence Estimate` / 100,
         aape = `Age-adjusted Prevalence Estimate` / 100)

# what was missed?
votes |>
  anti_join(dep, by = c("county_fips" = "CountyFIPS code")) |>
  count(state_name)
# 37 counties in Alaska, 9 and Connecticut and 7 in DC. Will ignore these
# for my purposes.

#========================modelling==================


#----------Ordinary Least Squares------------------

model <- lm(per_gop ~ cpe, data = combined)
summary(model)
# note several things could be happening here:
# - depressed people makes you vote for Trump
# - being around depressed people makes you vote for Trump
# - some underlying condition (eg economic) both leads to higher depression
#   and more likely to vote for Trump. This seems the most likely.

the_caption = "Source: data from tonmcg and CDC; analysis by freerangestats.info"

p1<- combined |>
  ggplot(aes(x= cpe, y = per_gop)) +
  geom_point(colour = "steelblue", alpha = 0.5) +
  geom_smooth(method = "lm", fill = "black", colour = "white", alpha = 0.8) +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = percent) +
  labs(x = "Crude prevalence estimate of depression",
       y = "Percentage vote for Trump in 2024 election",
       subtitle = "Line is ordinary least squares fit to all county data together",
       title = "Counties with more depression voted more for Trump",
       caption = the_caption)

svg_png(p1, "../img/0286-ols", w = 9, h = 6)

#----------------Quasibinomial GLM----------------

model2 <- glm(per_gop ~ cpe, 
              family = quasibinomial, data = combined, weights = total_votes)
summary(model2)

preds2 <- predict(model2, type = "response", se.fit = TRUE)

p2 <-combined |>
  mutate(fit = preds2$fit,
         se = preds2$se.fit,
         lower = fit - 1.96 * se,
         upper = fit + 1.96 * se) |>
  ggplot(aes(x = cpe, y = per_gop)) +
  geom_point(colour = "steelblue", alpha = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "black", alpha = 0.5) +
  geom_line(aes(y = fit), colour = "white") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 1), label = percent) +
  scale_x_continuous(label  = percent)  +
  labs(x = "Crude prevalence estimate of depression",
       y = "Percentage vote for Trump in 2024 election",
       subtitle = "Line is generalized linear model with quasibinomial response, fit to all county data together",
       title = "Counties with more depression voted more for Trump",
       caption = the_caption)

svg_png(p2, "../img/0286-glm", w = 9, h = 6)


#---------------------With state random effect with lmer4::glmer--------------------

model4 <- lme4::glmer(per_gop ~ cpe + (1 | state_name), 
                      family = "binomial", data = combined, 
                      weights = total_votes)
# note can't use quasibinomial family with glmer so we aren;t really dealing
# properly with the overdispersion. what to do about that? Confidence intervals
# will be too narrow. Various alternatives posisble.

summary(model4)

preds4 <- predict(model4, se.fit = TRUE, type = "response")

p4 <- combined |>
  mutate(fit = preds4$fit,
         se = preds4$se.fit,
         lower = fit - 1.96 * se,
         upper = fit + 1.96 * se) |>
  ggplot(aes(x = cpe, group = state_name)) +
  geom_point(aes(y = per_gop, colour = state_name), alpha = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "black", alpha = 0.5) +
  geom_line(aes(y = fit, colour = state_name)) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 1), label = percent) +
  scale_x_continuous(label  = percent)  +
  labs(x = "Crude prevalence estimate of depression",
       y = "Percentage vote for Trump in 2024 election",
       subtitle = "Lines are logistic regression with state-level random intercept effect (confidence intervals are too narrow)",
       title = "Counties with more depression voted more for Trump",
       caption = the_caption)

svg_png(p4, "../img/0286-glmer", w = 9, h = 6)

#--------------state random effect with mgcv::gam--------------
# gam lets us have a random effect and a wider range of families
# see https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/

# must be a factor to use as a random effect in gam():
combined <- mutate(combined, state_name = as.factor(state_name))

model5 <- gam(per_gop ~ cpe + s(state_name, bs = 're') , 
              family = quasibinomial, weights = total_votes,
              data = combined)
summary(model5)
# note standard error for cpe is much higher 0.6224, compared to 0 .0107

preds5 <- predict(model5, se.fit = TRUE, type = "response")

p5 <- combined |>
  mutate(fit = preds5$fit,
         se = preds5$se.fit,
         lower = fit - 1.96 * se,
         upper = fit + 1.96 * se) |>
  ggplot(aes(x = cpe, group = state_name)) +
  geom_point(aes(y = per_gop, colour = state_name), alpha = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "black", alpha = 0.5) +
  geom_line(aes(y = fit, colour = state_name)) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 1), label = percent) +
  scale_x_continuous(label  = percent)  +
  labs(x = "Crude prevalence estimate of depression",
       y = "Percentage vote for Trump in 2024 election",
       subtitle = "Lines are quasibinomial generalized additive model with state-level random intercept",
       title = "Counties with more depression voted more for Trump",
       caption = the_caption)

svg_png(p5, "../img/0286-gam", w = 9, h = 6)

#-----------gam, spatial, state effect--------------
# each county isn't really an independent data point, as counties next to eachother
# probably have lots in common. A great thing about gam is that not only can we
# have a quasibinomial family, we can do gam core business of adding in splines,
# including a two dimensional "rubber mat" that effectively knocks out our 
# spatial correlation problem for us.
#
# but first we need to know the centroids of all the counties:

fn <- "cb_2023_us_county_500k.zip"
if(!file.exists(fn)){
  download.file('https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_us_county_500k.zip',
                destfile = "cb_2023_us_county_500k.zip", mode = "wb")
}
unzip(fn)

counties <- st_read("cb_2023_us_county_500k.shp")
county_cent <- counties |>
  st_centroid() 

sc <- st_coordinates(county_cent)

county_cent <- county_cent |>
  mutate(x = sc[, 1],
         y = sc[, 2],
         # combine the two digit state code with the 3 digit county code:
         county_fips = paste0(STATEFP, COUNTYFP))

# check that we have successfully re-created the country_fips on same basis
# as our voting and depression data:
combined |>
  left_join(county_cent, by = "county_fips") |>
  select(county_name, NAME)

combined2 <- combined |>
  left_join(county_cent, by = "county_fips")

# check the county centres are where we expect. Note Alaska still missing
# (because voting data is not by country so lost on the first join)
p6a <- ggplot(combined2, aes(x = x, y = y, colour = state_name)) + 
  geom_point() +
  theme(legend.position = "none") +
  coord_map() +
  labs(title = "Centres of counties after merging data")

svg_png(p6a, "../img/0286-counties-map", w = 9, h = 6)


model6 <- gam(per_gop ~ cpe + s(x, y) + s(state_name, bs = 're') , 
              family = quasibinomial, weights = total_votes,
              data = combined2)

# the spatial rubber mat that is correcting for spatial correlation for us;
# scheme=1 is what makes it draw a perspective plot rather than contour or
# heatmap):
p6b <- function(){plot(model6, select = 1, scheme = 1, main = "Higher vote for GOP")}
svg_png(p6b, "../img/0286-rubber-sheet", w = 9, h = 6)

preds6 <- predict(model6, se.fit = TRUE, type = "response")

p6c <- combined2 |>
  mutate(fit = preds6$fit,
         se = preds6$se.fit,
         lower = fit - 1.96 * se,
         upper = fit + 1.96 * se) |>
  ggplot(aes(x = cpe, group = state_name)) +
  geom_point(aes(y = per_gop, colour = state_name), alpha = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "black", alpha = 0.5) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 1), label = percent) +
  scale_x_continuous(label  = percent)  +
  labs(x = "Crude prevalence estimate of depression",
       y = "Percentage vote for Trump in 2024 election",
       subtitle = "Grey ribbons are 95% confidence intervals from quasibinomial generalized additive model with spatial effect and state-level random intercept effect",
       title = "Counties with more depression voted more for Trump",
       caption = the_caption) +
  facet_wrap(~state_name)

svg_png(p6c, "../img/0286-gam-spatial", w = 11, h = 8)
