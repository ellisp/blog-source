library(tidyverse)
library(WDI)
library(nlme)
library(forecast) # for chosing box cox parameters
library(AICcmodavg) # for predictSE with lmer
library(ggrepel)
library(GGally)

# Analysis to follow up this remark:
# https://mastodon.sdf.org/@dlakelan/115055789165555934
# "On the other hand, the homicide rate across the world 
#  is well modeled as exp(k * income_gini_coefficient)."

#--------Download World Bank data and prep it------------
# These searches were used to find series with a lot of data for country-year combinations:
# WDIsearch("homicide")
# WDIsearch("Gini")

# Metadata for SI.POV.GINI at https://data360files.worldbank.org/data360-data/metadata/WB_WDI/WB_WDI_SI_POV_GINI.pdf
# Key points include:
# * individuals (not household)
# * adjusted for household size
# * can be income or consumption depending on what was available

d <- WDI(indicator = c(homicide = "VC.IHR.PSRC.P5", 
                              gini = "SI.POV.GINI")) |> 
  as_tibble()

# Countries we are going to want to highlight in our chart
highlights <- c("United States", "Russian Federation", 
                  "Samoa", "Australia", "United Kingdom",
                  "Fiji", "Mexico", "Papua New Guinea")

# There are a few country-years with zero homicides so can't use a simple
# logarithm transformation, but can have a Box-Cox that has a similar result
# Choose a lambda that gives a distribution after the transformation where
# changes are relatively stable in absolute terms:
lambda <- forecast::BoxCox.lambda(d$homicide)

# version of the data we will use for plotting and modelling:
d2 <- d |> 
  drop_na() |> 
  group_by(country) |> 
  mutate(type = ifelse(year == max(year), "Latest", "Earlier")) |> 
  mutate(ctry_avg_gini = mean(gini)) |> 
  ungroup() |> 
  mutate(label = ifelse(type == "Latest" & (gini > 53 | homicide > 35 | 
                                 gini < 26 | homicide < 0.26 | country %in% highlights),
                        country, ""),
         hom_tran = BoxCox(homicide, lambda = lambda),
         country = as.factor(country)) |> 
  arrange(year)

#------------Modelling and predictions (for drawing 95% confidence intervals on charts)----
# you could have random slopes too but a fair number of countries have only one
# or two observations
# see https://www.frontiersin.org/journals/education/articles/10.3389/feduc.2017.00058/full
# for example which says you want at least 6 observations per group to have
# a random slope. Not sure how many you need, but 1 isn't enough :)

p1 <- d2 |> 
  count(country, name = "n_obs") |> 
  count(n_obs, name = "n_countries") |> 
  ggplot(aes(x = n_obs, y = n_countries)) +
  geom_col(fill = "steelblue") +
  labs(x = "Number of observations (ie years)",
       y = "Number of countries",
      title = "Complete data for Gini coefficient and homicide rates")

svg_png(p1, "../img/0303-number-obs", w = 9, h = 5)

# super simple model, not mentioned in the actual blog post
model1 <- lm(hom_tran ~ gini, data = filter(d2, type == "Latest"))

# multilevel model with a country random intercept, and inequality only at the lowest level
model2 <- lme(hom_tran ~ gini, random = ~1  | country, 
                    data = d2, correlation = corAR1())


# multilevel model with country random intercept and countries' average inequality, as well
# as the lowest level (country-year) granularity inequality:
model3 <- lme(hom_tran ~ gini + ctry_avg_gini, random = ~1  | country, 
                    data = d2, correlation = corAR1())

# Fairly high (and similar) coefficients, about 0.11, but for differ
summary(model1) # 0.12 for gini
summary(model3) # 0.11 for ctry_avg_gini; gini not significant

# Relatively low coefficients - the country randomness 
# soaks up a lot of the randomness:
summary(model2) # gini not significant

# Is the average random country effect basically zero? - check:
stopifnot(round(mean(ranef(model2)[[1]]), 10) == 0)
stopifnot(round(mean(ranef(model3)[[1]]), 10) == 0)

# Find the country with random effect closest to zero. Needed for predictions
# to draw an average country ribbon on the chart
avg_country <- ranef(model3) |> 
  arrange(abs(`(Intercept)`)) |> 
  slice(1) |> 
  row.names()

pred_grid <- tibble(gini = 20:65, 
                    ctry_avg_gini = 20:65, 
                    country = avg_country)

pse1 <- predict(model1, newdata = pred_grid, se = TRUE)
pse2 <- predictSE(model2, newdata = pred_grid, se = TRUE)
pse3 <- predictSE(model3, newdata = pred_grid, se = TRUE)


pred_grid <- pred_grid |> 
  mutate(predicted1 = pse1$fit,
         lower1 = predicted1 - 1.96 * pse1$se.fit,
        upper1 = predicted1 + 1.96 * pse1$se.fit) |> 
  mutate(predicted2 = pse2$fit,
         lower2 = predicted2 - 1.96 * pse2$se.fit,
        upper2 = predicted2 + 1.96 * pse2$se.fit) |> 
  mutate(predicted3 = pse3$fit,
         lower3 = predicted3 - 1.96 * pse3$se.fit,
        upper3 = predicted3 + 1.96 * pse3$se.fit) |> 
  mutate(across(predicted1:upper3, function(x){InvBoxCox(x, lambda = lambda)}))

#------------------Draw chart--------------------

mod_cols <- c("purple", "darkgreen", "brown", "pink")

p2 <- d2 |> 
  ggplot(aes(x = gini, y = homicide)) +
  scale_y_log10() +
  xlim(18, 70) +
  scale_shape_manual(values = c(1, 19)) +
  scale_colour_manual(values = c("steelblue", "black")) +
  labs(x = "Inequality (Gini coefficient), based on individual income or in some cases, consumption. Higher means more inequality.",
       y = "Homicide rate (per 100,000)",
      colour = "Observation year:",
      shape = "Observation year:",
      title = "Higher inequality countries have more homicides.",
      caption = "Source: World Bank, World Development Indicators, series VC.IHR.PSRC.P5 and SI.POV.GINI. Analysis by freerangestats.info.") 
  
  
p2a <- p2 +
  # model2 - just country-year level data, country random effect
  geom_ribbon(data = pred_grid, aes(ymin = lower2, ymax = upper2, y = NA), 
                fill = mod_cols[2], alpha = 0.2) +
  
  #model3 - country-year but also country average data, country random effect
  geom_ribbon(data = pred_grid, aes(ymin = lower3, ymax = upper3, y = NA), 
                fill = mod_cols[3], alpha = 0.2) +
  
  geom_point(aes(shape = type, colour = type)) +
  geom_label_repel(aes(label = label), max.overlaps = Inf, colour = "grey10", size = 2.8,
                   seed = 123, label.size = unit(0, "mm"), fill = rgb(0,0,0, 0.04))  +
  # annotations - text and arrows - for models 2 and 3
  annotate("text", x = 61.5, y = 0.9, colour = mod_cols[2], hjust = 0, vjust = 1,
               label = str_wrap("Model with no such average country inequality effect.", 24)) +
  annotate("text", x = 61.5, y = 200, colour = mod_cols[3], hjust = 0, vjust = 1,
               label = str_wrap("Model including an effect for average inequality in each 
                                country over time.", 26)) +
  
  annotate("segment", x = 64, xend = 64, y = 1, yend = 2, colour = mod_cols[2], 
            arrow = arrow(length = unit(2, "mm"))) +
  annotate("segment", x = 64, xend = 64, y = 75, yend = 45, colour = mod_cols[3], 
            arrow = arrow(length = unit(2, "mm"))) +
  
  labs(subtitle = "Selected countries highlighted. 
Shaded ribbons show 95% confidence interval of mixed effects models with random country effect and autocorrelated error terms.",
)

svg_png(p2a, "../img/0303-scatter", w = 10, h = 6.5)


p2b <- p2 +
  
  #model3 - country-year but also country average data, country random effect
  geom_ribbon(data = pred_grid, aes(ymin = lower3, ymax = upper3, y = NA), 
                fill = mod_cols[3], alpha = 0.2) +
  
  geom_point(aes(shape = type, colour = type)) +
  geom_label_repel(aes(label = label), max.overlaps = Inf, colour = "grey10", size = 2.8,
                   seed = 123, label.size = unit(0, "mm"), fill = rgb(0,0,0, 0.04)) 

svg_png(p2b, "../img/0303-scatter-final", w = 10, h = 6.5)


#---------------residuals from model2 and model3---------------

# Individual level residuals - plot not used in blog
p3 <- d2 |> 
  mutate(`Residuals from Model 2` = residuals(model2),
          `Residuals from Model 3` = residuals(model3)) |> 
  select(gini, `Residuals from Model 2`, `Residuals from Model 3`) |> 
  gather(variable, value, -gini) |> 
  ggplot(aes(x = gini, y = value)) +
  facet_wrap(~variable) +
  geom_point() +
  labs(x = "Inequality",
       y = "Residuals (on Box-Cox transformed scale)",
       title = "Residuals from two mixed-effects models of homicide",
       subtitle = "Residuals at the most granular level ie year-country are uncorrelated with inequality")

svg_png(p3, "../img/0303-residuals", w = 8, h = 4)

# out of curiousity what are those low outlier residuals? - Iceland and Malta
round(sort(residuals(model2)),2)[1:5]
round(sort(residuals(model3)),2)[1:5]


# Country level effects plot - pairs plot used in blog
p4 <- function(){
  p <- d2 |> 
  distinct(country, ctry_avg_gini) |> 
  arrange(country) |> 
  mutate(`Country effects in Model 2` = ranef(model2)[[1]],
         `Country effects in Model 3` = ranef(model3)[[1]]) |> 
  select(-country) |> 
  rename(`Countries' average inequality` = ctry_avg_gini) |> 
  ggpairs() +
  labs(title = "Comparison of country-level random effects in two models",
      subtitle = "Model 3 has a fixed effect for countries' average inequality; Model 2 doesn't.
The country-level residuals are correlated with this variable in Model 2, but not Model 3.")
 
    print(p)
}

svg_png(p4, "../img/0303-country-level-ranef", w = 10, h = 7.5)


#---------------further illustrations - facets----------------------
d3 <- d2 |> 
  filter(country %in% c(highlights, "South Africa", "France", "Japan", "Ukraine")) |> 
  group_by(country) |> 
  summarise(r = coef(lm(log10(homicide) ~ gini))[2]) |> 
  mutate(r = replace_na(r, 0)) |> 
  mutate(country = fct_reorder(country, r) |>  fct_drop()) |> 
  arrange(country)


pred_grid2 <- d2 |> 
  filter(country %in% d3$country) |> 
  distinct(country, ctry_avg_gini) |> 
  expand_grid(gini = 20:70)

more_preds <- predictSE(model3, newdata = pred_grid2, se = TRUE)


pred_grid2 <- pred_grid2 |> 
  mutate(predicted3 = more_preds$fit,
         lower3 = predicted3 - 1.96 * more_preds$se.fit,
        upper3 = predicted3 + 1.96 * more_preds$se.fit) |> 
  mutate(across(predicted3:upper3, function(x){InvBoxCox(x, lambda = lambda)}))



p5 <- d2 |> 
  mutate(country = factor(country, levels = levels(d3$country))) |> 
  drop_na() |> 
  ggplot(aes(x = gini, y = homicide)) +
  scale_y_log10(label = comma, limits = c(0.1, 100)) +
  xlim(18, 70) +
  scale_shape_manual(values = c(1, 19)) +
  scale_colour_manual(values = c("steelblue", "black")) +
  geom_ribbon(data = pred_grid2, aes(ymin = lower3, ymax = upper3, y = NA), 
                fill = mod_cols[3], alpha = 0.2) +
  geom_smooth(method = lm, se = FALSE, colour = "grey50", fullrange = TRUE, linewidth = 0.5) +
  geom_point(aes(shape = type, colour = type)) +
  facet_wrap(~country, ncol = 3)  +
  labs(x = "Inequality (Gini coefficient), based on individual income or in some cases, consumption. Higher means more inequality.",
       y = "Homicide rate (per 100,000)",
      colour = "Observation year:",
      shape = "Observation year:",
      title = "Higher inequality countries have more homicides.",
      subtitle = "But for any given country, the relationship over time may well be the reverse.
Pale ribbon shows overall model's confidence interval for homicide for this country, at its average over time level of inequality.
Dark line shows simple model fit to just this country's data.",
      caption = "Source: World Bank, World Development Indicators, series VC.IHR.PSRC.P5 and SI.POV.GINI. Analysis by freerangestats.info.") 
  

svg_png(p5, "../img/0303-faceted", w = 10, h = 7.5)

