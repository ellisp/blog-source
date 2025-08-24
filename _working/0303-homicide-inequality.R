library(tidyverse)
library(WDI)
library(lme4)
library(forecast) # for chosing box cox parameters
library(AICcmodavg) # for predictSE with lmer
library(ggrepel)
library(mgcv)
library(gratia)
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
lambda <- forecast::BoxCox.lambda(d$homicide)


d2 <- d |> 
  drop_na() |> 
  group_by(country) |> 
  mutate(type = ifelse(year == max(year), "Latest", "Earlier")) |> 
  ungroup() |> 
  mutate(label = ifelse(type == "Latest" & (gini > 53 | homicide > 35 | 
                                 gini < 26 | homicide < 0.26 | country %in% highlights),
                        country, ""),
         hom_tran = BoxCox(homicide, lambda = lambda),
         country = as.factor(country)) 

#------------Modelling and predictions (for drawing 95% confidence intervals on charts)----

model0 <- lm(hom_tran ~ gini, data = d2)
model1 <- lmer(hom_tran ~ gini + (1 | country), data = d2)
# log link function
model2 <- gam(homicide ~ gini + s(country, bs = "re"), 
              family = quasipoisson, data = d2, method = "REML")

summary(model0)
summary(model1)


# Is the average random country effect basically zero? - check:
stopifnot(round(mean(ranef(model1)[[1]][,1]), 10) == 0)

# Find the country with random effect closest to zero. Needed for predictions
# to draw an average country ribbon on the chart
avg_country <- ranef(model1)[[1]] |> 
  arrange(abs(`(Intercept)`)) |> 
  slice(1) |> 
  row.names()

# compare the country level effects
tibble(m1 = ranef(model1)[[1]]$`(Intercept)`,
       m2 = as.numeric(smooth_coefs(model2, "s(country)")),
      country = levels(d2$country)) |> 
  arrange(abs(m2) + abs(m1))

pred_grid <- tibble(gini = 20:65, country = avg_country)

pse0 <- predict(model0, newdata = pred_grid, se = TRUE)
pse1 <- predictSE(model1, newdata = pred_grid)
pse2 <- predict(model2, newdata = pred_grid, se = TRUE)

pred_grid <- pred_grid |> 
  mutate(predicted0 = pse0$fit,
         lower0 = predicted0 - 1.96 * pse0$se.fit,
        upper0 = predicted0 + 1.96 * pse0$se.fit) |> 
  mutate(predicted1 = pse1$fit,
         lower1 = predicted1 - 1.96 * pse1$se.fit,
        upper1 = predicted1 + 1.96 * pse1$se.fit) |> 
  mutate(predicted2 = pse2$fit,
         lower2 = predicted2 - 1.96 * pse2$se.fit,
        upper2 = predicted2 + 1.96 * pse2$se.fit) |> 
  mutate(across(predicted0:upper1, function(x){InvBoxCox(x, lambda = lambda)}),
         across(predicted2:upper2, function(x){exp(x)}))

#------------------Draw chart--------------------

mod_cols <- c("purple", "darkgreen", "brown")

p1 <- d2 |> 
  ggplot(aes(x = gini, y = homicide)) +
  geom_ribbon(data = pred_grid, aes(ymin = lower0, ymax = upper0, y = NA), 
               fill = mod_cols[1], alpha = 0.2) +
  geom_ribbon(data = pred_grid, aes(ymin = lower1, ymax = upper1, y = NA), 
               fill = mod_cols[2], alpha = 0.2) +
# ribbon from fit with gam, is very similar to lmer, just a bit wider. Commented out.
#  geom_ribbon(data = pred_grid, aes(ymin = lower2, ymax = upper2, y = NA), 
#                fill = mod_cols[3], alpha = 0.2) +
  geom_point(aes(shape = type, colour = type)) +
  geom_label_repel(aes(label = label), max.overlaps = Inf, colour = "grey10", size = 2.8,
                   seed = 123, label.size = unit(0, "mm"), fill = rgb(0,0,0, 0.04)) +
  annotate("text", x = 61.5, y = 3.6, colour = mod_cols[2], hjust = 0, vjust = 1,
               label = str_wrap("Mixed-effects model takes into account random 'country level' 
                                effects, and repeated observations for each country.", 26)) +
  annotate("text", x = 61.5, y = 135, colour = mod_cols[1], hjust = 0, vjust = 1,
               label = str_wrap("Naive model treats each point as equally valuable.", 26)) +
  annotate("segment", x = 64, xend = 64, y = 4, yend = 6, colour = mod_cols[2], 
            arrow = arrow(length = unit(2, "mm"))) +
  annotate("segment", x = 64, xend = 64, y = 80, yend = 40, colour = mod_cols[1], 
            arrow = arrow(length = unit(2, "mm"))) +
  scale_y_log10() +
  xlim(18, 70) +
  scale_shape_manual(values = c(1, 19)) +
  scale_colour_manual(values = c("steelblue", "black")) +
  labs(x = "Individual income (or in some cases, consumption) inequality (Gini coefficient)",
       y = "Homicide rate (per 100,000)",
      colour = "Observation year:",
      shape = "Observation year:",
      title = "Higher inequality countries have more homicides. But modelling choices impact on 'how much'.",
      subtitle = "Selected countries highlighted. Green modelled line is 95% confidence interval of a mixed effects model with random country effect.",
      caption = "Source: World Bank, World Development Indicators, series VC.IHR.PSRC.P5 and SI.POV.GINI. Analysis by freerangestats.info.")

svg_png(p1, "../img/0303-scatter", w = 10, h = 6.5)

