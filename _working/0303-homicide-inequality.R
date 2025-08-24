

library(tidyverse)
library(WDI)
library(lme4)
library(forecast) # for chosing box cox parameters
library(AICcmodavg)

WDIsearch("homicide")
WDIsearch("Gini")

d <- WDI(indicator = c(homicide = "VC.IHR.PSRC.P5", 
                              gini = "SI.POV.GINI")) |> 
  as_tibble()

d |> 
  drop_na() |> 
  distinct(country)


highlights <- c("United States", "Russian Federation", 
                  "Samoa", "Australia", "United Kingdom",
                  "Fiji", "Mexico", "Papua New Guinea")

lambda <- forecast::BoxCox.lambda(d$homicide)


d2 <- d |> 
  drop_na() |> 
  group_by(country) |> 
  mutate(type = ifelse(year == max(year), "Latest", "Earlier")) |> 
  ungroup() |> 
  mutate(label = ifelse(type == "Latest" & (gini > 53 | homicide > 35 | 
                                 gini < 26 | homicide < 0.26 | country %in% highlights),
                        country, ""),
         hom_tran = BoxCox(homicide,lambda = lambda)) 

model0 <- lm(hom_tran ~ gini, data = d2)
model1 <- lmer(hom_tran ~ gini + (1 | country), data = d2)
summary(model0)
summary(model1)

# find the country with random effect closest to zero

avg_country <- ranef(model1)[[1]] |> 
  arrange(abs(`(Intercept)`)) |> 
  slice(1) |> 
  row.names()

pred_grid <- tibble(gini = 20:65, country = avg_country)

pse0 <- predict(model0, newdata = pred_grid, se = TRUE)
pse1 <- predictSE(model1, newdata = pred_grid)

pred_grid <- pred_grid |> 
  mutate(predicted0 = pse0$fit,
         lower0 = predicted0 - 1.96 * pse0$se.fit,
        upper0 = predicted0 + 1.96 * pse0$se.fit) |> 
  mutate(predicted1 = pse1$fit,
         lower1 = predicted1 - 1.96 * pse1$se.fit,
        upper1 = predicted1 + 1.96 * pse1$se.fit) |> 
  mutate(across(predicted0:upper1, function(x){InvBoxCox(x, lambda = lambda)}))

mod_cols <- c("purple", "darkgreen")

p1 <- d2 |> 
  ggplot(aes(x = gini, y = homicide)) +
  geom_ribbon(data = pred_grid, aes(ymin = lower0, ymax = upper0, y = NA), 
               fill = mod_cols[1], alpha = 0.2) +
  geom_ribbon(data = pred_grid, aes(ymin = lower1, ymax = upper1, y = NA), 
                fill = mod_cols[2], alpha = 0.2) +
  geom_point(aes(shape = type, colour = type)) +
  geom_label_repel(aes(label = label), max.overlaps = Inf, colour = "grey10", size = 2.7,
                   seed = 123, label.size = unit(0, "mm"), fill = rgb(0,0,0, 0.04)) +
  annotate("text", x = 61.5, y = 2.5, colour = mod_cols[2], hjust = 0,
               label = str_wrap("Mixed-effects model takes into account random 'country level' effects.", 26)) +
  annotate("text", x = 61.5, y = 105, colour = mod_cols[1], hjust = 0,
               label = str_wrap("Naive model treats each point as equally valuable.", 26)) +
  annotate("segment", x = 64, xend = 64, y = 4, yend = 6, colour = mod_cols[2], 
            arrow = arrow(length = unit(2, "mm"))) +
  annotate("segment", x = 64, xend = 64, y = 80, yend = 40, colour = mod_cols[1], 
            arrow = arrow(length = unit(2, "mm"))) +
  
  scale_y_log10() +
  xlim(18, 70) +
  scale_shape_manual(values = c(1, 19)) +
  scale_colour_manual(values = c("steelblue", "black")) +
  labs(x = "Income inequalty (Gini coefficient)",
       y = "Homicide rate (per 100,000)",
      colour = "Observation year:",
      shape = "Observation year:",
      title = "Higher inequality countries have more homicides",
      subtitle = "Selected countries highlighted. Green modelled line is 95% confidence interval of a mixed effects model with random country effect.")

svg_png(p1, "../img/0303-scatter", w = 10, h = 7)

