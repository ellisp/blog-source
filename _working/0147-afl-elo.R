# devtools::install_github("jimmyday12/fitzRoy")
library(tidyverse)
library(scales)
library(fitzRoy)   # for reading AFL scores
library(frs)       # for Elo functions
library(Cairo)
library(ggrepel)
library(lubridate)

update_geom_defaults("text_repel", list(colour = "brown", font = myfont ))
the_caption <- "Source: afltables via fitZroy; analysis by freerangestats.info"

results <- get_match_results()

r <- results %>%
  rename_all(tolower) %>%
  rename_all(function(x){gsub("\\.", "_", x)}) %>%
  # limit to 2018 season and earlier:
  filter(date < as.Date("2019-01-01")) %>%
  select(home_team, away_team, date, game, margin) %>%
  gather(location, team, -date, -game, -margin) %>%
  mutate(location = gsub("_team", "", location),
         margin = ifelse(location == "home", margin, -margin)) %>%
  mutate(winner = margin > 0) %>%
  arrange(date) %>%
  mutate(starting_elo = 1500,
         new_elo = 1500) %>%
  group_by(game) %>%
  # sequence so the winner is the first listed each time for each game:
  arrange(game, desc(winner))

#' Calculate Elo ratings for 
#' 
#' @param r a data frame with columns including date, team, game, starting_elo, new_elo
afl_elos <- function(r, sc = 6){
  r <- r %>%
    group_by(game) %>%
    arrange(date, game, desc(winner))
  
  all_games <- unique(r$game)
  
  # This loop will give some tidyverse advocates conniptions but it works for me; and
  # it seems inherently iterative so perhaps a loop is the logical way to do it:
  for(g in all_games){
    this_game <- r[r$game == g, ] 
    er <- elo_rating(a = this_game[1, "starting_elo"], 
                     b = this_game[2, "starting_elo"],
                     winner = "a",
                     ml = round(this_game[1, "margin"] / sc))
    
    r[r$game == g, "new_elo"]  <- unlist(c(er$a, er$b))
    r <- r %>%
      group_by(team) %>%
      mutate(starting_elo = lag(new_elo, default = 1500)) %>%
      ungroup()
  }
  r <- r %>%
    mutate(season = lubridate::year(date)) %>%
    group_by(game) %>%
    arrange(game, desc(starting_elo)) %>%
    mutate(predicted_winner = c(TRUE, FALSE),
           successful_prediction = predicted_winner == winner) %>%
    ungroup()
    
  return(r)
}

# Elo ratings if we started back in 1897 (takes a couple of minutes):
elos_all <- afl_elos(r)


CairoSVG("../img/0147-long-history.svg", 10, 6)
# Plot of Elo ratings over all of history
elos_all %>%
  mutate(team = fct_reorder(team, -new_elo, .fun = last)) %>%
  ggplot(aes(x = date, y = new_elo, colour = team, group = paste(season, team))) +
  geom_line(size = 0.2) +
  theme(legend.position = "right") +
  labs(x = "", y = "Elo rating", colour = "",
       caption = the_caption) +
  ggtitle("Elo ratings of AFL / VFL teams, 1897 to present",
          "Elo rating calculated by modified FIBS method. Gaps in lines are the inter-season breaks.")
dev.off()


# Elo ratings if we did them only for last year:
elos_2018 <- r %>%
  filter(date > as.Date("2018-01-01")) %>%
  afl_elos()

# Elo ratings if we did them only from 2007 onwards
elos_2007 <- r %>%
  filter(date > as.Date("2007-01-01")) %>%
  afl_elos()



CairoSVG("../img/0147-2018-only.svg", 10, 6)
# Plot of Elo ratings if done just from 2018
elos_2018 %>%
  mutate(team = fct_reorder(team, -new_elo, .fun = last)) %>%
  ggplot(aes(x = date, y = new_elo, colour = team)) +
  geom_line() +
  theme(legend.position = "right") +
  labs(x = "", y = "Elo rating", colour = "",
       caption = the_caption) +
  ggtitle("Elo ratings of AFL / VFL teams, 2018 season",
          "Elo rating calculated by modified FIBS method, ignoring pre-2018 performance.")
dev.off()
  

CairoSVG("../img/0147-2007-on.svg", 8, 6)
# Plot of Elo ratings if done from 2007
elos_2007 %>%
  mutate(team = fct_reorder(team, -new_elo, .fun = last)) %>%
  ggplot(aes(x = date, y = new_elo, colour = team, group = paste(team, season))) +
  geom_line() +
  theme(legend.position = "right") +
  labs(x = "", y = "Elo rating", colour = "",
       caption = the_caption) +
  ggtitle("Elo ratings of AFL / VFL teams, 2007 to present",
          "Elo rating calculated by modified FIBS method, ignoring pre-2007 performance.")
dev.off()

#------------not used-------------
# r18_all <- elos_all %>%
#   group_by(team) %>%
#   filter(date == max(date)) %>%
#   select(team, elo_based_on_1897 = new_elo)
# 
# r18_18 <- elos_2018 %>%
#   group_by(team) %>%
#   filter(date == max(date)) %>%
#   select(team, elo_based_on_2018 = new_elo)
# 
# r18_07 <- elos_2007 %>%
#   group_by(team) %>%
#   filter(date == max(date)) %>%
#   select(team, elo_based_on_2007 = new_elo)
# 
# r18_all <- r18_all %>%
#   inner_join(r18_18) %>%
#   inner_join(r18_07)
# 
# CairoSVG("../img/0147-compare-1897-2018.svg", 8, 6)
# # Compare end ratings based on two different starting points (1897 and 2018):
# set.seed(123)
# r18_all %>%
#   ggplot(aes(x = elo_based_on_1897, y = elo_based_on_2018, label = team)) +
#   geom_smooth(method = "lm") +
#   geom_point() +
#   geom_text_repel(colour = "brown") +
#   labs(x = "Rating based on performance since 1897",
#        y = "Rating based just on 2018 performance",
#        caption = the_caption) +
#   coord_equal()
# dev.off()
# 
# CairoSVG("../img/0147-compare-1897-2007.svg", 8, 6)
# # Compare end ratings based on two different starting points (1897 and 2007):
# set.seed(123)
# r18_all %>%
#   ggplot(aes(x = elo_based_on_1897, y = elo_based_on_2007, label = team)) +
#   geom_smooth(method = "lm") +
#   geom_point() +
#   geom_text_repel() +
#   labs(x = "Rating based on performance since 1897",
#        y = "Rating based on performance since 2007",
#        caption = the_caption) +
#   coord_equal()
# dev.off()
# 
#----------------is it best just to use recent Elo, or all history---------

# Elo ratings if we did them only for 2017
elos_2017_final <-  r %>%
  filter(year(date) == 2017) %>%
  afl_elos() %>%
  group_by(team) %>%
  arrange(desc(date)) %>%
  slice(1) %>%
  select(team, elo_2017 = new_elo) %>%
  ungroup()

# based on all history up to 2017 (already calculated this so can just filter to get it)
elos_2017_and_earlier <-  elos_all %>%
  filter(year(date) == 2017) %>%
  group_by(team) %>%
  arrange(desc(date)) %>%
  slice(1) %>%
  select(team, elo_up_to_2017 = new_elo) %>%
  ungroup() 

elos_2018_final <- elos_2018 %>%
  group_by(team) %>%
  arrange(desc(date)) %>%
  slice(1) %>%
  select(team, elo_2018 = new_elo) %>%
  ungroup() 

elos_two_types <- elos_2017_final %>%
  left_join(elos_2017_and_earlier, by = "team") %>%
  inner_join(elos_2018_final, by = "team")

CairoSVG("../img/0147-starting-points.svg", 8, 5)
elos_two_types %>%
  gather(variable, value, -team, -elo_2018) %>%
  ggplot(aes(x = value, y = elo_2018, label = team)) +
  geom_smooth(method = "gam") +
  geom_point() +
  facet_wrap(~variable, scales = "free_x") +
  geom_text_repel() +
  labs(x = "Elo rating before the 2018 season",
       y = "Elo rating at end of 2018",
       caption = the_caption) +
  ggtitle("Elo ratings at end of 2018 compared to two alternative starting points",
          "Horizontal axis for panel on left shows rating based on just 2017; on right is based on 1897 to 2017")
dev.off()

mod <- lm(elo_2018 ~ elo_up_to_2017 + elo_2017, data = elos_two_types)
summary(mod)
anova(mod)

# this suggests that Elo ratings have too long a memory; it is better 
# to shrink them towards 1500 in between each season

#---------------success over time-----------------

elos_2017_onwards <- r %>%
  filter(year(date) >= 2017) %>%
  afl_elos() %>%
  group_by(season) %>%
  summarise(successful_predictions = mean(predicted_winner == winner)) %>%
  mutate(method = "2017")

CairoSVG("../img/0147-success-rates.svg", 8, 4)
elos_all %>%
  group_by(season) %>%
  summarise(successful_predictions = mean(successful_prediction)) %>%
  mutate(method = "1897") %>%
  rbind(elos_2017_onwards) %>%
  ggplot(aes(x = season, y = successful_predictions, colour = method)) +
  geom_line() +
  labs(x = "Season", colour = "Prediction based on performance since:") +
  scale_y_continuous("Prediction success", label = percent_format(accuracy = 1)) +
  ggtitle("Success rate of predicted AFL outcomes from Elo rating based on full history")
dev.off()

#---------------current ratings--------------------------
# This provides round 1 tips for 2019
# The differ from the popular choice on two:
# Hawthorn predicted by me to beat Adelaide Crows, although tips are 88:12 other direction
# North Melbourne predicted by me to beat Fremantle, although tips are 62:38 in other direction
elos_2018 %>%
  group_by(team) %>%
  arrange(desc(date)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(new_elo)) %>%
  select(team, new_elo) %>%
  knitr::kable() %>%
  clipr::write_clip()

convert_pngs("0147")

#--------------model-based-solution---------------
library(mgcv)
d <- elos_all %>%
  group_by(game) %>%
  summarise(home_elo = starting_elo[location == "home"],
         away_elo = starting_elo[location == "away"],
         home_winner = winner[location == "home"],
         date = unique(date))

model <- gam(home_winner ~ s(home_elo, away_elo), data = d)
coef(model)
plot(model, scheme = 2)

predict(model, newdata = data.frame(home_elo = 1523, away_elo = 1508))

elos_all %>%
  filter(winner) %>%
  group_by(season) %>%
  summarise(prop_home_winners = mean(winner * (location == "home"))) %>%
  ggplot(aes(x = season, y = prop_home_winners)) +
  geom_line()

elo_prob(1508, 1523, 5)

# Fremantle raw chance, averaged with their chance given being the home team
(elo_prob(1425, 1523, 32) + 0.59) / 2

mean(abs(r$margin))
