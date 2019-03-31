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
  arrange(game, desc(winner)) %>%
  ungroup() %>%
  mutate(season = year(date)) %>%
  group_by(season, team) %>%
  mutate(round = 1:n()) %>%
  ungroup()


#----------------home/away adjustments---------------------
d <- r %>%
  group_by(team) %>%
  summarise(start = min(date),
            end = max(date),
            home_wins = mean(winner[location == "home" & date > "1997-01-01"]),
            away_wins = mean(winner[location == "away" & date > "1997-01-01"]),
            home_adv = (home_wins - away_wins) / 2) %>%
  arrange(desc(home_adv)) %>%
  filter(end > "2000-01-01")

adjustments <- d %>%
  mutate(home =  home_adv / 2,
         away = -home_adv / 2) %>%
  select(team, home, away) %>%
  gather(location, location_adjustment, -team)

r2 <- r %>%
  left_join(adjustments, by = c("team", "location")) %>%
  mutate(location_adjustment = replace_na(location_adjustment, 0),
         predicted_winner = NA) 

#----------------Elo rating------------

#---------------Function to determine ratings for one particular set of parameters-----------------

#' Calculate Elo ratings for a historical set of AFL results
#' 
#' @param r a data frame with columns including date, team, game, starting_elo, new_elo
#' @param sc scaling factor for dividing the winning margin by to convert it to "match length" 
#' for Elo calculation purposes (FIBS method requires a match length)
#' @param pred_margin the prediction margin in points to use as match length for prediction purposes.
#' Note that this gets divided by sc so it is on the same scale as the historical margins.
#' @param margin_power parameter that scaled prediction margin is put to the power of in the match length
#' calculation. 0 will mean all match lengths are 1 (so sc and pred_margin make no difference)
#' @param experience how much experience (sum of previous match length) teams are presumed to have, for the
#' FIBS-style Elo rating. Numbers below 400 make the rating adjustments more responsive to recent results.
#' @param new_round_factor how much to shrink Elo ratings towards 1500 in the first match of each season.
#' 1 means no shrinkage, 0 means every team starts the season fresh with a rating of 1500 regardless of 
#' past performance.
afl_elos <- function(r, sc = 1, pred_margin = 30, margin_power = 1, experience = 100, new_round_factor = 1){
  r <- r %>%
    group_by(game) %>%
    arrange(date, game, desc(winner))
  
  all_games <- unique(r$game)
  
  # This seems inherently iterative so perhaps a loop is the logical way to do it:
  for(g in all_games){
    
    this_game <- r[r$game == g, ] %>%
      mutate(starting_elo = ifelse(round == 1, 
                                   (starting_elo - 1500) * new_round_factor + 1500,
                                   starting_elo))
    
    # er: calculate elo rating arising from this game to use for changes in ratings
    er <- elo_rating(a = this_game[1, "starting_elo"], 
                     b = this_game[2, "starting_elo"],
                     winner = "a",
                     ml = (round(this_game[1, "margin"] / sc) ^ margin_power),
                     axp = experience,
                     bxp = experience,
                     a_adv = this_game[1, "location_adjustment"],
                     b_adv = this_game[2, "location_adjustment"])
    
    # er: calculate elo rating arising from this game to use for the prediction, for use in
    # measureing prediction success
    er2 <- elo_rating(a = this_game[1, "starting_elo"], 
                      b = this_game[2, "starting_elo"],
                      winner = "a",
                      ml = (round(pred_margin / sc) ^ margin_power),
                      axp = experience,
                      bxp = experience,
                      a_adv = this_game[1, "location_adjustment"],
                      b_adv = this_game[2, "location_adjustment"])
    
    
    r[r$game == g, "new_elo"]  <- unlist(c(er$a, er$b))
    r[r$game == g, "predicted_winner"] <- c(er2$winproba >= 0.5, er2$winproba < 0.5)
    r <- r %>%
      group_by(team) %>%
      mutate(starting_elo = lag(new_elo, default = 1500)) %>%
      ungroup()
  }
  r <- r %>%
    mutate(season = lubridate::year(date)) %>%
    group_by(game) %>%
    mutate(successful_prediction = predicted_winner == winner) %>%
    ungroup()
  
  return(r)
}

best_params <- data.frame(
  sc = 1,
  pred_margin = 30,
  margin_power = 0.8333,
  experience = 100,
  new_round_factor = 0.4
)


elos_best <- r2 %>%
  afl_elos(sc               = best_params$sc,
           pred_margin      = best_params$pred_margin,
           margin_power     = best_params$margin_power,
           experience       = best_params$experience,
           new_round_factor = best_params$new_round_factor)





#==================Predictions for this round!=====================
elos_latest <- elos_best %>%
  group_by(team) %>%
  arrange(desc(date)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(new_elo)) %>%
  select(team, elo = new_elo)

fixture <- tibble(
  home = c("Richmond", "Sydney", "Essendon", "Port Adelaide", "Geelong", "West Coast",
           "North Melbourne", "Hawthorn", "Gold Coast"),
  away = c("Collingwood", "Adelaide", "St Kilda", "Carlton", "Melbourne", "GWS", "Brisbane Lions", 
           "Footscray", "Fremantle")
)

fixture %>%
  left_join(elos_latest, by = c("home" = "team")) %>%
  rename(home_elo = elo) %>%
  left_join(elos_latest, by = c("away" = "team")) %>%
  rename(away_elo = elo)  %>%
  left_join(filter(adjustments, location == "home"), by = c("home" = "team")) %>%
  rename(home_adjustment = location_adjustment) %>%
  select(-location)  %>%
  left_join(filter(adjustments, location == "away"), by = c("away" = "team")) %>%
  rename(away_adjustment = location_adjustment) %>%
  select(-location) %>%
  mutate(final_prob = elo_prob(home_elo, away_elo, 
                               ml = (best_params$pred_margin / best_params$sc) ^ best_params$margin_power, 
                               a_adv = home_adjustment, 
                               b_adv = away_adjustment),
         winner = ifelse(final_prob > 0.5, home, away),
         fair_returns_home = 1/final_prob,
         fair_returns_away = 1/ (1- final_prob))