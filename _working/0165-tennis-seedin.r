library(tidyverse)

# deuce package by @skoval to get Elo ratings of real tennis players
# My fork of Stephanie Kovalchik package of tennis data and analytical functions
# devtools::install_github("ellisp/deuce")
library(deuce)
data(wta_elo)

head(wta_elo)
max(wta_elo$tourney_start_date) # 2018-09-17


women128 <- wta_elo %>%
  # pick just the latest Elo rating
  group_by(player_name) %>%
  arrange(desc(tourney_start_date)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(overall_elo)) %>%
  select(player_id, player_name, latest_elo_date = tourney_start_date, elo = overall_elo) %>%
  slice(1:128)
  

#' @source https://www.betfair.com.au/hub/tennis-elo-modelling/
#' 
#' @examples 
#' # should be 24%
#' pwin(1800, 2000)
pwin <- function(a, b){
  1 / (1 + 10 ^ ((b - a) / 400))
}

#' Function to simulate the result of a tournament of 128 players
#' 
#' @param brackets a data frame or tibble with columns for player_id (player id), round (referring to
#' the number of players left in that round of the tournament - 2,4,8,16,32,64 and 128),
#' match (match id within that round)
simulate_tournament <- function(brackets){
  one_sim_results <- tibble(winner = numeric(), 
                            loser = numeric(),
                            round = numeric(),
                            match = numeric())
  
  all_rounds <- unique(brackets$round)
  
  remaining_players <- brackets
  
  filter(brackets, round == 32)
  
  for(this_round in all_rounds){
    
    all_matchups <- remaining_players %>%
      filter(round == this_round) %>%
      pull(match) %>%
      unique()
    
    for(this_match in all_matchups){
      these_players <- remaining_players %>%
        filter(round == this_round) %>%
        filter(match == this_match) %>%
        select(player_id, elo)
      
      prob_win <- pwin(these_players[1, ]$elo, these_players[2, ]$elo)
      a_wins <- rbinom(1, 1, prob = prob_win) == 1
      if(a_wins){
        update <- tibble(winner = these_players[1, ]$player_id, 
                         loser = these_players[2, ]$player_id,
                         round = this_round,
                         match = this_match)
      } else {
        update <- tibble(winner = these_players[2, ]$player_id, 
                         loser = these_players[1, ]$player_id,
                         round = this_round,
                         match = this_match)
      }
      one_sim_results <- rbind(one_sim_results, update)
    }
    remaining_players <- brackets %>%
      filter(!player_id %in% one_sim_results$loser)
    
  }
  return(one_sim_results)
}


#' Assign bracket slots to 128 players
#'
#' @param d a data frame or tibble with 128 rows and one column player_id
#' @return a long data frame with columns for player_id, round and match indicating which
#' match each player is in in each round (if they survive to that round). "round" is indicated
#' by the convention of number of players left ie "Round of 128" is the first round, "Round of 2"
#' is the grand final.
#' @details The original order is retained so this can be used (with caution) to allocate
#' matches to a seeded tournament. For example, with 32 seeds if the original data frame
#' is in the order of position32 from the round_of_32_seeding object, with each position
#' followed by 7 unseeded players.
assign_rounds <- function(d){
  d %>%
    mutate(position128 = rep(1:64, each = 2),
         position64 = rep(1:32, each = 4),
         position32 = rep(1:16, each = 8),
         position16 = rep(1:8, each = 16),
         position8 = rep(1:4, each = 32),
         position4 = rep(1:2, each = 64),
         position2 = rep(1:1, each = 128)) %>%
    gather(round, match, -player_id) %>%
    mutate(round = as.numeric(gsub("position", "", round)))
    
}

results <- tibble(winner = numeric(), 
                  loser = numeric(),
                  round = numeric(),
                  match = numeric(),
                  sim = numeric(),
                  method = character())

n_sims <- 1000
set.seed(123)

#-------------------unseeded tournament-------------------
for(this_sim in 1:n_sims){
  
  brackets <- women128 %>%
    sample_n(128) %>%
    select(player_id) %>%
    assign_rounds() %>%
    left_join(women128, by = "player_id")
  
  one_sim_results <- simulate_tournament(brackets)
  
  results <- one_sim_results %>%
    mutate(sim = this_sim,
           method = "No seeding") %>%
    rbind(results)
}

#------------------------seeded tournament-----------

# https://en.wikipedia.org/wiki/Template:32TeamBracket
round_of_32_seeding <- tibble(
  position32 = rep(1:16, each = 2),
  seed = c(1, 32, 17, 16, 9, 24, 25, 8, 5, 28, 21, 12, 13, 20, 29, 4,
           3, 30, 19, 14, 11, 22, 27, 6, 7, 26, 23, 10, 15, 18, 31, 2)
) 
stopifnot(all(1:32 %in% round_of_32_seeding$seed))

players <- women128 %>%
  arrange(desc(elo)) %>%
  mutate(ranking = 1:128,
         seed = ifelse(ranking <= 32, ranking, NA)) %>%
  left_join(round_of_32_seeding, by = "seed")




for(this_sim in 1:n_sims){
  unseeded <- players[-(1:32), ] %>%
    # in random order:
    sample_n(n()) %>%
    mutate(position32 = 1:n() %% 16 + 1) %>%
    arrange(position32)
  
  brackets <- rbind(filter(players[, c("player_id", "position32")], !is.na(position32)), 
        unseeded[, c("player_id", "position32")]) %>%
    arrange(position32) %>%
    assign_rounds() %>%
    left_join(select(players, -position32), by = "player_id") %>%
    arrange(desc(round), match)
  
  
  one_sim_results <- simulate_tournament(brackets)
  
  results <- one_sim_results %>%
    mutate(sim = this_sim,
           method = "Seeded (32 seeds)") %>%
    rbind(results)
}

results %>%
  filter(round == 2) %>%
  group_by(winner, method) %>%
  summarise(wins = n()) %>%
  left_join(players, by = c("winner" = "player_id")) %>%
  ungroup() %>%
  select(player_name, method, wins) %>%
  complete(player_name, method, fill = list(wins = 0)) %>%
  pivot_wider(names_from = method, values_from = wins) %>%
  arrange(desc(`Seeded (32 seeds)`))
