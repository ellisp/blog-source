library(foreach)
library(doParallel)
library(tidyverse)
library(scales)
library(kableExtra)
library(clipr)

# deuce package by Stephanie Kovalchik to get Elo ratings of real tennis players
# devtools::install_github("skoval/deuce")
library(deuce)
data(wta_elo)

max(wta_elo$tourney_start_date) # 2018-09-17

# Identify the top 128 women playing in 1990 and their last Elo rating that year
women128 <- wta_elo %>%
  filter(year(tourney_start_date) == 1990) %>%
  # pick just the latest Elo rating
  group_by(player_name) %>%
  arrange(desc(tourney_start_date)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(overall_elo)) %>%
  dplyr::select(player_id, player_name, elo = overall_elo) %>%
  slice(1:128)


# Chance of beating Steffi Graf
steffis_opponents <- c(2:5, 10, 20, 50, 128)

y <- elo_prediction(women128[steffis_opponents, ]$elo, women128[1, ]$elo)

the_caption <- "Source: http://freerangestats.info analysis based on 1990 WTA ratings in Stephanie Kovalchik's {deuce} R package."

p1 <- cbind(women128[steffis_opponents, ], y, rank = steffis_opponents) %>%
  mutate(lab = paste0(player_name, " (", rank, ")")) %>%
  ggplot(aes(x = elo, y = y, label = lab)) +
  geom_point() +
  geom_text_repel(size = 3, colour = "steelblue") +
  scale_y_continuous(label = percent) +
  labs(title = "Probability of beating Steffi Graf at the end of 1990",
       subtitle = "Various players from the top 128 WTA players, probability based only on Elo rating",
       caption = the_caption,
       x = "Elo rating of Ms Graf's hypothetical opponent (ranking shown in brackets)",
       y = "Ms Graf's opponent's probability of winning")

svg_png(p1, "../img/0165-playing-graf")


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
      
      prob_win <- deuce::elo_prediction(these_players[1, ]$elo, these_players[2, ]$elo)
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

n_sims <- 10000

#-----------set up parallel processing-----------

cluster <- makeCluster(7) # only any good if you have at least 7 processors :)
registerDoParallel(cluster)

clusterEvalQ(cluster, {
  library(foreach)
  library(tidyverse)
  library(deuce)
})

clusterExport(cluster, c("women128", "assign_rounds", "simulate_tournament"))


#-------------------unseeded tournament-------------------
r1 <- foreach(this_sim = 1:n_sims, .combine = rbind) %dopar% {
  
  # new random positionining of all 128 players
  brackets <- women128 %>%
    sample_n(128) %>%
    select(player_id) %>%
    assign_rounds() %>%
    left_join(women128, by = "player_id")
  
  one_sim_results <- simulate_tournament(brackets)
  
  one_sim_results %>%
    mutate(sim = this_sim,
           method = "No seeding")
}

#------------------------seeded tournament-----------

# https://en.wikipedia.org/wiki/Template:32TeamBracket
round_of_32_seeding <- tibble(
  position32 = rep(1:16, each = 2),
  seed = c(1, 32, 17, 16, 9, 24, 25, 8, 5, 28, 21, 12, 13, 20, 29, 4,
           3, 30, 19, 14, 11, 22, 27, 6, 7, 26, 23, 10, 15, 18, 31, 2)
) 
stopifnot(all(1:32 %in% round_of_32_seeding$seed))

# players and their position in the round of 32 bracket - doesn't change in each sim
players <- women128 %>%
  arrange(desc(elo)) %>%
  mutate(ranking = 1:128,
         seed = ifelse(ranking <= 32, ranking, NA)) %>%
  left_join(round_of_32_seeding, by = "seed")

clusterExport(cluster, "players")


r2 <- foreach(this_sim = 1:n_sims, .combine = rbind) %dopar% {
  unseeded <- players[-(1:32), ] %>%
    # in random order:
    sample_n(n()) %>%
    mutate(position32 = 1:n() %% 16 + 1) %>%
    arrange(position32)
  
  # actual brackets change each sim for players who aren't one of the top 32 seeds
  brackets <- rbind(filter(players[, c("player_id", "position32")], !is.na(position32)), 
        unseeded[, c("player_id", "position32")]) %>%
    arrange(position32) %>%
    assign_rounds() %>%
    left_join(select(players, -position32), by = "player_id") %>%
    arrange(desc(round), match)
  
  
  one_sim_results <- simulate_tournament(brackets)
  
  one_sim_results %>%
    mutate(sim = this_sim,
           method = "Seeded (32 seeds)") 
}

results <- rbind(r1, r2) %>%
  left_join(players, by = c("winner" = "player_id")) %>%
  rename(winner_name = player_name)

# Summarise the individual winners of the overall tournament:
results %>%
  filter(round == 2) %>%
  group_by(winner_name, method, ranking) %>%
  summarise(wins = n()) %>%
  ungroup() %>%
  select(winner_name, method, wins, `Actual ranking` = ranking) %>%
  complete(winner_name, method, fill = list(wins = 0)) %>%
  pivot_wider(names_from = method, values_from = wins) %>%
  arrange(desc(`Seeded (32 seeds)`)) %>%
  slice(1:10) %>%
  kable() %>%
  kable_styling(full_width = FALSE) %>%
  write_clip()

#' Present the success rate of different simulated draw methods
#' 
#' @param results data frame of results as created earlier in this script
#' @param women128 data frame with extra details on the 128 players in the tourname
#' @param how many of the top players you want to compare the draw methods for. Must be a power of 2. 
#' @details Very specific to this particular bit of analysis
success_rate <- function(results, women128, top_x){
 
  if(log(top_x, 2) != as.integer(log(top_x, 2))){
    stop("top_x must be a power of 2")
  }
  
  top_players <- women128[1:top_x, ]$player_name
    
  # we're going to count this by counting the winners in the previous round. eg to find if the top 2
  # players are the final 2 in the competition, we see if they are in the winners from the round of 4:
  tmp <- results %>%
    filter(round == (top_x * 2)) %>%
    arrange(sim, method, match) %>%
    group_by(sim, method) %>%
    summarise(best = all(winner_name %in% top_players)) %>%
    group_by(method) %>%
    summarise(best = mean(best)) %>%
    mutate(top_x = top_x)

  return(tmp)

}

p <- rbind(
  success_rate(results, women128, top_x = 8),
  success_rate(results, women128, top_x = 4),
  success_rate(results, women128, top_x = 2),
  success_rate(results, women128, top_x = 1)
) %>%
  ggplot(aes(x = as.ordered(top_x), y = best, colour = method)) +
  geom_point() +
  geom_line(aes(x = as.numeric(as.ordered(top_x)))) +
  scale_y_continuous(label = percent) +
  expand_limits(y = 0) +
  labs(x = "Number of top players of interest",
       y = "Chance of those players being the\nlast ones in the competition",
       colour = "Method of allocating draw:",
       title = "Impact of seeding on the better players' end results in a tennis tournament",
       subtitle = "Seeding materially improves the chance of the best player winning or the best two players being in the 
grand final,and impacts the semi- and quarter- finals even more.",
       caption = the_caption)
p
svg_png(p, "../img/0165-tennis-seeding")
