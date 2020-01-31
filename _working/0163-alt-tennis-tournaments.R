devtools::install_github("ellisp/deuce")

3library(tidyverse)
library(deuce)

n_players <- 128

players <- tibble(
  player_id = str_pad(1:n_players, 4, side = "left", pad = "0"),
  matches = 0,
  wins = 0,
  superiors = 0,
  elo = rnorm(n_players, 1500, 200)
)

match_wins <- tibble(
  winner_id = character(),
  loser_id = character(),
  combo = character()
)

remaining_players <- players %>%
  filter(superiors < 3) %>%
  select(player_id, matches, elo) %>%
  mutate(rnd = runif(n())) %>%
  arrange(rnd)

candidate_players <- remaining_players %>%
  filter(matches <= min(matches))

rpi <- candidate_players$player_id

the_match <- expand_grid(p1 = rpi, p2 = rpi) %>%
  # limit to those with id 1 less than id 2 - just for sorting purposes
  filter(p1 < p2) %>%
  mutate(combo = paste(p1, p2)) %>%
  # restrict to people who haven't played eachother:
  anti_join(match_wins, by = "combo") %>%
  sample_n(1)





# idea - instead of thinking in rounds, simply keep cycling through
# until there are no more winners. Pick a game at random from people
# who haven't played excessive games, add it to the results

nm <- nrow(remaining_players) / 2

tibble(
  p1 = remaining_players[1:nm, ]$player_id,
  p2 = remaining_players[(nm + 1):(nm * 2), ]$player_id
  ) %>%
  mutate(q1 = ifelse(p1 < p2, p1, p2),
         q2 = ifelse(p1 < p2, p2, p1))
