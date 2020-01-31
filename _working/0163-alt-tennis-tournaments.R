yr)

n_players <- 64

players <- tibble(
  player_id = 1:n_players,
  wins = 0,
  superiors = 0,
  elo = rnorm(n_players, 1500, 200)
)

direct_superiors <- tibble(
  player_id = integer(),
  superior_id = integer()
)

all_win_counts <- unique(players$wins)

for(this_win_count in all_win_counts){
  

  these_players <- filter(players, wins == this_win_count) %>%
    sample_n(n())
  
  number_matches <- nrow(these_players) / 2
  
  a <- these_players[1:number_matches, ] %>%
    select(player_a = player_id,
           elo_a = elo)
  b <- these_players[(number_matches + 1):(number_matches * 2), ] %>%
    select(player_b = player_id,
           elo_b = elo)
  
  matchups <- cbind(a, b)
  
  for(i in 1:number_matches){
    if(matchups[i, ]$elo_a > matchups[i, ]$elo_b){
      players[players$player_id == matchups[i, "player_a"], "wins"] <-
        players[players$player_id == matchups[i, "player_a"], "wins"] + 1
      direct_superiors <- rbind(direct_superiors,
                                tibble(player_id = matchups[i, "player_a"],
                                       superior_id = matchups[i, "player_b"]))
    } else {
      players[players$player_id == matchups[i, "player_b"], "wins"] <-
        players[players$player_id == matchups[i, "player_b"], "wins"] + 1
      direct_superiors <- rbind(direct_superiors,
                         tibble(player_id = matchups[i, "player_b"],
                                superior_id = matchups[i, "player_a"]))
        }
  }
}

# end of round
direct_superiors <- distinct(direct_superiors)

# need a trick to count not just those who have beaten you, but people
# who have beaten those who beat you. Must be a common network problem!

dsc <- direct_superiors %>%
  group_by(player_id) %>%
  summarise(count = n())
