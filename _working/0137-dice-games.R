library(tidyverse)
library(scales)

n <- 1000000
dice <- sample(1:6, n, replace = TRUE)
wins <- which(dice == 6)

results <- data.frame(game_length = diff(c(0, wins))) %>%
  mutate(who_won = ifelse(game_length %% 2 == 1, "Player A", "Player B"))

rs <- results %>%
  group_by(who_won) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  mutate(prop = freq / sum(freq))
rs
svg("../img/0137-simple-results.svg", 8, 4)
ggplot(results, aes(x = game_length, fill = who_won)) +
  geom_histogram(binwidth = 1) +
  ggtitle("Results of an alternating dice roll game",
          paste0("First to roll a six wins; Starting player wins ", 
                 round(rs[1, "prop"], 2), " of the time")) +
  scale_y_continuous(label = comma) +
  labs(x = "Game length", fill = "Winner:", 
       y = paste("Number of wins out of", format(n, big.mark = ",", scientific = FALSE)))
dev.off()


#-----Game 2--------

#' Roll a dice and return the game 2 results for one round
#' 
#' @param last_roll the previous roll of the dice. If NA, this means we are at the beginning of the game
#' @return a list with two elements: whether it was a win based on the rule of 6, or matching the last roll; 
#' and what the latest roll of the dice is
diceroll <- function(last_roll){
  this_roll <- sample(1:6, 1)
  
  win <- this_roll %in% c(6, last_roll)
  return(list(
    win = win,
    this_roll = this_roll)
  )
}

#' Main cycle for playing "Game 2"
#' 
#' @return the number of rolls it took to win the game
dicegame <- function(){
  i <- 0
  rolls <- NA
  win <- FALSE
  while(!win){
    i <- i + 1
    dr <- diceroll(rolls[length(rolls)])
    win <- dr$win
    rolls <- c(rolls, dr$this_roll)
  }
  return(i)
}

game_length <- sapply(1:n, function(x){dicegame()})

results <- data.frame(game_length = game_length) %>%
  mutate(who_won = ifelse(game_length %% 2 == 1, "Player A", "Player B"))

results %>%
  group_by(who_won, game_length) %>%
  summarise(freq = n()) %>%
  arrange(game_length)

rs <- results %>%
  group_by(who_won) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  mutate(prop = freq / sum(freq))

rs

svg("../img/0137-game2-results.svg", 8, 4)
ggplot(results, aes(x = game_length, fill = who_won)) +
  geom_histogram(binwidth = 1) +
  ggtitle("Results of an alternating dice roll game",
          paste0("First to roll a six or to match the last roll wins; Starting player wins ", 
                 round(rs[1, "prop"], 2), " of the time")) +
  scale_y_continuous(label = comma) +
  labs(x = "Game length", fill = "Winner:", 
       y = paste("Number of wins out of", format(n, big.mark = ",", scientific = FALSE)))
dev.off()

convert_pngs("0137")
