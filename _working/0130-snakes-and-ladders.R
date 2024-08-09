library(tidyverse)
library(foreach)
library(doParallel)

# add in the snakes and ladders:
# see https://en.wikipedia.org/wiki/Snakes_and_Ladders 
# "In the original game the squares of virtue are: Faith (12), Reliability (51), Generosity (57), 
# Knowledge (76), and Asceticism (78). The squares of vice or evil are: Disobedience (41), 
# Vanity (44), Vulgarity (49), Theft (52), Lying (58), Drunkenness (62), Debt (69), Murder (73), 
# Rage (84), Greed (92), Pride (95), and Lust (99).[8]


# Milton Bradley version, from image on Wikipedia.  This is identical
# to the 10 snakes and 9 ladders listed in Althoen et al:
snakes_and_ladders <- tibble(
  starting = c(1, 4, 9,  16, 21, 28, 36, 47, 49, 51, 56, 62, 64, 71, 80,  87, 93, 95, 98),
  ending = c(38, 14, 31, 6,  42, 84, 44, 26, 11, 67, 53, 19, 60, 91, 100, 24, 73, 75, 78),
  type = ifelse(ending < starting, "Snake", "Ladder")
) %>%
  mutate(id = 1:n())

# the board is a vector of numbers that basically show where you end up

board <- c(1:100, 99:95)
board[snakes_and_ladders$starting] <- board[snakes_and_ladders$ending]


#------------visualise board-------------------
nrows <- 10
n <- 100

board_df <- tibble(id = 0:n) %>%
  mutate(board_row = c(0, rep(1:nrows, each = nrows)),
         board_col = c(0, rep(1:nrows, nrows)),
         sequence = 0:n) %>%
  mutate(board_col = ifelse(board_row %% 2 == 0 & sequence != 0, 
                            11 - board_col, board_col)) %>%
  select(board_row : sequence)

sl_df <- snakes_and_ladders %>%
  left_join(board_df[ , c("sequence", "board_row", "board_col")], by = c("starting" = "sequence")) %>%
  left_join(board_df[ , c("sequence", "board_row", "board_col")], by = c("ending" = "sequence"))


p1 <- ggplot(board_df, aes(y = board_row, x = board_col)) +
  geom_tile(colour = "white", fill = "steelblue") +
  
  geom_curve(data = sl_df, aes(y = board_row.x, x = board_col.x,
                                 yend = board_row.y, xend = board_col.y,
                               colour = type),
               arrow = arrow(type = "closed", length = unit(0.1, "inches")),
               curvature = 0.2, size = 2) +
  geom_text(aes(label = sequence)) +
  scale_colour_manual(values = c(Ladder = "white", Snake = "orange")) +
  theme_void(base_family = "Roboto")  +
  theme(legend.position = "none", 
        plot.title = element_text(family = heading_font) ) +
  coord_equal() +
  ggtitle("Snakes and ladders board")

svg_png(p1, "../img/0130-s-and-l-board", w = 7, h = 6)

#---play the game---------

sl_game <- function(start = 0, end = 100){
  if(!start %in% c(0, board)){
    return(NULL)
  }
  positions <- position <- start
  turns <- 0
  while(position != 100){
    turns <- turns + 1
    dice <- sample(1:6, 1)
    position <- board[position + dice]
    positions <- c(positions,position)
    
    if(dice == 6 & position != end){
      dice <- sample(1:6, 1)
      position <- board[position + dice]
      positions <- c(positions,position)
    }
    
    if(dice == 6 & position != end){
      dice <- sample(1:6, 1)
      if(dice == 6){
        position <- 1
      } else{
        position <- board[position + dice]
      }
      positions <- c(positions,position)
    }
  }
  
  return(list(positions = positions, rolls = length(positions), turns = turns))
}

sl_game(0)
# should be NULL as it's not possible to finish a turn on 1 so can't start there either:
sl_game(1)
sl_game(2)

# simulate 10000 games starting from square zero
set.seed(123)
sims_0 <- lapply(rep(0, 10000), sl_game)

# make an animation out of 100 of them
dir.create("0130-tmp")

for(i in 1:100){
  this_sim <- data_frame(square = sims_0[[i]]$positions) |>
    left_join(board_df, by = c("square" = "sequence"))
  
  p <- ggplot(board_df, aes(y = board_row, x = board_col)) +
    geom_tile(colour = "white", fill = "steelblue") +
    
    geom_curve(data = sl_df, aes(y = board_row.x, x = board_col.x,
                                 yend = board_row.y, xend = board_col.y,
                                 colour = type),
               arrow = arrow(type = "closed", length = unit(0.1, "inches")),
               curvature = 0.2, size = 2) +
    geom_text(aes(label = sequence)) +
    
    geom_path(data = this_sim, colour = "red", size = 2, arrow = arrow()) +
    
    scale_colour_manual(values = c(Ladder = "white", Snake = "orange")) +
    
    theme_void(base_family = "Roboto")  +
    theme(legend.position = "none", 
          plot.title = element_text(family = heading_font) ) +
    coord_equal() +
    ggtitle("Simulated snakes and ladders games")
  
  png(paste0("0130-tmp/", 1000000 + i, ".png"), 6 * 200, 6 * 200, res = 200)
    print(p)
  dev.off()
}

pd <- setwd("0130-tmp")
system('magick -loop 0 -delay 40 *.png "0130-snakes-and-ladders.gif"')
setwd(pd)

number_turns <- tibble(turns = sapply(sims_0, function(x){x$turns}),
                       rolls = sapply(sims_0, function(x){x$rolls}))

number_turns |>
  gather(variable, value) |>
  group_by(variable) |>
  summarise(mean(value),
            median(value))

ggplot(number_turns, aes(x= turns)) +
  geom_density(fill = "blue", alpha = 0.2) +
  labs(x = "Number of moves before winning",
       title = "Distribution of length of one player snakes and ladders games",
       subtitle = "6 gets you another role, three 6s is back to square 1.
'Bounce back' rule applies when trying to finish exactly on 100")


#-----------------chance of winning from different positions------------
# set up parallel processing cluster
cluster <- makeCluster(7) # only any good if you have at least 7 processors :)
registerDoParallel(cluster)

clusterEvalQ(cluster, {
  library(foreach)
  library(tidyverse)
})


# how many times to play the game from each starting position
reps <- 1e4

# export onto each cluster some objects we need to use:
clusterExport(cluster, c("board", "sl_game", "reps"))

# define the starting positions, which should only be valid ones:
starts <- 0:99
starts <- starts[starts %in% c(0, board)]

# simulate all the games from the different starting positions
# takes a while to run even with the parallel processing
results <- foreach(start = starts, .combine = rbind) %dopar% {
  these_results <- tibble(start = numeric(), rep =numeric(), turns = numeric())
  
  for(rep in 1:reps){
    this_game <- sl_game(start)
    res <- tibble(start = start, rep = rep, turns = this_game$turns)
    these_results <- rbind(these_results, res)
  }
  return(these_results)
}
dim(results)

# average time to win from each starting position
results |>
  group_by(start) |>
  summarise(avg_turns = mean(turns))

# distribution of results - count of number of turns it takes to win
# from each starting position
distrib <- results |>
  count(start, turns) |>
  group_by(start) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>
  mutate(link = 1) |>
  select(start, turns, prop, link)

# for each combination of starting positions and number of turns to win,
# who wins with one player at one position and another player at another
# (caution this makes a very big object because of the full join)
who_wins <- distrib |>
  left_join(distrib, by = "link", 
            suffix = c("_p1", "_p2"), 
            relationship = "many-to-many") |>
  mutate(prob = prop_p1 * prop_p2) |>
  group_by(start_p1, start_p2) |>
  summarise(p1 = sum(prob[turns_p1 <= turns_p2]),
            p2 = sum(prob[turns_p1 > turns_p2])) |>
  mutate(unusual = p1 > 0.55 & start_p2 > start_p1)
  
# Visualise the chance of winning from various positions
who_wins |>
  ggplot(aes(x = start_p1, y = start_p2, fill = p1)) +
  geom_tile() +
  geom_tile(data = filter(who_wins, unusual), fill = "white", colour = "black") +
  scale_fill_gradientn(colours = c("red", "white", "blue")) +
  labs(x = "Player one current square",
       y = "Player 2 current square",
       fill = "Probability of player one winning",
       subtitle = "Player 1 has the dice. 6 gets you another roll. Three 6s is back to square 1. 
'Bounce back' rule applies if you don't land exactly on square 100.
Black squares indicate situations where it is worth betting on Player 1 even though they are behind",
       title = "Chance of winning a standard snakes and ladders game at different positions")


# some examples where it would be worth putting money on player 1 even
# though they are behind
who_wins |>
  filter(unusual) |>
  arrange(desc(p1))

