library(tidyverse)
library(scales)
library(expm)
library(knitr)

# TODO - this ignores the fact that you get another roll if you get a six

test_m <- function(M){
  probs <- apply(M, 1, sum) 
  if(sum(probs == 1) != nrow(M)){
    message(paste("rows: ", paste(which(probs !=1 ), collapse = ", ")))
    print(probs[probs != 1])
    stop("Some rows of transition probabilities don't sum to 1")
  }
}

n <- 10 * 10

# we have n + 1 different states (because position zero is "off the board"), each of which 
# has a probability of transitioning to another. To start with, w say they can transition to 
# n +1 + 5 (105 in the case of a 10 x 10 board, as if you roll 99 you go first 10 105)
M <- matrix(0, n + 1, n + 1 + 6)

# The row is the position at the start of the round; the column is the position at the end; 
# the cell value is the probability of transitioning from the row to the column

for(i in 1:6){
  diag(M[ , (i + 1):(i+1+n)]) <- 1 / 6
}

# add in in the "bounce back"s
for(i in 1:6){
  M[, n + 1 - i] <- M[, n + 1 - i] + M[ , n + 1  + i] # If you end up on square 101 (i=102), you go down to square 99 (i = 100)
}

M <- M[1:(n + 1), 1:(n + 1)]

M_orig <- M
test_m(M_orig)

# add in the snakes and ladders:
# see https://en.wikipedia.org/wiki/Snakes_and_Ladders 
# "In the original game the squares of virtue are: Faith (12), Reliability (51), Generosity (57), 
# Knowledge (76), and Asceticism (78). The squares of vice or evil are: Disobedience (41), 
# Vanity (44), Vulgarity (49), Theft (52), Lying (58), Drunkenness (62), Debt (69), Murder (73), 
# Rage (84), Greed (92), Pride (95), and Lust (99).[8]


# snakes_and_ladders <- data_frame(
#   starting = c(12, 51, 57, 76, 78, 41, 44, 49, 52, 58, 62, 69, 73, 84, 92, 95, 99),
#   ending = c( ),
#   type = ifelse(ending < starting, "Snake", "Ladder"),
#   issue = c("Faith", "Reliability", "Generosity", "Knowledge", "Asceticism", 
#             "Disobedience", "Vanity", "Vulgarity", "Theft", "Lying", "Drunkenness", 
#             "Debt", "Murder", "Rage", "Greed", "Pride", "Lust")
# ) %>%
#   mutate(id = 1:n())



# snakes_and_ladders <- data_frame(
#   starting = c(4,9,17,20,28,40,51,54,62,64,63,71,93,95,92),
#   ending = c(14,31,7,38,84,59,67,34,19,60,81,91,73,75,78),
#   type = ifelse(ending < starting, "Snake", "Ladder")
# ) %>%
#   mutate(id = 1:n())

# Milton Bradley version, from image on Wikipedia.  This is identical
# to the 10 snakes and 9 ladders listed in Althoen et al:
snakes_and_ladders <- data_frame(
  starting = c(1, 4, 9,  16, 21, 28, 36, 47, 49, 51, 56, 62, 64, 71, 80,  87, 93, 95, 98),
  ending = c(38, 14, 31, 6,  42, 84, 44, 26, 11, 67, 53, 19, 60, 91, 100, 24, 73, 75, 78),
  type = ifelse(ending < starting, "Snake", "Ladder")
) %>%
  mutate(id = 1:n())

M <- M_orig
  
if(length(starting) != length(ending)){
  stop("different number of starting and ending points for snakes and ladders")
  }

# For each snake/ladder
for(i in 1:(nrow(snakes_and_ladders))){
  sl_start_col <- snakes_and_ladders$starting[i] + 1
  sl_end_col <- snakes_and_ladders$ending[i] + 1
  
  # retrieve current probabilities of landing on the snake/ladder starting square:
  v <- M[ , sl_start_col]

  # which rows (ie jumping off positions at beginning of turn) have the positive transitions:
  ind <- which(v > 0)
  
  # set zero probabilities of counter resting and taking off from start of the snake/ladder:
  M[ind, sl_start_col] <- 0
  
  # Move all existing probabilities to the end of the snake/ladder:
  M[ind, sl_end_col] <- M[ind, sl_end_col] + v[ind]
  
  if(sum(is.na(M)) > 0){
    stop(paste("Some NAs were introduced at snake/ladder number", i))
  }
  test_m(M)
}

# If you get to row 100, you stay there:
M[n + 1, ] <- 0
M[n + 1, n + 1] <- 1
colnames(M) <- 0:n
rownames(M) <- 0:n



#------------visualise board-------------------
nrows <- sqrt(n)

board <- as.data.frame(M) %>%
  mutate(board_row = c(0, rep(1:nrows, each = nrows)),
         board_col = c(0, rep(1:nrows, nrows)),
         sequence = 0:n) %>%
  mutate(board_col = ifelse(board_row %% 2 == 0 & sequence != 0, 
                            11 - board_col, board_col)) %>%
  select(board_row : sequence)

sl_df <- snakes_and_ladders %>%
  left_join(board[ , c("sequence", "board_row", "board_col")], by = c("starting" = "sequence")) %>%
  left_join(board[ , c("sequence", "board_row", "board_col")], by = c("ending" = "sequence"))


svg("../img/0130-board.svg", 6, 7)
ggplot(board, aes(y = board_row, x = board_col)) +
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
dev.off()

#---play the game---------
current_state <- function(h, M){
  n <- nrow(M) - 1
  initial_state = c(1, rep(0, n))
  prob <- as.vector(initial_state %*% (M %^% h))
  square <- 0:n
  return(data.frame(square = square, prob = prob))
}

# state after 1 turns
current_state(2, M) %>%
  filter(prob > 0)


# state after 2 turns
current_state(2, M) %>%
  filter(prob > 0)


# state after 5 turns
current_state(5, M) %>%
  filter(prob > 0) %>%
  arrange(desc(prob)) %>%
  head()


# state after 20 turns
current_state(20, M) %>%
  filter(prob > 0) %>%
  arrange(desc(prob)) %>%
  head()


turns <- c(0:11 * 5)
distrib <- lapply(turns, function(i){cbind(current_state(i, M), turns = i)})
dist_df <- do.call("rbind", distrib)

ggplot(results_df, aes(x = square, weight = prob)) +
  facet_wrap(~turns, scales = "free_y") +
  geom_bar()


turns2 <- 1:500
prob_finished <- sapply(turns2, function(h){current_state(h, M)[101, "prob"]})

res_df <- data_frame(turns2, prob_not_finished = 1 - prob_finished)
ggplot(res_df, aes(x = turns2, y = prob_not_finished)) +
  geom_line() +
  scale_x_log10(breaks = c(1, 3, 10, 30, 100, 300)) +
  theme(panel.grid.minor.x = element_blank())
View(res_df)

sum(M[101 , -101])


turns3 <- c(0:300)
distrib3 <- lapply(turns3, function(i){cbind(current_state(i, M), turns = i)})
dist_df3 <- do.call("rbind", distrib3)

dist_df3 %>%
  as_tibble() %>%
  filter(square == 100) %>%
  mutate(prob_won = c(0, diff(prob))) %>%
  ggplot(aes(x = turns, y = prob_won)) +
  geom_line()


dist_df3 %>%
  as_tibble() %>%
  filter(square == 100) %>%
  mutate(prob_won = c(0, diff(prob)))  %>%
  arrange(desc(prob_won))

dist_df3 %>%
  as_tibble() %>%
  filter(square == 100) %>%
  mutate(prob_won = c(0, diff(prob)))  %>%
  summarise(expected_win_time = sum(prob_won * turns))
