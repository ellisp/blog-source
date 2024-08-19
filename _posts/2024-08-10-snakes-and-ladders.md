---
layout: post
title: Snakes and ladders
date: 2024-08-10
tag: 
   - Games
   - Animations
description: When playing Snakes and Ladders with the common rules actually used, it is more complex than a simple mathematical model; I simulate games and put forward some findings that could be useful in a future high stakes Snakes and Ladders game. 
image: /img/0130-snakes-and-ladders.gif
socialimage: https:/freerangestats.info/img/0130-snakes-and-ladders.gif
category: R
---

More than five years ago I started a [series of posts on games of chance](https://freerangestats.info/blog/2018/10/27/dice-games) and for one reason or another never got around to finishing them. Partially redressing that, here is a long-lost and now tidied up post on Snakes and Ladders. Read it before you next *gaze across the smoke filled casino table at your opponent, a sinister operative of SMERSH, and take a sip at your vodka martini while deciding whether to take his proferred wager or not with him on square 90 and you on 75...*

OK, maybe that scenario is unlikely. But one of my themes in these posts is that even games of pure chance will become skill if you gamble on them. Choices like whether to bet, what odds to offer or accept, and (in some games) whether to offer, accept or decline a doubling cube all turn even a pure chance game like [Persian Monarchs](/blog/2018/12/23/persian-monarchs) into one where the best player will win (in the long run).

First, a reminder of how [Snakes and Ladders](https://en.wikipedia.org/wiki/Snakes_and_ladders) works. Here is a stylised version of a common Snakes and Ladders board, the Milton Bradley 1952 "Chutes and Ladders" (American children are frightened of snakes, apparently) depicted on the Wikipedia page. I've used arrows rather than snakes or chutes and ladders, and of course omitted all the extra imagery, which while interesting for a historian of morality and games, isn't relevant to gameplay. I was actually in Sri Lanka back in 2018 when I started this Snakes and Ladders work, so had a particular interest in its origins, but I don't have time to explain that now.

<object type="image/svg+xml" data='/img/0130-s-and-l-board.svg' width='100%'><img src='/img/0130-s-and-l-board.png' width='100%'></object>

You start at a virtual square 0, roll a six-sided dice and move your counter that number of squares. If you end in a position with a ladder, you move up to where it finishes. So if your first roll is a 1,you move to square 1 and immediately climb the ladder to square 38. If you end on a snake, you move down to the tail of the snake.

Here's the R code to draw that board:

{% highlight R lineanchors %}
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

ggplot(board_df, aes(y = board_row, x = board_col)) +
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
{% endhighlight %}

Now, the Wikipedia page claims that Snakes and Ladders can be represented exactly as an absorbing Markov chain, because the transition probability from each square to any other square is fixed and easy to define, and not dependent on the path the counter took to that square. However, this model is only a useful one if you omit the common rule that rolling a six gets you another roll; and rolling three sixes sends you "back to square one" (in fact, Snakes and Ladders is the origin of this common English phrase).

I want a realistic simulation of the game as actually played, so after mucking around with Markov chains for a while realised that it was going to be much simpler to write code that mimics how the players go about it. That's what I used to simulate 10,000 solo games to create this animation of a hundred Snakes and Ladders games:

<img src='/img/0130-snakes-and-ladders.gif' width = '90%'>

The code for which is below. All the heavy work is done by the `sl_game()` function, which takes as an argument the current position of the player (defaults to zero) - we'll be using this more later.

{% highlight R lineanchors %}
#---play the game---------

sl_game <- function(start = 0, end = 100){
  if(!start %in% c(0, board)){
    return(NULL)
  }
  positions <- position <- start
  turns <- 0
  while(position != end){
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

# knit the images together into an animation
pd <- setwd("0130-tmp")
system('magick -loop 0 -delay 40 *.png "0130-snakes-and-ladders.gif"')
setwd(pd)

# a bit of analysis of the number of turns it takes to win:
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

{% endhighlight %}

Here's the output from that bit of analysis of the number of turns and rolls (remembering you can get more rolls than turns, if you get sixes)

```
  variable `mean(value)` `median(value)`
  <chr>            <dbl>           <dbl>
1 rolls             42.7              34
2 turns             35.0              28
```

It's a bit more complicated than the mean 39.2 dice throws that can be analytically calculated as the average number of rolls needed under the simplified version modelled with a Markov chain. The expected number of rolls is higher because of 'back to square one'; the number of turns is fewer because of the bonus rolls you get when you have a six. 

The distribution of the number of rolls required is very much squewed with a long tail - it is in fact possible for a game to go on forever (although extremely improbable):

<object type="image/svg+xml" data='/img/0130-lengths.svg' width='100%'><img src='/img/0130-lengths.png' width='100%'></object>

OK, so where does the gambling come in? Gambling becomes interesting when there is a choice about the *timing* or *odds* of a wager. If all gambling was an even odds stake put down before the game, then Snakes and Ladders would stay a game of pure chance. But if it is possible, say at the beginning of your turn, to size up the board and say "I'll bet you on even odds that I'll win", you're making a choice based on your knowledge of the game. A naive observer might think that anyone who is at a more advanced square is more likely to win, and therefore could be trapped into taking a bet that would only be fair if there were favourable odds given to them.

To explore this with Snakes and Ladders, I simulated ten thousand solo games with the counter starting from each square that is a valid starting position for a turn (e.g. excluding squares that are at the bottom of a ladder - you can't end a turn on that square because if you land there you would go straight up the ladder). This gives us a probability distribution for how many turns it is expected to take to win from that point. If we do a full join of this distribution to itself, we will get a joint probability for how many turns it will take player 1 to win from that position and every combination of player 2's starting points.

We can visualise the result in a chart like this one. The highlight boxes are where Player 1, who is about to roll, has a surprisingly good chance of winning (higher than 0.55), despite being behind Player 2 in the race. These are the probable opportunities to offer a 50:50 bet to your opponent; only an unusually disciplined or knowledgeable player would think they were losing in this position.

<object type="image/svg+xml" data='/img/0130-unusual-winning-chances.svg' width='100%'><img src='/img/0130-unusual-winning-chances.png' width='100%'></object>.

Here are some of those positions:

```
   start_p1 start_p2    p1    p2 unusual how_surprising
      <int>    <int> <dbl> <dbl> <lgl>            <dbl>
 1       65       81 0.611 0.389 TRUE             0.761
 2       66       81 0.607 0.393 TRUE             0.746
 3       65       82 0.590 0.410 TRUE             0.744
 4       67       81 0.615 0.385 TRUE             0.743
 5       68       81 0.622 0.378 TRUE             0.740
 6       69       81 0.626 0.374 TRUE             0.735
 7       63       81 0.570 0.430 TRUE             0.733
 8       22       29 0.555 0.445 TRUE             0.731
 9       74       81 0.667 0.333 TRUE             0.730
10       66       82 0.587 0.413 TRUE             0.729
```

The `how_surprising` column is a metric I made up that tries to incorporate both the high probability of Player 1 winning and how far they currently *seem* to be behind. So if you are Player 1 on square 65, and see your opponent on 81, now is the time to offer them a bet.

Intuitively, why are 81 and 82 bad squares? It's because you've missed the ladders from 80 straight to 100 (instant win) and from 71 to 90. You still have snakes at 93, 95 and 98 that might trip you up whereas your opponent might skip them altogether if they land on square 80 - which is a non-trivial 1/6 chance for them. 

Similarly, square 29 is a bad one because you've just missed the big ladder starting at square 28, whereas your apparently-behind opponent still has a chance to get on it.

Here's the code for running those simulations and drawing the chart:

{% highlight R lineanchors %}
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
       subtitle = "Rules: 6 gets you another roll. Three 6s is back to square 1. 'Bounce back' rule applies if you don't land
exactly on square 100. Player 1 has the dice.
Black squares indicate situations where it is worth betting on Player 1 even though they are behind",
       title = "Chance of winning a standard snakes and ladders game at different positions")

# some examples where it would be worth putting money on player 1 even
# though they are behind
who_wins |>
  filter(unusual) |>
  mutate(how_surprising = start_p2 / start_p1 * p1) |>
  arrange(desc(how_surprising))
{% endhighlight %}

Finally, because of my backgammon interests, I wondered about the use of a doubling cube. In backgammon, at the beginning of your turn (before throwing the dice), you have the option of offering your opponent the doubling cube. They can accept, in which case the game is now being played for twice as many points / stakes; or refuse in which case they lose the game instantly at the current stake. Once they have accepted the cube, only they can offer it again (presumably if the fortunes change their way).

Generally speaking - putting aside some backgammon-specific complications - it makes sense to accept the doubling cube if you have a 1 in 4 or better chance of winning. If you have a 0.75 probability of winning and have the cube available, you should definitely offer the double; and your opponent should refuse. Now, I've never heard of Snakes and Ladders with a doubling cube but actually I am sure it has happened (or will happen in the future). So it's worth highlighting what are the points at which Player 2 should decline the cube if offered and accept the loss at the current stake? This chart answers this for us (although probably a table would be more useful for actual use):

<object type="image/svg+xml" data='/img/0130-doubling-cube.svg' width='100%'><img src='/img/0130-doubling-cube.png' width='100%'></object>

Produced with this little snipped of extra code:

{% highlight R lineanchors %}
who_wins |>
  ggplot(aes(x = start_p1, y = start_p2, fill = p1)) +
  geom_tile() +
  geom_tile(data = filter(who_wins, p2 < 0.25), fill = "white", colour = "black") +
  scale_fill_gradientn(colours = c("red", "white", "blue")) +
  labs(x = "Player one current square",
       y = "Player 2 current square",
       fill = "Probability of player one winning",
       subtitle = "Rules: 6 gets you another roll. Three 6s is back to square 1. 'Bounce back' rule applies if you don't land
exactly on square 100. Player 1 has the dice.
Black squares indicate situations where Player 2 should decline an offered doubling cube",
       title = "Chance of winning a standard snakes and ladders game at different positions")
{% endhighlight %}

OK folks, that's all. Remember to gamble responsibly. In particular, if you use this post to rip off any kids, their sadness will be on your conscience, and I absolutely disclaim all responsibility for any misuse of any kind of any of the above material.