library(tidyverse)
library(scales)
library(parallel)

#' Define a starting full pack of cards. In sequence as per Bridge ie Ace of Spades highest, Two of Clubs lowest.
pack <- data_frame(
  id = 1:52,
  suit = rep(c("clubs", "diamonds", "hearts", "spades"), each = 13),
  value = rep(c(2:10, "Jack", "Queen", "King", "Ace"), 4)
) %>%
  mutate(label = paste(value, "of", suit))

full_pack <- pack

#' For a given pack (not necessarily a full one), pull out one card, and return
#' that card and the remaining pack (as a list with two elements)
#' 
#' @param pack a dataframe with such as that defined by `pack`, with at least
#' a column named `id`
pm_draw <- function(pack){
  drawn <- sample_n(pack, size = 1)
  remaining_pack <- pack[pack$id != drawn$id, ]
  return(list(drawn = drawn, remaining_pack = remaining_pack))
}


#' Play a hand of Persian Monarchs
#' 
#' @param p1_counters number of counters owned by player 1 (non-dealer) at beginning of hand
#' @param p2_counters number of coutners owned by player 2 (dealer) at beginning of hand
#' @known_pack pack at the beginning of the hand, 'known' by the players if they have been counting cards.
#' @p1_method method for player 1 deciding whether to offer an increased wager
#' @p2_method method for player 2 deciding whether to accept an offered increased wager
#' @verbose Whether or not to print out extra information to console
#' @p1_lab Label for player 1, used in messages to console
#' @p2_lab Label for player 2, used in messages to console
#' @max_wager Maximum additional wager to be offered by player 1
#' @details Player 2 is the dealer and Player 1 gets the first choice - whether or not to offer an increased wager
#' on top of the original wager of 1 counter, and if so how much to make that additional wager. Player 2 then gets
#' to accept and cover that wager, of to fold and concede the current wager of 1 counter to Player 1.
pm_hand <- function(p1_counters, p2_counters, known_pack,
                    p1_method = c("auto", "ask", "random", "non-card-counter"), 
                    p2_method = c("auto", "ask", "random", "non-card-counter"),
                    verbose = FALSE,
                    p1_lab = "Non-dealer",
                    p2_lab = "Dealer",
                    max_wager = 10){
  
  # convenience function for printing a message to the console but substituting the correct label
  # for p1 or p2 (player 1 or player 2):
  pm_cat <- function(txt){
    txt <- gsub("p1", p1_lab, txt)
    txt <- gsub("p2", p2_lab, txt)
    if(verbose){
      cat(txt)
    }
    
  }
  
  p1_method <- match.arg(p1_method)
  p2_method <- match.arg(p2_method)
  
  # Both players start by putting in one counter each just to start to play
  p1_counters <- p1_counters - 1
  p2_counters <- p2_counters - 1
  wagered <- 2
  
  # Draw a card each
  tmp1 <- pm_draw(known_pack)
  tmp2 <- pm_draw(tmp1$remaining_pack)
  p1 <- tmp1$drawn
  p2 <- tmp2$drawn
  
  # The probabilities as they appear to the players:
  p1_chance = sum(known_pack$id < p1$id) / (nrow(known_pack) - 1)
  p2_chance = sum(known_pack$id < p2$id) / (nrow(known_pack) - 1)
  
  # Decision - will p1 wager more?
  if(p1_method %in% c("auto", "random", "non-card-counter")){
    # If p1 is being rational, it's chance of increasing the wager comes from it's appreciation
    # of the probability of winning this hand (more is 1 if increasing the wager, 0 otherwise).
    # But if the method is random, then they choose at random
    chance_more <- case_when(
      p1_method == "auto" ~ p1_chance, 
      p1_method == "random" ~ runif(1, 0, 1),
      p1_method == "non-card-counter" ~ sum(full_pack$id < p1$id) / 51
    )
    more <- rbinom(prob = chance_more, size = 1, n = 1)
    extra <- sample(1:max_wager, size = 1)
    if(more == 0){
      pm_cat("p1 does not add to the initial wager.\n")
    }
  } else {
    pm_cat(paste0("You drew the ", p1$label, ". How much extra will you bet this hand?\n(maximum is ", max_wager, 
                    ", 0 is acceptable)"))
      extra <- -1
      while(!extra %in% 0:max_wager){
        extra <- as.numeric(readLines(n = 1))
        
      }
      more <- as.integer(extra > 0)
     
  }  
  
  if(more == 1){
    
    # Decision - will p2 try to accept the increased wager?
    if(p2_method %in% c("auto", "non-card-counter")){
      p2_chance_enh <- case_when(
        p2_method == "auto" ~ p2_chance,
        p2_method == "non-card-counter" ~ sum(full_pack$id < p2$id) / 51
      )
      p2_expected <- p2_chance_enh * (wagered + extra * 2) - (1 - p2_chance_enh) * (wagered + extra * 2)
      accept = p2_expected > (-wagered)
    } else {
      if(p2_method == "ask"){
        pm_cat(paste0("You drew the ", p2$label, ". The other player raises the wager by ", extra, ". Do you accept? (Y/n)\n"))
        input <- ""
        while(!tolower(input) %in% c("y", "n")){
          input <- readLines(n = 1)
        }
        accept = (tolower(input) == "y")
      } else {
        # random acceptance:
        accept = as.logical(runif(1, 0 , 1) > 0.5)
      }
    }
    
    if(accept & p2_counters >= extra){
      # the bet is on:
      p1_counters <- p1_counters - extra
      p2_counters <- p2_counters - extra
      wagered <- wagered + extra * 2
      pm_cat("p2 accepts the added wager.\n")
    } else {
      # or else, p2 gives up:
      p1_counters <- p1_counters + wagered
      wagered <- 0
      if(verbose){
        pm_cat("p2 did not accept the added wager.\n")
      }
    }
    
  }
  
  # Who won?
  if(p1$id > p2$id){
    p1_counters <- p1_counters + wagered
    if(verbose){
      pm_cat(paste0(p1$label, " beats ", p2$label, "; p1 wins.\n"))
    }
    
  } else {
    p2_counters <- p2_counters + wagered
    if(verbose){
      pm_cat(paste0(p1$label, " loses to ", p2$label, "; p2 wins.\n"))
    }
    
  }
  
  known_pack <- tmp2$remaining_pack
  if(nrow(known_pack) < 2){
    pm_cat("Getting a fresh pack of cards.\n")
    known_pack <- pack
  }
  
  return(list(
    p1_counters = p1_counters,
    p2_counters = p2_counters,
    known_pack = known_pack))
}

#-----------------------Human versus computer---------------------
#' Play a human v computer game of Persian Monarchs
#' 
#' @param human number of chips human starts with
#' @param computer number of chips computer starts with
#' @param number of rounds to play (both players get a turn in a round)
persian_monarchs_hvc <- function(human = 100, computer = 100, rounds = 10){
  start_pack <- data_frame(
    id = 1:52,
    suit = rep(c("clubs", "diamonds", "hearts", "spades"), each = 13),
    value = rep(c(2:10, "Jack", "Queen", "King", "Ace"), 4)
  ) %>%
    mutate(label = paste(value, "of", suit))
  
  known_pack <- start_pack
  
  for(i in 1:rounds){

    # Computer deals:
    if(i != 1) {
      cat("Press enter for next hand.\n")
      readLines(n = 1)
    }
    
    
    message("Computer's deal.")
    cp <- pm_hand(human,
                  computer,
                  known_pack,
                  p1_method = "ask",
                  verbose = TRUE,
                  p1_lab = "Human",
                  p2_lab = "Computer")
    human <- cp$p1_counters
    computer <- cp$p2_counters
    known_pack <- cp$known_pack
    
    cat(paste0("You have ", human, " counters.\n"))
    cat(paste0("There are ", nrow(known_pack), " cards left in the pack.\n"))
    
    # Human deals:
    cat("Press enter for next hand.\n")
    readLines(n = 1)
    
    message("Your deal.")
    cp <- pm_hand(computer,
                  human,
                  known_pack,
                  p2_method = "ask",
                  verbose = TRUE,
                  p1_lab = "Computer",
                  p2_lab = "Human")
    
    human <- cp$p2_counters
    computer <- cp$p1_counters
    known_pack <- cp$known_pack
    
    cat(paste0("You have ", human, " counters.\n"))
    cat(paste0("There are ", nrow(known_pack), " cards left in the pack.\n"))
  }
}

# Uncomment next line and run it if you want to play an interactive game
# persian_monarchs_hvc(rounds = 3)

#-------------------------computer versus computer-------------------
#' Play a computer v computer game of Persian Monarchs
#' 
#' @param c1 number of chips Computer One starts with
#' @param c2 number of chips Computer Two starts with
#' @param c1_method one of "auto" (best choice given expected value) and "random" (coin flip) for
#' deciding whether Computer One offers or accepts an increased wager
#' @param c2_method one of "auto" (best choice given expected value) and "random" (coin flip) for
#' deciding whether Computer Two offers or accepts an increased wager
#' @param number of rounds to play (both players get a turn as dealer in a round, so a round is two hands)
#' @param verbose whether or not to print results to the screen
persian_monarchs_cvc <- function(c1 = 100, c2 = 100, 
                                 c1_method = c("auto", "random", "non-card-counter"),
                                 c2_method = c("auto", "random", "non-card-counter"), 
                                 rounds = 10, verbose = TRUE){
  
  c1_method <- match.arg(c1_method)
  c2_method <- match.arg(c2_method)
  
  start_pack <- data_frame(
    id = 1:52,
    suit = rep(c("clubs", "diamonds", "hearts", "spades"), each = 13),
    value = rep(c(2:10, "Jack", "Queen", "King", "Ace"), 4)
  ) %>%
    mutate(label = paste(value, "of", suit))
  
  known_pack <- start_pack
  
  for(i in 1:rounds){
    
    # c2 deals:
    if(verbose){message("Computer Two's deal.")}
    cp <- pm_hand(c1,
                  c2,
                  known_pack,
                  p1_method = c1_method,
                  p2_method = c2_method,
                  verbose = verbose,
                  p1_lab = "Computer One",
                  p2_lab = "Computer Two")
    c1 <- cp$p1_counters
    c2 <- cp$p2_counters
    known_pack <- cp$known_pack
    
    if(verbose){
      cat(paste0("Computer One has ", c1, " counters.\n"))
      cat(paste0("There are ", nrow(known_pack), " cards left in the pack.\n"))
    }    
    # c1 deals:
    if(verbose){
      message("Computer One's deal.")      
    }
    

    cp <- pm_hand(c2,
                  c1,
                  known_pack,
                  p1_method = c2_method,
                  p2_method = c1_method,
                  verbose = verbose,
                  p1_lab = "Computer Two",
                  p2_lab = "Computer One")
    
    c1 <- cp$p2_counters
    c2 <- cp$p1_counters
    known_pack <- cp$known_pack
  
    if(verbose){
      cat(paste0("Computer One has ", c1, " counters.\n"))
      cat(paste0("There are ", nrow(known_pack), " cards left in the pack.\n"))
    }
  }
  return(data_frame(c1 = c1, c2 = c2))
}

persian_monarchs_cvc(rounds = 3)
persian_monarchs_cvc(rounds = 26, c1_method = "auto", c2_method = "auto", verbose = FALSE)
persian_monarchs_cvc(rounds = 26, c1_method = "auto", c2_method = "random", verbose = FALSE)

#--------------------multiple simulations using parallel processing--------------------
# cluster with seven processors:
cl <- makeCluster(7)

# load the functionality we need onto the cluster:
clusterEvalQ(cl, {library(tidyverse)})
clusterExport(cl, c("persian_monarchs_cvc", "full_pack", "pack", "pm_hand", "pm_draw"))

# number of simulations to do in each run. Takes about 90 seconds per run to do 10,000.
n <- 10000

# Run 1 - with both players using the optimal strategy
results_auto <- parLapply(cl, 1:n, function(x){
  persian_monarchs_cvc(rounds = 26, c1_method = "auto", c2_method = "auto", verbose = FALSE)
  })
results_auto_df <- do.call("rbind", results_auto) %>%
  mutate(c2_method = "Best strategy")

# Run 2 - computer two plays ok but without counting cards left in the pack
results_ncc <- parLapply(cl, 1:n, function(x){
  persian_monarchs_cvc(rounds = 26, c1_method = "auto", c2_method = "non-card-counter", verbose = FALSE)
})
results_ncc_df <- do.call("rbind", results_ncc) %>%
  mutate(c2_method = "OK strategy but no card counting")

# Run 3 - computer two makes and accepts bets at random
results_random <- parLapply(cl, 1:n, function(x){
  persian_monarchs_cvc(rounds = 26, c1_method = "auto", c2_method = "random", verbose = FALSE)
})
results_random_df <- do.call("rbind", results_random) %>%
  mutate(c2_method = "Random")

stopCluster(cl)

results_combined <- results_auto_df %>%
  rbind(results_random_df) %>%
  rbind(results_ncc_df)

p <- results_combined  %>%
  mutate(c2_method = fct_reorder(c2_method, c2)) %>%
  ggplot(aes(x = c2, fill = c2_method, colour = c2_method)) +
  geom_density(alpha = 0.5) +
  #facet_wrap(~c2_method) +
  labs(fill = "Wagering decisions:",
       x = "Counters left at end of game",
       colour = "Wagering decisions:",
       y = paste("Density of results from", format(n, big.mark = ","), "simulations")) +
  ggtitle("Results of two automated wagering strategies for Persian Monarchs",
          "Counters left (having started with 100) after 26 rounds (two full packs) against a\nsound strategy")

CairoSVG("../img/0135-density.svg", 8, 5)
print(p)
dev.off()

mod <- lm(c2 ~ c2_method, data = results_combined)
confint(mod)

convert_pngs("0135")
