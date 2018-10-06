library(tidyverse)

pack <- data_frame(
  id = 1:52,
  suit = rep(c("clubs", "diamonds", "hearts", "spades"), each = 13),
  value = rep(c(2:10, "Jack", "Queen", "King", "Ace"), 4)
) %>%
  mutate(label = paste(value, "of", suit))



pm_draw <- function(pack){
  drawn <- sample_n(pack, size = 1)
  remaining_pack <- pack[pack$id != drawn$id, ]
  return(list(drawn = drawn, remaining_pack = remaining_pack))
}


#-------------Game cycle begins here-------------------



  
pm_hand <- function(p1_counters, p2_counters, known_pack,
                    p1_method = c("auto", "ask"), p2_method = c("auto", "ask"),
                    verbose = FALSE,
                    p1_lab = "Non-dealer",
                    p2_lab = "Dealer",
                    max_wager = 10){
  
  pm_cat <- function(txt){
    txt <- gsub("p1", p1_lab, txt)
    txt <- gsub("p2", p2_lab, txt)
    cat(txt)
    
  }
  
  p1_method <- match.arg(p1_method)
  p2_method <- match.arg(p2_method)
  
  p1_counters <- p1_counters - 1
  p2_counters <- p2_counters - 1
  wagered <- 2
  
  tmp1 <- pm_draw(known_pack)
  tmp2 <- pm_draw(tmp1$remaining_pack)
  
  p1 <- tmp1$drawn
  p2 <- tmp2$drawn
  
  # The probabilities as they appear to the players:
  p1_chance = sum(known_pack$id < p1$id) / (nrow(known_pack) - 1)
  p2_chance = sum(known_pack$id < p2$id) / (nrow(known_pack) - 1)
  
  # Decision - will p1 wager more?
  if(p1_method == "auto"){
    more <- rbinom(prob = p1_chance, size = 1, n = 1)
    extra <- sample(1:max_wager, size = 1)
    if(more == 0){
      pm_cat("p1 does not add to the initial wager.\n")
    }
  } else {
    pm_cat(paste0("You drew the ", p1$label, ". How much extra will you bet this hand?\n"))
    extra <- -1
    while(!extra %in% 0:max_wager){
      extra <- as.numeric(readLines(n = 1))
      
    }
    more <- as.integer(extra > 0)
  }  
  
  if(more == 1){
    
    # Decision - will p2 try to accept the increased wager?
    if(p2_method == "auto"){
      p2_expected <- p2_chance * (wagered + extra * 2) - (1 - p2_chance) * (wagered + extra * 2)
      accept = p2_expected > (-wagered)
    } else {
      pm_cat(paste0("You drew the ", p2$label, ". The other player raises the wager by ", extra, ". Do you accept? (Y/n)\n"))
      input <- ""
      while(!tolower(input) %in% c("y", "n")){
        input <- readLines(n = 1)
      }
      accept = (tolower(input) == "y")
      
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

persian_monarchs <- function(human = 100, computer = 100, rounds = 10){
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

persian_monarchs(rounds = 3)


