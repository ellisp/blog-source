
library(tidyverse)

sim_handedness <- function(nr = 100, ns = 50, ni = 5){

  # nr is number of runs
  # ns is number of subjects
  # ni is number of items

  ng <- 2    # number of groups, only works if it is 2

  cutoff1 <- (ni + 1 ) /2


  subjects <- expand_grid(
    run = 1:nr,
    group = LETTERS[1:ng],
    subject = 1:ns 
  ) |> 
    mutate(pr = sample(c(0.2, 0.9), n(), replace = TRUE, prob = c(0.2, 0.8)),
          used_right = rbinom(n(), size = ni, prob = pr)) |> 
    # apply the various arbitrary cut-off points
    mutate(range1 = if_else(used_right %in% cutoff1:ni, "Right-handed", "Not right-handed"),
          range2 = if_else(used_right %in% c(0, ni), "Consistent", "Inconsistent"),
          range3 = if_else(used_right %in% c(0:1, (ni-1):ni), "Strong", "weak"),
          range4 = if_else(used_right %in% ni, "Very-right", "Not very right-handed"),
          range5 = if_else(used_right %in% 0, "Very-left", "Not very left-handed"),
          range6 = if_else(used_right %in% (ni-1:ni), "mostly-right", "Not mostly-right"),
          range7 = if_else(used_right %in% (0:1), "mostly-left", "Not mostly-left"))


  # check overall proportions right
  # subjects |>
  #  summarise(apparently_biased_left = mean(range1 == "Not right-handed"),
  #            sim_biased_left = mean(pr == 0.2)) 
  
  methods <- paste0("range", 1:7)
  method_full_names <- c("right v not right", "consistent v inconsistent",
                            "strong v weak", "very-right v not", 
                             "very-left v not", "mostly-right v not",
                            "mostly-left v not"  )

  results <- matrix(0, nrow = nr, ncol = length(methods))

  for(r in 1:nr){
    m <- list()

    s <- subjects |> 
      filter(run == r)

    for(i in 1:length(methods)){
      s2 <- s
      names(s2)[names(s2) == methods[i]] <- "this_method"

      m[[i]] <- s2 |> 
        count(group, this_method) |> 
        spread(this_method, n, fill = 0) 
    }

    results[r, ] <- sapply(m, function(x){
      
      test_m <- x |> 
        select(-group) |> 
        as.matrix() 
      
      ft <- test_m |> 
          fisher.test(alternative = "two.sided")
    
      pv <- as.numeric(ft$p.value)
    
      return(pv)
    })

    


    
  }

  most_sig <- apply(results, 1, min)
  which_method <- unlist(apply(results == most_sig, 1, function(x){which(x)[1]}))

  return(list(subjects = subjects, 
              results = cbind(results, most_sig),
              prop_sig_05 = mean(most_sig < 0.05),
              prop_sig_01 = mean(most_sig < 0.01), 
              which_sig = which(most_sig < 0.05),
              best_method = tibble(best_method = method_full_names[which_method],
                                   run = 1:nr,
                                  which_method = which_method)))
}

sims <- sim_handedness(nr = 100)
sims
sims$subjects |> 
  filter(run %in% sims$which_sig) |> 
  count(run, group, used_right) |> 
  spread(used_right, n, fill = 0) |> 
  left_join(sims$best_method, by ="run")
