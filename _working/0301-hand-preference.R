# Original article is at https://statmodeling.stat.columbia.edu/wp-content/uploads/2024/07/bishop1990.pdf


library(tidyverse)

sim_handedness <- function(nr = 100, ns = 50, ni = 5, 
                           tail_method = c("most.sig", "two.sided")){
  
  tail_method <- match.arg(tail_method)

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
          range4 = if_else(used_right %in% ni, "Pure-right", "Not pur right-handed"),
          range5 = if_else(used_right %in% 0, "Pure-left", "Not pure left-handed"),
          range6 = if_else(used_right %in% (ni-1:ni), "Mostly-right", "Not mostly-right"),
          range7 = if_else(used_right %in% (0:1), "Mostly-left", "Not mostly-left"))


  # check overall proportions right
  # subjects |>
  #  summarise(apparently_biased_left = mean(range1 == "Not right-handed"),
  #            sim_biased_left = mean(pr == 0.2)) 
  
  methods <- paste0("range", 1:7)
  method_full_names <- c("rightish v not rightish", "consistent v inconsistent",
                            "strong v weak", "Pure-right v not", 
                             "Pure-left v not", "mostly-right v not",
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
      

      if(all(nrow(test_m) == 2, ncol(test_m) == 2)){
        # see https://online.stat.psu.edu/stat504/lesson/4/4.5
        # to think about why we are choosing the best of less
        # or greater,... an alternative might be to half the two.sided
        # option, is that always the same as doing less or greater and 
        # choosing the lowest?

        if(tail_method == "most.sig"){

          ft1 <- fisher.test(test_m, alternative = "greater")$p.value
          ft2 <- fisher.test(test_m, alternative = "less")$p.value

          pv <- min(ft1, ft2)
        } else {
          pv <- fisher.test(test_m, alternative = "two.sided")$p.value
        }
      } else {
        pv <- NA
      }
    
      return(pv)
    })

  }

  most_sig <- apply(results, 1, min, na.rm = TRUE)
  which_method <- unlist(apply(results == most_sig, 1, 
    function(x){which(x)[1]}))

  return(list(subjects = subjects, 
              results = cbind(results, most_sig, which_method),
              prop_sig_05 = mean(most_sig < 0.05, na.rm = TRUE),
              prop_sig_01 = mean(most_sig < 0.01, na.rm = TRUE), 
              which_sig = which(most_sig < 0.05),
              best_method = tibble(best_method = method_full_names[which_method],
                                   run = 1:nr,
                                  which_method = which_method)))
}

set.seed(123)
sims <- sim_handedness(nr = 1000, ni = 5)
sims$prop_sig_05
sims$prop_sig_01

sims$subjects |> 
  filter(run %in% sims$which_sig) |> 
  count(run, group, used_right) |> 
  spread(used_right, n, fill = 0) |> 
  left_join(sims$best_method, by ="run")

set.seed(123)
sims_2s <- sim_handedness(nr = 1000, ni = 5, tail_method = "two.sided")
sims_2s$prop_sig_05
sims_2s$prop_sig_01


set.seed(123)
sims_9i <- sim_handedness(nr = 1000, ni = 9)
sims_9i$prop_sig_05
sims_9i$prop_sig_01

sims$_9isubjects |> 
  filter(run %in% sims$which_sig) |> 
  count(run, group, used_right) |> 
  spread(used_right, n, fill = 0) |> 
  left_join(sims$best_method, by ="run")


many_results <- sapply(1:100, function(x){
  sim_handedness(nr = 100, ni = 5)$prop_sig_05
})
