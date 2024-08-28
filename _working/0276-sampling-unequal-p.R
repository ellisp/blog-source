
# probabilities of sampling with unequal probability with replacement aren't what
# you thought!
# https://notstatschat.rbind.io/2024/08/26/another-way-to-not-sample-with-replacement/

# see also 
# https://stats.stackexchange.com/questions/110178/brewers-method-for-sampling-with-unequal-probabilities-with-n2

library(tidyverse)
library(glue)

# TODO - plot should say which sampling method used.

compare_ppswor <- function(n =10,
                           N = 20,
                           replace = FALSE,
                           reps = 1e5,
                           FUN = sample){

  x <- paste0("unit", 1:N)
  x <- factor(x, levels = x)
  w <- 1:N / sum(1:N)
  
  
  samples <- lapply(rep(n, reps), function(size){
    FUN(x, size = size, prob = w, replace = replace)
    })
  
  s <- ifelse(replace, 'with replacement', 'without replacement')
  m <- ifelse(identical(FUN, sample), "R's native `sample()` function.", "experimental function based on Brewer (1975).")
  
  p <- tibble(
    original = w,
    selected = as.numeric(table(unlist(samples))) / (reps * n)
  )  |>
    ggplot(aes(x = original, y = selected)) +
    geom_abline(slope = 1 , intercept = 0, colour = "orange") +
    geom_point(colour = "steelblue") +
    labs(x = "Original probability",
         y = "Actual probability of selection",
         subtitle = glue("Population of {N}, sample size {n}, sampling {s}.\nUsing {m}"),
         title = "Use of `sample()` with unequal probabilities of sampling") +
    coord_equal() 
  
  return(p)
}

compare_ppswor(replace = TRUE)
compare_ppswor()
compare_ppswor(N = 50, replace = FALSE)
compare_ppswor(N = 250, replace = FALSE)

#---------Brewer method-------------
# Taken from the answer by StasK at
# https://stats.stackexchange.com/questions/110178/brewers-method-for-sampling-with-unequal-probabilities-with-n2



#' @param p probabilities of remaining units
#' @param n total sample size
#' @param k which unit this is for the sampling of
P <- function(p, n, k){
  r <- n - k + 1
  D <- sum((p * (1 - p)) / (1 - r * p))
  new_p <- p * (1 - p) / (D * (1 - r * p))
  return(new_p)
}

sample_unequal <- function(x, size, prob, replace = FALSE){

  if(size > length(x)){
    stop("Sample size cannot be larger than the population of units")
  }
  
  if(replace){
    stop("Only sampling without replacement implemented at this point")
  }
  
  the_sample <- x[NULL]
  remnants <- x
  remnant_p <- prob
  
  for(k in 1:size){
    latest_sample <- sample(remnants, size = 1, prob = P(remnant_p, size, k))
    which_chosen <- which(remnants == latest_sample)
    remnants <- remnants[-which_chosen]
    remnant_p <- remnant_p[-which_chosen]
    the_sample <- c(the_sample, latest_sample)
  }
  
  return(the_sample)
}  


N = 20
x <- paste0("unit", 1:N)
x <- factor(x, levels = x)
p <- 1:N / sum(1:N)
stopifnot(round(sum(P(p, 10, 1)), 2) == 1)


sample_unequal(x, 10, p)

compare_ppswor(FUN = sample_unequal, reps = 1000)

compare_ppswor(FUN = sample, reps = 10000)
# there is still some systematic bias in the new method, but it is much better:
compare_ppswor(FUN = sample_unequal, reps = 10000)
