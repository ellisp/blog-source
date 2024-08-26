
# probabilities of sampling with unequal probability with replacement aren't what
# you thought!
# https://notstatschat.rbind.io/2024/08/26/another-way-to-not-sample-with-replacement/

# see also 
# https://stats.stackexchange.com/questions/110178/brewers-method-for-sampling-with-unequal-probabilities-with-n2

library(tidyverse)
library(glue)

compare_ppswor <- function(n =10,
                           N = 20,
                           replace = FALSE,
                           reps = 1e5){

  x <- paste0("unit", 1:N)
  x <- factor(x, levels = x)
  w <- 1:N / sum(1:N)
  
  
  samples <- lapply(rep(n, reps), function(size){
    sample(x, size = size, prob = w, replace = replace)
    })
  
  s <- ifelse(replace, 'with replacement', 'without replacement')
  
  p <- tibble(
    original = w,
    selected = as.numeric(table(unlist(samples))) / (reps * n)
  )  |>
    ggplot(aes(x = original, y = selected)) +
    geom_abline(slope = 1 , intercept = 0, colour = "orange") +
    geom_point(colour = "steelblue") +
    labs(x = "Original probability",
         y = "Actual probability of selection",
         subtitle = glue("Population of {N}, sample size {n}, sampling {s}."),
         title = "Use of `sample()` with unequal probabilities of sampling") +
    coord_equal() 
  
  return(p)
}

compare_ppswor(replace = TRUE)
compare_ppswor()
compare_ppswor(N = 50, replace = FALSE)
compare_ppswor(N = 250, replace = FALSE)
