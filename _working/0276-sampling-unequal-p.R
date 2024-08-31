
# probabilities of sampling with unequal probability with replacement aren't what
# you thought!
# https://notstatschat.rbind.io/2024/08/26/another-way-to-not-sample-with-replacement/

# see also 
# https://stats.stackexchange.com/questions/110178/brewers-method-for-sampling-with-unequal-probabilities-with-n2

library(tidyverse)
library(glue)
library(GGally)
library(sampling)

compare_ppswor <- function(n = 10,
                           N = 20,
                           replace = FALSE,
                           reps = 1e5,
                           prob = 1:N / sum(1:N),
                           FUN = sample){

  x <- paste0("unit", 1:N)
  x <- factor(x, levels = x)
  prob <- prob / sum(prob)
  
  
  samples <- lapply(rep(n, reps), function(size){
    FUN(x, size = size, prob = prob, replace = replace)
    })
  
  s <- ifelse(replace, 'with replacement', 'without replacement')
  m <- case_when(identical(FUN, sample) ~ "R's native `sample()` function.", 
                 identical(FUN, sample_unequal) ~ "experimental function based on Brewer (1975).",
                 identical(FUN, sample_brewer) ~ "Brewer (1975) as implemented by Tillé/Matei.")
  
  p <- tibble(
    original = prob,
    selected = as.numeric(table(unlist(samples))) / (reps * n)
  )  |>
    ggplot(aes(x = original, y = selected)) +
    geom_abline(slope = 1 , intercept = 0, colour = "orange") +
    geom_point(colour = "steelblue") +
    labs(x = "Original 'probability' or weight",
         y = "Actual proportion of selections",
         subtitle = glue("Population of {N}, sample size {n}, sampling {s}.\nUsing {m}"),
         title = "Use of `sample()` with unequal probabilities of sampling") +
    coord_equal() 
  
  return(p)
}

p1 <- compare_ppswor(replace = TRUE)
svg_png(p1, "../img/0276-20-10-replace")

p2 <- compare_ppswor()
svg_png(p2, "../img/0276-20-10-no-replace")

p3 <- compare_ppswor(N = 50, replace = FALSE)
p4 <- compare_ppswor(N = 250, replace = FALSE)

svg_png(p3, "../img/0276-50-10-no-replace")
svg_png(p4, "../img/0276-250-10-no-replace")

p5 <- compare_ppswor(N = 20, n = 20, replace = FALSE)
svg_png(p5, "../img/0276-20-20-no-replace")

#---------Brewer method-------------
# Taken from the answer by StasK at
# https://stats.stackexchange.com/questions/110178/brewers-method-for-sampling-with-unequal-probabilities-with-n2



#' @param p probabilities of remaining units
#' @param n total sample size
#' @param k which sequence of the sample this is for the sampling of
P <- function(p, n, k){
  r <- n - k + 1
  D <- sum((p * (1 - p)) / (1 - r * p))
  new_p <- p * (1 - p) / (D * (1 - r * p))
  return(new_p)
}

sample_unequal <- function(x, size, prob, replace = FALSE, keep = FALSE){

  if(size > length(x)){
    stop("Sample size cannot be larger than the population of units")
  }
  
  if(replace){
    stop("Only sampling without replacement implemented at this point")
  }
  
  if(size > (1 / max(prob))){
    stop(glue("Sample size ({size}) cannot be larger than 1 / max(prob) (which is {1 / max(prob)})"))
  }
  
  d <- tibble(x, prob)
  
  the_sample <- x[NULL]
  remnants <- x
  remnant_p <- prob
  
  for(k in 1:size){
    new_p <- P(remnant_p, size, k)
    
    if(keep){
      d2 <- tibble(x = remnants, prob = new_p)
      names(d2)[2] <- paste0("k", k)
      d <- d |>
        left_join(d2, by = "x")
    }
    
    if(min(new_p) < 0){
      warning("Some negative probabilities returned")
      new_p <- pmax(0, new_p)
    }
    
    latest_sample <- sample(remnants, size = 1, prob = new_p)
    which_chosen <- which(remnants == latest_sample)
    remnants <- remnants[-which_chosen]
    remnant_p <- remnant_p[-which_chosen]
    the_sample <- c(the_sample, latest_sample)
    
      }
  
  if(keep){
    return(list(the_sample = the_sample, d = d)) 
  } else {
    return(the_sample)
  }
}  


compare_ppswor(FUN = sample, reps = 1000)
# there is still some systematic bias in the new method, but it is much better:
set.seed(123)
p6 <- compare_ppswor(FUN = sample_unequal, reps = 1000)
svg_png(p6, "../img/0276-20-10-no-replace-brewer")

p7 <- compare_ppswor(n = 5, FUN = sample_unequal, reps = 10000)
svg_png(p7, "../img/0276-20-5-no-replace-brewer")

stop()

#--------------using sampling library------------------
library(sampling)
sample_brewer <- function(x, size, prob, replace = FALSE, keep = FALSE){
 pik <- prob / sum(prob) * size
 s <- UPbrewer(pik)
 the_sample <- x[which(s == 1)]
 return(the_sample)
 }

p8 <- compare_ppswor(FUN = sample_brewer, reps = 10000)
svg_png(p8, "../img/0276-20-10-brewer-better")
#------------------extra not for blog--------------
compare_ppswor(n = 10, FUN = sample_unequal, reps = 1000)

# doesn't work when sample gets more than 50% of N in our default case
compare_ppswor(n = 11, FUN = sample_unequal, reps = 1000)

# with other probabilities with less variance it's ok
compare_ppswor(n = 11, FUN = sample_unequal, reps = 1000, prob = runif(20, 1, 10))
compare_ppswor(n = 13, FUN = sample_unequal, reps = 1000, prob = runif(20, 1, 10))
compare_ppswor(n = 14, FUN = sample_unequal, reps = 1000, prob = runif(20, 1, 10))

# as StasK says in the comments on cross-validated, if 1-rp < 0
# For us that happens when sample is more thna 50% but that's
# just because of how our p is defined
# there is a significant problem

#----------------------closer look at an example---------------
N = 20
x <- paste0("unit", 1:N)
x <- factor(x, levels = x)
p <- 1:N / sum(1:N)
stopifnot(round(sum(P(p, 10, 1)), 2) == 1)

y <- sample_unequal(x, 10, p, keep = TRUE)

apply(y$d[,-1], 2, sum, na.rm = TRUE)



compare_ppswor(FUN = sample_unequal, reps = 1000)

compare_ppswor(FUN = sample, reps = 10000)
# there is still some systematic bias in the new method, but it is much better:
compare_ppswor(FUN = sample_unequal, reps = 10000)


y$d[ , -1] |>
  ggpairs()


par(mfrow = c(2,2), bty = "l", pch = 19, family = "Calibri", 
    col = "steelblue", font.main = 1)

plot(p, P(p, 1, 1), main = "Sample size = 1", xlab = ""); abline(0, 1)
plot(p, P(p, 5, 1), main = "Sample size = 5", xlab = ""); abline(0, 1)
plot(p, P(p, 10, 1), main = "Sample size = 10"); abline(0, 1)
plot(p, P(p, 20, 1), main = "Sample size = 20"); abline(0, 1)



P(p, 10, 1)
P(p, 11, 1)

#-------------larger population and sample-----

N = 2000
x <- paste0("unit", 1:N)
x <- factor(x, levels = x)
p <- 1:N / sum(1:N)

# works ok
sample_unequal(x, size = 1000, p, keep = FALSE)

# doesn';t because this is when the 1-rp < 0
sample_unequal(x, size = 1001, p, keep = FALSE)

# with less variation in the weights
set.seed(123)
p2 <- runif(N, 1, 5)
p2 <- p2 / sum(p2)
sample_unequal(x, size = 1001, p2, keep = FALSE)
1/max(p2) 
sample_unequal(x, size = 1150, p2, keep = FALSE)
sample_unequal(x, size = 1200, p2, keep = FALSE)


