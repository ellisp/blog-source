library(tidyverse)
library(scales)
library(Rcpp)
library(microbenchmark)


#' Simulate filling of an urn with red and white balls, with serial correlation
#'
#' @param n Number of red and white balls in the urn
#' @param p overall hyper-population proportion of red balls
#' @param q probability that a ball, when originally placed in the urn in order, will be the same
#' colour as the preceding ball rather than randomly chosing between red and white with probability p
fill_urn_r <- function(n, p, q){
  x <- numeric(n)
  x[1] <- rbinom(1, 1, prob = p)
  
  for(i in 2:n){
    x[i] <- ifelse(rbinom(1, 1, prob = q) == 1, 
                   x[i-1],
                   rbinom(1, 1, prob = p))
  }
  return(x)
}

# C++ version, hopefully much faster:
cppFunction('NumericVector fill_urn_c(int n, double p, double q) {
  NumericVector x(n);
  x[0] = as<double>(rbinom(1, 1, p));
  for (int i=1; i<n; i++) {
  
   if(as<double>(rbinom(1, 1, q)) == 1){
    x[i] = x[i-1];
   } else {   
    x[i] = as<double>(rbinom(1, 1, p));
   }
  }
  
  return x;
}')

# Check the R and C++ versions give same results:
all.equal({set.seed(123); fill_urn_c(30, 0.3, 0.9)},
          {set.seed(123); fill_urn_r(30, 0.3, 0.9)})

# Compare speeds
microbenchmark("C++" = fill_urn_c(10000, 0.3, 0.9),
               "R" =   fill_urn_r(10000, 0.3, 0.9))
# Roughly a 40 times speed up

#------------------Comparing two sampling methods-----------

#' Compare two methods of sampling
#' 
#' @param x a vector of 1 and 0 for which we want to estimate the proportion of 1s
#' @return a data frame with one row and two columns, showing the difference between two 
#' methods of sampling and estimating a proportion
#' @details The "big data" method takes the most recent 80% as the sample. The "sample" method
#' takes a simple random sample.
compare_methods <- function(x){
  n <- length(x)
  
  correct <- mean(x)
  
  # Method 1 - the most recent 80% of observations:
  bigdata_method <- mean(tail(x, round(n * 0.8))) - correct
  
  # Method 2 - simple random sample of 1% of observations:
  sample_method <- mean(sample(x, round(n * 0.01))) - correct
  
  return(data.frame(
    bigdata_method = bigdata_method,
    sample_method = sample_method
  ))
}

#' Repeated comparison of two methods of sampling
#' 
#' @param m Number of times to run the experiment
#' @param n Number of red and white balls in the urn for which we are trying to estimate a proportion
#' @param p overall hyper-population proportion of red balls
#' @param q probability that a ball, when originally placed in the urn in order, will be the same
#' colour as the preceding ball rather than randomly chosing between red and white with probability p
#' @details This works by repeated calls to fill_urn_c() and compare_methods()
overall_results <- function(m, n, p, q){
  results <- lapply(1:m, function(j){
    x <- fill_urn_c(n, p, q)
    y <- compare_methods(x)
    return(y)
  })
  
  tmp <- do.call(rbind, results) %>%
    tidyr::gather(variable, value) %>%
    dplyr::mutate(m = m,
                  n = n,
                  p = p,
                  q = q)
  return(tmp)
}

#-------------------------------Small urns--------------
tmp1 <- overall_results(1000, 1e4, 0.3, 0.9)
tmp2 <- overall_results(1000, 1e4, 0.3, 0.99)  
tmp3 <- overall_results(1000, 1e4, 0.3, 0.999)
tmp4 <- overall_results(1000, 1e4, 0.3, 0.9999)

p1 <- rbind(tmp1, tmp2, tmp3, tmp4)  %>%
  mutate(variable = case_when(
    variable == "bigdata_method" ~ "Sample 80% of balls at top of urn",
    variable == "sample_method" ~ "Stir urn and take random sample of 1%"
  )) %>%
  mutate(q = paste(q, "serial relationship factor")) %>%
  ggplot(aes(x = value, fill = variable, colour = variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~q, scales = "free") +
  labs(fill = "Sampling method:", colour = "Sampling method:",
       x = "Discrepency between sample proportion and true proportion from entire urn",
       title = "Comparison of 'big data' and random sampling methods",
       subtitle = "Estimating a proportion from an urn of 10,000 red and white balls.
Which method works best depends on the degree of serial correlation between balls.",
       caption = "http://freerangestats.info")

frs::svg_png(p1, "../img/0160-10000")

#----------------------Large urns------------------
tmp5 <- overall_results(1000, 1e6, 0.3, 0.9)
tmp6 <- overall_results(1000, 1e6, 0.3, 0.99)  
tmp7 <- overall_results(1000, 1e6, 0.3, 0.999)
tmp8 <- overall_results(1000, 1e6, 0.3, 0.9999)

p2 <- rbind(tmp5, tmp6, tmp7, tmp8)  %>%
  mutate(variable = case_when(
    variable == "bigdata_method" ~ "Sample 80% of balls at top of urn",
    variable == "sample_method" ~ "Stir urn and take random sample of 1%"
  )) %>%
  mutate(q = paste(q, "serial relationship factor")) %>%
  ggplot(aes(x = value, fill = variable, colour = variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~q, scales = "free") +
  labs(fill = "Sampling method:", colour = "Sampling method:",
       x = "Discrepency between sample proportion and true proportion from entire urn",
       title = "Comparison of 'big data' and random sampling methods",
       subtitle = "Estimating a proportion from an urn of one million red and white balls.
Which method works best depends on the degree of serial correlation between balls.",
       caption = "http://freerangestats.info")
frs::svg_png(p2, "../img/0160-million")

p3 <- rbind(tmp5, tmp6, tmp7, tmp8)  %>%
  mutate(variable = case_when(
    variable == "bigdata_method" ~ "Sample 80% of balls at top of urn",
    variable == "sample_method" ~ "Stir urn and take random sample of 1%"
  )) %>%
  mutate(q = paste(q, "serial relationship factor")) %>%
  mutate(value = abs(value)) %>%
  ggplot(aes(x = value, fill = variable, colour = variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~q, scales = "free") +
  labs(fill = "Sampling method:", colour = "Sampling method:",
       x = "Absolute value of discrepency between sample proportion and true proportion from entire urn",
       title = "Comparison of 'big data' and random sampling methods",
       subtitle = "Estimating a proportion from an urn of one million red and white balls.
Which method works best depends on the degree of serial correlation between balls.",
       caption = "http://freerangestats.info")

frs::svg_png(p3, "../img/0160-million-abs")

