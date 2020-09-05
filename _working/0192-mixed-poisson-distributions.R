library(tidyverse)
library(scales)
library(ggrepel)
library(glue)

orig_d <- tibble(freq= c(11,18,2,2,1,5,2,3,0,1,2,1,3,1,0,3,1,1,0,2,0,0,1,
                    3,0,0,1,0,0,1,2,2,2,7,8,4,7,6,6,3,3,6,0,2,2,1,3,
                    0,2,3,0,0,1,0,0,1)) %>%
  mutate(delay = 1:n() - 1) %>%
  mutate(cumulative_freq = cumsum(freq),
         quantile = cumulative_freq / sum(freq))

orig_x <- with(orig_d, rep(delay, freq))
mean(orig_x)
median(orig_x)

ggplot(data.frame(orig_x), aes(x = orig_x)) +
  geom_histogram()

# mu = p.lambda1 + (1-p).lambda2
# lambda2 = (mu - p.lambda1) / (1-p)


#' Density of a mixture of two Poisson distributions
#' 
#' @param x vector of non-negative integers
#' @param lambda1 mean of first Poisson distribution
#' @param lambda2 mean of second Poisson distribution
#' @param p1 proportion of values that come from the first distribution
dpois2 <- function(x, lambda1, lambda2, p1){
  d <- dpois(x, lambda1) * p1 +
    dpois(x, lambda2) * (1 - p1)
  return(d)
}


#' Approximate the median of a mixture of two Poisson distributions
#' 
#' @param lambda1 mean of first Poisson distribution
#' @param lambda2 mean of second Poisson distribution
#' @param p1 proportion of values that come from the first distribution
#' @param maxx highest value data to take into account
median_pois2 <- function(lambda1, lambda2, p1, maxx = 1000){
  df <- data.frame(x = 0:maxx) 
  df$dens <- dpois2(df$x, lambda1, lambda2, p1)
  df$F <- cumsum(df$dens)
  which_row <- which(abs(df$F - 0.5) == min(abs(df$F - 0.5)))[1]
  if(which_row == 1){
    med <- df[1, ]$x
  } else {
    m <- lm(x ~ F, data = df[(which_row - 1):(which_row + 1), ])
    med <- as.numeric(predict(m, newdata = data.frame(F = 0.5)))
  }
  return(med)
}

trial_grid <- expand.grid(p1 = 0:100 / 100,
                          lambda1 = 1:30 / 6) %>%
  as_tibble() %>%
  mutate(lambda2 = (23.25 - p1 * lambda1) / (1 - p1)) %>%
  mutate(med = Vectorize(median_pois2)(lambda1, lambda2, p1))

trial_grid %>%
  arrange(abs(med - 31))

trial_grid %>%
  ggplot(aes(x = p1, y = lambda1, fill = med)) +
  geom_tile() +
  scale_x_continuous(limits = c(0.4, 0.5)) +
  scale_fill_viridis_c()



trial_grid %>%
  filter(round(med, 1) == 31.0) %>%
  ggplot(aes(x = p1, y = lambda1)) +
  geom_smooth(se = FALSE, colour = "lightblue") +
  geom_point(size =2) +
  geom_text_repel(aes(label = round(lambda2, 1))) +
  labs(x = "Proportion of variables from first Poisson distribution",
       y = "Mean of first Poisson distribution",
       title = "Different mixtures to give you the same mean and median",
       subtitle = "Parameters of a mixture of two Poisson distributions that have a mean of 23.25 and median of 31.") +
  annotate("text", x = 0.415, y = 1.7, label = "Labels show mean of second\nPoisson distribution.", hjust = 0)

top_four_params <- trial_grid %>%
  arrange(abs(med - 31)) %>%
  slice(1:4)

dens_results <- list()
x <- 0:40

for(i in 1:nrow(top_four_params)){
  d <- top_four_params[i, ]
  dens_results[[i]] <- with(d, 
                            data.frame(x = x,
                              dens = dpois2(0:40, lambda1, lambda2, p1),
                              lambda1 = lambda1,
                              lambda2 = lambda2,
                              p1 = p1,
                              parameter_set = i))
}

# Not sure why I have to define my own labeller function, but I can't see
# otherwise how I pass multi_line through to label_parsed in facet_wrap() !
lp <- function(labels, multi_line = FALSE){
  label_parsed(labels, multi_line = multi_line)
}

bind_rows(dens_results) %>%
  as_tibble() %>%
  mutate(par_lab1 = glue("lambda[1]=={round(lambda1, 1)}"),
         par_lab2 = glue("lambda[2]=={round(lambda2, 2)}"),
         par_lab3 = glue("pi[1]=={p1}")) %>%
  ggplot(aes(x = x, y = dens)) +
  facet_wrap(~par_lab1 + par_lab2 + par_lab3, labeller = lp) +
  geom_col() +
  theme(panel.spacing = unit(1.5, "lines")) +
  labs(x = "x", 
       y = "Density",
       title = "Different mixtures to give you the same mean and median",
       subtitle = "Parameters of a mixture of two Poisson distributions that (taken together) have a mean of 23.25 and median of 31.") 
