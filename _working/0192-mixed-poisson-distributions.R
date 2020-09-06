library(tidyverse)
library(scales)
library(ggrepel)
library(glue)
library(ggtext)
library(rstan)


#---------Example adapted fro Wilcox Modern Statistics for the Social and Behavioral Sciences---------
# Wilcox's eg 3.8

set.seed(123)
egd <- tibble(diet = rep(c("Diet", "No diet"), c(10000, 90000))) %>%
  mutate(weight_loss = ifelse(diet == "Diet", 
                              rnorm(n(), 0, 10),
                              rnorm(n(), 0, 1))) 

egd %>% group_by(diet) %>% summarise(var(weight_loss))

p0 <- ggplot(egd, aes(x = weight_loss)) +
  geom_density(fill = "grey", alpha = 0.5, colour = "darkblue") +
  stat_function(fun = dnorm, colour = "darkgreen", n = 1000) +
  coord_cartesian(xlim = c(-3, 3)) +
  annotate("text", x = 1, y = 0.3, 
           label = "sigma^2==1", parse = TRUE,
           colour = "darkgreen", hjust = 0) +
  annotate("text", x = -1.1, y = 0.17, 
           label = glue("sigma^2=={round(var(egd$weight_loss), 1)}"), parse = TRUE,
           colour = "darkblue", hjust = 0) +
  annotate("text", x = -1.1, y = 0.13, 
           label ="Mixture of standard normal (90%) and\nnormal with sd=10" , 
           colour = "darkblue", hjust = 0, size = 3) +
  
  labs(caption = "Adapted from Figure 3.8 of Wilcox, Modern Statistics for the Social and Behavioral Sciences",
       x = "Weight loss",
       title = "The perils of a mixed or 'contanimated' distribution",
       subtitle = "Comparison of a <span style = 'color:darkgreen;'>standard normal</span> and a <span style = 'color:darkblue;'>mixed normal</span> distribution") +
  theme(plot.subtitle = element_textbox())

svg_png(p0, "../img/0192-normal-eg", 8, 5)

#--------Florida delay in death reporting data-----------
# From Jason Salemi 28 August 2020 via Twitter

orig_d <- tibble(freq= c(11,18,2,2,1,5,2,3,0,1,2,1,3,1,0,3,1,1,0,2,0,0,1,
                    3,0,0,1,0,0,1,2,2,2,7,8,4,7,6,6,3,3,6,0,2,2,1,3,
                    0,2,3,0,0,1,0,0,1)) %>%
  mutate(delay = 1:n() - 1) %>%
  mutate(cumulative_freq = cumsum(freq),
         quantile = cumulative_freq / sum(freq))

orig_x <- with(orig_d, rep(delay, freq))
mean(orig_x)
median(orig_x)


p1 <- ggplot(orig_d, aes(x = as.ordered(delay), y = freq)) +
  geom_vline(xintercept = mean(orig_x), colour = "blue", size = 2) +
  geom_vline(xintercept = median(orig_x), colour = "red", size = 2) +
  geom_col(width = 1, colour = "grey50", alpha = 0.9) +
  geom_text(aes(label = ifelse(freq > 0, freq, ""), y = freq + 1), size = 3, colour = "steelblue") +
  scale_x_discrete(breaks = 0:5 * 10) +
  labs(x = "Time elapsed from death occuring to appearing in confirmed statistics",
       y = "Frequency",
       title = "Reporting lags in Covid-19 deaths in Florida",
       caption = "Data from Jason L. Salemi",
       subtitle = "An interesting example of a mixed distribution; with two modes, and <span style = 'color:red;'>median</span> > <span style = 'color:blue;'>mean</span>.") +
  theme(plot.subtitle = element_textbox())

svg_png(p1, "../img/0192-original", w = 9, h = 5)



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
  med <- df[which_row, ]$x - 1
  return(med)
}

trial_grid <- expand.grid(p1 = 0:200 / 200,
                          lambda1 = 1:30 / 6) %>%
  as_tibble() %>%
  mutate(lambda2 = (23.25 - p1 * lambda1) / (1 - p1)) %>%
  mutate(med = Vectorize(median_pois2)(lambda1, lambda2, p1))

set.seed(42)
p2 <- trial_grid %>%
  filter(round(med, 2) == 31.0) %>%
  mutate(mu2_lab = ifelse(runif(n()) > 0.9, round(lambda2, 1), "")) %>%
  ggplot(aes(x = p1, y = lambda1)) +
  geom_smooth(se = FALSE, colour = "lightblue") +
  geom_point(size = 2, colour = "grey") +
  geom_text_repel(aes(label = mu2_lab), colour = "steelblue") +
  labs(x = "Proportion of variables from first Poisson distribution",
       y = "Mean of first Poisson distribution",
       title = "Different mixtures to give you the same mean and median",
       subtitle = "Parameters of a mixture of two Poisson distributions that have a mean of 23.25 and median of 31.") +
  annotate("text", x = 0.378, y = 2, label = "Labels show mean of second Poisson\ndistribution (selected points only).", 
           hjust = 0, colour = "steelblue")

svg_png(p2, "../img/0192-different-fits", w = 9, h = 6)

top_params <- trial_grid %>%
  arrange(abs(med - 31), runif(n())) %>%
  slice(1:4)

dens_results <- list()
x <- 0:70

for(i in 1:nrow(top_params)){
  d <- top_params[i, ]
  dens_results[[i]] <- with(d, 
                            data.frame(x = x,
                              dens = dpois2(x, lambda1, lambda2, p1),
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

p3 <- bind_rows(dens_results) %>%
  as_tibble() %>%
  mutate(par_lab1 = glue("lambda[1]=={round(lambda1, 1)}"),
         par_lab2 = glue("lambda[2]=={round(lambda2, 2)}"),
         par_lab3 = glue("pi[1]=={p1}")) %>%
  ggplot(aes(x = x, y = dens)) +
  facet_wrap(~par_lab1 + par_lab2 + par_lab3, labeller = lp) +
  geom_vline(xintercept = 31, colour = "red", size = 2) +
  geom_vline(xintercept = 23.5, colour = "blue", size = 2) +
  geom_col() +
  theme(panel.spacing = unit(1.5, "lines"),
        plot.title = element_textbox()) +
  xlim(0, 60) +
  labs(x = "x", 
       y = "Density",
       title = "Different mixtures to give you the same <span style = 'color:blue;'>mean</span> and <span style = 'color:red;'>median</span>",
       subtitle = "Parameters of a mixture of two Poisson distributions that (taken together) have a mean of 23.25 and median of 31.") 

svg_png(p3, "../img/0192-distributions", w = 10, h = 6)

#---------------check I'm not going mad and that I do get the right results!-------------------
n <- 1e5
xsim <- c(rpois(n * top_params[1, ]$p1, top_params[1, ]$lambda1),
          rpois(n * (1 - top_params[1, ]$p1), top_params[1, ]$lambda2))

c(mean(xsim), median(xsim))


#-----------------------fitting with stan-------

ms1 <- stan("0192-poisson.stan", data = list(x = orig_x, 
                                             N = length(orig_x),
                                             K = 2))
ms1
# Inference for Stan model: 0192-poisson.
# 4 chains, each with iter=2000; warmup=1000; thin=1; 
# post-warmup draws per chain=1000, total post-warmup draws=4000.
# 
# mean se_mean   sd    2.5%     25%     50%     75%   97.5% n_eff Rhat
# lambda[1]    3.42    0.01 0.30    2.84    3.21    3.41    3.61    4.02  2849    1
# lambda[2]   35.41    0.01 0.68   34.09   34.94   35.41   35.88   36.71  4107    1
# p[1]         0.38    0.00 0.04    0.30    0.35    0.38    0.41    0.47  3532    1
# p[2]         0.62    0.00 0.04    0.53    0.59    0.62    0.65    0.70  3532    1
# lp__      -580.55    0.03 1.29 -584.02 -581.09 -580.22 -579.62 -579.11  2029    1
# 
# Samples were drawn using NUTS(diag_e) at Sun Sep 06 11:36:48 2020.
# For each parameter, n_eff is a crude measure of effective sample size,
# and Rhat is the potential scale reduction factor on split chains (at 
#                                                                   convergence, Rhat=1).


ms2 <- stan("0192-negbin.stan", data = list(x = orig_x, 
                                             N = length(orig_x),
                                             K = 2),
            cores = 4, chains = 4)
ms2
# Inference for Stan model: 0192-negbin.
# 4 chains, each with iter=2000; warmup=1000; thin=1; 
# post-warmup draws per chain=1000, total post-warmup draws=4000.
# 
# mean se_mean   sd    2.5%     25%     50%     75%   97.5% n_eff Rhat
# mu[1]     4.83    0.04 1.64    2.52    3.70    4.49    5.58    9.04  1423    1
# mu[2]    35.82    0.02 1.50   32.87   34.86   35.80   36.81   38.79  4234    1
# phi[1]    0.77    0.00 0.23    0.44    0.61    0.74    0.89    1.30  2190    1
# phi[2]   13.71    0.06 3.22    8.18   11.39   13.48   15.71   20.74  3032    1
# p[1]      0.40    0.00 0.05    0.31    0.37    0.40    0.44    0.51  2152    1
# p[2]      0.60    0.00 0.05    0.49    0.56    0.60    0.63    0.69  2152    1
# lp__   -526.41    0.04 1.64 -530.64 -527.20 -526.08 -525.21 -524.25  1524    1
# 
# Samples were drawn using NUTS(diag_e) at Sun Sep 06 12:04:16 2020.
# For each parameter, n_eff is a crude measure of effective sample size,
# and Rhat is the potential scale reduction factor on split chains (at 
# convergence, Rhat=1).

#---------------simulated data from that fitted mixed negative binomial distribution-------------------
n <- 1e5
xsim <- c(rnbinom(n * 0.4, mu = 4.83, size = 0.77),
          rnbinom(n * 0.6, mu = 35.82, size = 13.71)
)

c(mean(xsim), median(xsim))

p4 <- data.frame(x = xsim) %>%
  ggplot(aes(x = x)) +
  geom_vline(xintercept = mean(xsim), colour = "blue", size = 2) +
  geom_vline(xintercept = median(xsim), colour = "red", size = 2) +
  geom_histogram(binwidth = 1, aes(y = ..density..), colour = "grey50", alpha = 0.9) +
  scale_y_continuous(label = comma) +
  coord_cartesian(xlim = range(orig_d$delay)) +
  theme(plot.subtitle = element_textbox_simple()) +
  labs(y = "Relative frequency",
       title = "Simulations from fitted mixture of two negative binomial distributions",
       subtitle = "Fit is ok and <span style = 'color:red;'>median</span> > <span style = 'color:blue;'>mean</span>, but not by as much as in original.")

svg_png(p4, "../img/0192-final-sim", 9, 5)  
