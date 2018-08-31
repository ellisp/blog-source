library(tidyverse)
library(boot)
library(testthat)
library(survey)
library(broom)
library(scales)
library(gridExtra)
library(GGally)
library(knitr)

#--------------------create a simulated population------------------------
#' @param N total population size
#' @param n_regions number of regions (which will be used in sampling as strata)
#' @param psu_sd standard deviation of random contribution to latent variable under y at the PSU 
#' (primary sampling unit eg suburb or meshblock) level.  Higher values lead to higher intra-class
#' correlation, and hence higher design effects when we use psu as the primary sampling unit in a
#' sampling design
#' @param region_sd as psu_sd but at region level
#' @param mystery_sd extra random continuous variable that contributes to the latent variable under y.
make_population <- function(N = 100000, 
                            n_regions = 14,
                            psu_sd = 1,
                            region_sd = 1,
                            colours_sd = 1,
                            shapes_sd = 1,
                            mystery_sd = 1,
                            colours_v = c("red", "blue", "green", "yellow", "orange", "grey"),
                            shapes = c("square", "circle", "triangle", "hexagon"),
                            seed = 123){

  set.seed(seed)
  
  
  population1 <- data_frame(
    region = sample(LETTERS[1:n_regions], size = N, replace = TRUE, prob = c(runif(n_regions)))
  ) %>%
    group_by(region) %>%
    mutate(psu = paste(region, sample(1:(n() / 100) , size = n(), replace = TRUE)),
           shape = sample(shapes, size = n(), replace = TRUE)) %>%
    ungroup() %>%
    # we want colours and shapes to be crosscorrelated, so we make generation of colour dependent on shape:
    mutate(colour = case_when(
      shape == shapes[1] ~ sample(colours_v, size = n(), prob =   c( 1, 1, 2, 2, 1, 4), replace = TRUE),
      shape == shapes[2] ~ sample(colours_v, size = n(), prob =   c( 2, 2, 1, 1, 2, 3), replace = TRUE),
      shape == shapes[3] ~ sample(colours_v, size = n(), prob = c( 4, 1, 3, 2, 3, 2), replace = TRUE),
      shape == shapes[4] ~ sample(colours_v, size = n(), prob =  c( 2, 5, 1, 2, 4, 1), replace = TRUE)
    )) %>%
    mutate(mystery_var = as.vector(scale(rnorm(n(), 0, 1) * as.numeric(as.factor(region))) * mystery_sd))
  
  # numeric values to add for each particular value of region, psu, colour, and shape:
  regions <- population1 %>%
    distinct(region) %>%
    mutate(region_y = as.vector(scale(runif(n())) * region_sd))
  
  psus <- population1 %>%
    distinct(psu) %>%
    mutate(psu_y = rnorm(n()) * psu_sd)
  
  shapes <- population1 %>%
    distinct(shape) %>%
    mutate(shape_y = as.vector(scale(rgamma(n(), 1, 0.1)) * shapes_sd))
  
  colours <- population1 %>%
    distinct(colour) %>%
    mutate(colour_y = as.vector(scale(rnorm(n())) * colours_sd))
  
  population <- population1 %>%
    left_join(regions, by = "region") %>%
    left_join(psus, by = "psu") %>%
    left_join(shapes, by = "shape") %>%
    left_join(colours, by = "colour") %>%
    mutate(latent = inv.logit(as.vector(scale(mystery_var + region_y + psu_y + shape_y + colour_y + 
                                               rnorm(n(), 0, 0.05)))) / 2) %>%
    mutate(y = as.logical(rbinom(n(), size = 1, prob = latent)))
  
  return(population)
}



#==================sampling======================

#' Two stage sample from a "population" data frame
#' 
#' @param population a data frame to be sampled from, must include columns with name region, psu,
#' shape and colour
#' @param npsu number of PSUs (primary sampling units - think suburbs or meshblocks) per region (strata) to
#' saple
#' @param n total sample size
#' @param npeop number of people to sample per PSU
#' @value a survey design object, calibrated to population totals for region, shape and colour; with primary
#' selection by psu and stratified by region; with Jackknife replicate weights.
samp <- function(population, 
                 npsu = 10, 
                 n = 2100, 
                 npeop = round(n / npsu / length(unique(population$region))),
                 seed = 123){

  if(!is.null(seed)){
    set.seed(seed)
  }
  
  # Population totals for margins, which we presume are known from a census or similar.  We are
  # pretending we know the marginal totals for these dimensions, but not a full cross tabulation
  # (ie we know the total number of circles, and of red people, but not of red circular people):
  shape_tots <- population %>%
    group_by(shape) %>%
    summarise(Freq = n())
  
  colour_tots <- population %>%
    group_by(colour) %>%
    summarise(Freq = n())
  
  region_tots <- population %>%
    group_by(region) %>%
    summarise(Freq = n())
  
  # number of people in each psu, which we will use for our weighting:
  region_psu_pops <- population %>%
    group_by(region) %>%
    mutate(psus_in_region = length(unique(psu))) %>%
    group_by(region, psu) %>%
    summarise(
              # how many people in each PSU:
              people_in_psu = n(),
              # given the PSU is sampled, what's the chance of an individual from that PSU being in sample:
              prob_from_psu = npeop / people_in_psu,
              # so what's the a total priori probability for an individual being sampled:
              prob = npsu / unique(psus_in_region) * prob_from_psu,
              # inverse of that probability for use as weight:
              wt = 1 / prob) %>%
    left_join(region_tots, by = "region") %>%
    rename(people_in_region = Freq)
    
  # check that the total sum of probabilities of being selected times people in psus 
  # equals our desired sample size:
  expect_equal(round(n, -1), 
               round(with(region_psu_pops, sum(prob * people_in_psu)), -1))
  
  # we sample PSUs with equal probability within each region (ie not proportionate to size, 
  # which would be a better sampling strategy):
  psu_sampled <- population %>%
    distinct(region, psu) %>%
    # group by region, so each region acts as a stratum and we get the same number of
    # of PSUs per region (again, this isn't efficient sampling, but part of the
    # point of this exercise is to have a complex non-optimal sample):
    group_by(region) %>%
    mutate(seq = rnorm(n())) %>%
    arrange(seq) %>%
    slice(1:npsu) %>%
    select(-seq)
    
  sampled <- population %>%
    select(region, psu, shape, colour, y) %>%
    # limit ourselves to just the PSUs we have chosen for our sample:
    inner_join(psu_sampled, by = c("region", "psu")) %>%
    # within each PSU, pick `npeop` people at random:
    group_by(psu) %>%
    mutate(seq = rnorm(n())) %>%
    slice(1:npeop) %>%
    left_join(region_psu_pops, by = c("region", "psu"))
  
  # Specify the survey design.
  # we will simplify things by ignoring any finite population corrections, either
  # for PSUs within region, or for individuals within PSUs.  This is ok if the sample
  # is small compared to population (which it is).
  
  # Sampling scheme:
  des <- svydesign(~psu, strata = ~region, data = sampled, weights = ~wt)
  
  # Replicate weights:
  des <- as.svrepdesign(des, type = "JKn")
  
  # Post stratification calibration to marginal population totals:
  des <- calibrate(des, list(~shape, ~colour, ~region),
              population = list(shape_tots, colour_tots, region_tots))

  return(des)
  }



#=============playing around==============
population <-make_population(N = 10 ^ 6,
                             psu_sd = 5,
                             region_sd = 2,
                             mystery_sd = 0.5,
                             shapes_sd = 4,
                             colours_sd = 1.5)

svg("../img/0132-proportions.svg", 8, 5)
population %>%
  select(region, shape, colour, y) %>%
  gather(variable, value, -y) %>%
  ggplot(aes(x = value, fill = y)) +
  facet_wrap(~variable, scales = "free_y") +
  geom_bar(position = "fill") +
  scale_y_continuous("Prevalence of 'y'", label = percent) +
  coord_flip() +
  labs(x = "") +
  ggtitle("Prevalence of y in simulated population")
dev.off()


des <- samp(population, npsu = 10, seed = 123)
svymean(~y, des, deff = TRUE)
#0.25117, 0.017
svytable(~shape, des)

plot(density(population$latent))

population %>%
  select(mystery_var:y) %>%
  sample_n(10000) %>%
  cor %>%
  round(2) %>%
  kable()

des$variables %>%
  select(region, region_y, shape, shape_y, colour, colour_y, mystery_var, latent, y) %>% 
  ggpairs()

des$variables %>%
  select(region, shape, colour, y) %>% 
  ggpairs()



npsus <- rep(2:40, 6)

deffs_l <- lapply(npsus, function(i){
  des <- samp(population, npsu = i, seed = NULL)
  X <- svymean(~y, des, deff = TRUE)
  return(c(npsu = i, deff = attributes(X)$"deff"[1,1]))
  })
deffs <- as.data.frame(do.call("rbind", deffs_l))

svg("../img/0132-design-effects.svg", 8, 5)
ggplot(deffs, aes(x = npsu, y = deff)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = "Number of PSUs selected per stratum", 
       y = "Design effect for mean(y)") +
  ggtitle("Design effect is higher when we rely on fewer primary sampling units")
dev.off()

#===========modelling============
# First, we model the full population to get the "true" values (note that these are not
# really "true" as the model is somewhat mis-specified, but they are as true as can be done
# given the imperfection of the model, and hence form a good benchmark for the same model
# fit to a sample).  As we have the full population, we don't need to worry about survey methods
# and can just use stats::glm rather than survey::svyglm
full_pop_model_bin <-  glm(y ~ region + shape + colour, family = quasibinomial(log), data = population)

#-------------compare a single example-------
des <- samp(population, npsu = 5, seed = 999)
data <- des$variables

samp_model_bin <-  glm(y ~ region + shape + colour, family = quasibinomial(log), data = data)

samp_model_bin_svy <-  svyglm(y ~ region + shape + colour, family = quasibinomial(log), design = des)


res <- mutate(tidy(exp(confint(samp_model_bin))), model = "naive") %>%
  rbind(mutate(tidy(exp(confint(samp_model_bin_svy))), model = "survey"))  %>%
  rename(lower = X2.5..,
         upper = X97.5..,
         names = .rownames) %>%
  left_join(tidy(exp(coef(full_pop_model_bin)), by = "names")) %>%
  mutate(name_number = as.numeric(as.factor(names)),
         true_value = x,
         y_pos = ifelse(model == "naive", 
                        name_number + 0.1,
                        name_number - 0.1),
         names = gsub("colour", "Colour: ", names),
         names = gsub("region", "Region: ", names),
         names = gsub("shape", "Shape: ", names)) %>%
  arrange(model, name_number)

svg("../img/0132-example-cis.svg", 8, 8)
res %>%
  ggplot() +
  geom_segment(aes(yend = y_pos, y = y_pos,
                   x = lower, xend = upper, colour = model), size = 1.7) +
  geom_point(aes (y = as.numeric(as.factor(names)), x = x), size = 2) +
  scale_y_continuous(labels = res[1:22, ]$names, breaks = 1:22) +
  labs(x = "Relative risk ratios",
       y = "Variable",
       colour = "Estimation method",
       caption = "Complex sample of around 2,100 drawn from a population of a million.  Black points show the true values") +
  ggtitle("Example confidence intervals for risk ratios from a quasi-binomial model",
          "Two modelling methods from the same sample, compared to true values from total population.")
dev.off()
#------------systematically compare lots of different samples' point estimates--------

compare_mse <- function(npsu, seed = NULL){
  des <- samp(population, npsu = npsu, seed = seed)
  data <- des$variables

  # The quasibinomial response isn't robust enough for either glm or svyglm, so we get some starting
  # values with quasipoisson to ensure we can always get a response:
  samp_model_pois_svy <-  svyglm(y ~ region + shape + colour, family = quasipoisson(log), design = des)
  
    
  samp_model_bin <-  glm(y ~ region + shape + colour, family = quasibinomial(log), data = data,
                         start = coef(samp_model_pois_svy))

  samp_model_bin_svy <-  svyglm(y ~ region + shape + colour, family = quasibinomial(log), design = des,
                                start = coef(samp_model_pois_svy))
  
  return(
    c(survey = mean((coef(samp_model_bin_svy)[-1] - coef(full_pop_model_bin)[-1]) ^ 2),
      naive = mean((coef(samp_model_bin)[-1] - coef(full_pop_model_bin)[-1]) ^ 2))
  )}

mses <- lapply(1:100, function(i){compare_mse(npsu = 5, seed = i)})
mses_df <- as.data.frame(do.call("rbind", mses))

mse <- round(apply(mses_df, 2, mean), 3)

p1 <- ggplot(mses_df, aes(x = naive, y = survey)) +
  geom_abline(intercept = 0, slope = 1, colour = "steelblue") +
  geom_point() +
  coord_equal() +
  ggtitle("Mean square error of coefficients",
          "Two methods of modelling, compared to correct (full\npopulation) values for all coefficients except the intercept.") +
  labs(x = paste0("Complex survey methods\nAverage mean squared error: ", mse[1]),
       y = paste0("Naive methods.\nAverage mean squared error: ", mse[2]))


#========compare confidence interval coverage========

#------------systematically compare lots of different samples' point estimates--------

ci_rate <- function(correct_mod, ci_mod){
  X <- as.data.frame(cbind(coef(correct_mod), confint(ci_mod)))
  names(X) <- c("correct", "lower", "upper")
  return(sum(with(X, correct > lower & correct < upper)) / nrow(X))
}

compare_ci <- function(npsu, seed = NULL){
  des <- samp(population, npsu = npsu, seed = seed)
  data <- des$variables
  
  # The quasibinomial response isn't robust enough for either glm or svyglm, so we get some starting
  # values with quasipoisson to ensure we can always get a response:
  samp_model_pois_svy <-  svyglm(y ~ region + shape + colour, family = quasipoisson(log), design = des)
  
  
  samp_model_bin <-  glm(y ~ region + shape + colour, family = quasibinomial(log), data = data,
                         start = coef(samp_model_pois_svy))
  
  samp_model_bin_svy <-  svyglm(y ~ region + shape + colour, family = quasibinomial(log), design = des,
                                start = coef(samp_model_pois_svy))
  
    
  return(
    c(binomial_svy = ci_rate(full_pop_model_bin, samp_model_bin_svy),
      poisson_svy = ci_rate(full_pop_model_bin, samp_model_pois_svy),
      naive = ci_rate(full_pop_model_bin, samp_model_bin))
  )}

cis <- lapply(1:100, function(i){compare_ci(npsu = 5, seed = i)})
cis_df <- as.data.frame(do.call("rbind", cis))

successes <- round(apply(cis_df, 2, mean) * 100)

p2 <- ggplot(cis_df, aes(x = binomial_svy, y = naive)) +
  geom_abline(intercept = 0, slope = 1, colour = "steelblue") +
  annotate("rect", xmin = -Inf, xmax = 1, ymin = 0.95, ymax = 1, fill = "steelblue", alpha = 0.5) +
  annotate("rect", xmin = 0.95, xmax = 1, ymin = -Inf, ymax = 0.95, fill = "steelblue", alpha = 0.5) +
  geom_jitter(width = 0.01, height = 0.01) +
  coord_equal() +
  ggtitle("Confidence intervals' coverage",
          "Two methods of modelling, showing proportion that\ncontain the correct value (should be 95%).") +
  labs(x = paste0("Complex survey methods.\nAverage coverage: ", successes[1], "%"),
       y = paste0("Naive methods.\nAverage coverage: ", successes[3], "%"),
       caption = str_wrap("Source for both diagrams: complex samples of around 2,100 each, drawn from a 
population of 1 million stratified by 14 unequal regions with 
5 out of 100 potential primary sampling units chosen per region.  Model has three categorical explanatory variables and a single binary (quasi-binomial family) response.
Points have been jittered.", 90)) +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = percent)

svg("../img/0132-comparisons.svg", 10, 7)
  grid.arrange(p1, p2, ncol = 2)
dev.off()

svg("../img/0132-bin-pois.svg", 8, 8)
ggplot(cis_df, aes(x = binomial_svy, y = poisson_svy)) +
  geom_abline(intercept = 0, slope = 1, colour = "steelblue") +
  annotate("rect", xmin = -Inf, xmax = 1, ymin = 0.95, ymax = 1, fill = "steelblue", alpha = 0.5) +
  annotate("rect", xmin = 0.95, xmax = 1, ymin = -Inf, ymax = 0.95, fill = "steelblue", alpha = 0.5) +
  geom_jitter(width = 0.01, height = 0.01) +
  coord_equal() +
  ggtitle("Confidence intervals' coverage",
          "Two methods of modelling, showing proportion that\ncontain the correct value (should be 95%).") +
  labs(x = paste0("Quasi-binomial family\nAverage coverage: ", successes[1], "%"),
       y = paste0("Quasi-poisson family\nAverage coverage: ", successes[2], "%"),
       caption = str_wrap("Source: complex samples of around 2,100 each, drawn from a 
population of 1 million stratified by 14 unequal regions with 
5 out of 100 potential primary sampling units chosen per region.  Model has three categorical 
explanatory variables and a single binary response from either the quasi-binomial or quasi-poisson family.
Points have been jittered.", 90)) +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = percent)
dev.off()

convert_pngs("0132")


