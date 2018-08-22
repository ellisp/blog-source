library(tidyverse)
library(boot)
library(testthat)
library(survey)
library(broom)

N <- 1000000

set.seed(123)

colours_v <- c("red", "blue", "green", "yellow", "orange", "grey")

population1 <- data_frame(
  region = sample(LETTERS[1:14], size = N, replace = TRUE, prob = c(runif(14)))
) %>%
  group_by(region) %>%
  mutate(psu = paste(region, sample(1:(n() / 100) , size = n(), replace = TRUE)),
         shape = sample(c("square", "circle", "triangle", "hexagon"), size = n(), replace = TRUE)) %>%
  ungroup() %>%
  mutate(colour = case_when(
    shape == "square" ~ sample(colours_v, size = n(), prob =   c( 1, 1, 2, 2, 1, 4), replace = TRUE),
    shape == "circle" ~ sample(colours_v, size = n(), prob =   c( 2, 2, 1, 1, 2, 3), replace = TRUE),
    shape == "triangle" ~ sample(colours_v, size = n(), prob = c( 4, 1, 3, 2, 3, 2), replace = TRUE),
    shape == "hexagon" ~ sample(colours_v, size = n(), prob =  c( 2, 5, 1, 2, 4, 1), replace = TRUE)
  )) %>%
  mutate(mystery_var = rnorm(n(), 0, 0.2) * as.numeric(as.factor(region)))


regions <- population1 %>%
  distinct(region) %>%
  mutate(region_y = rnorm(n()) * 3)

psus <- population1 %>%
  distinct(psu) %>%
  mutate(psu_y = rnorm(n()) * 2)

shapes <- population1 %>%
  distinct(shape) %>%
  mutate(shape_y = rnorm(n()) * 3)

colours <- population1 %>%
  distinct(colour) %>%
  mutate(colour_y = rnorm(n()) * 2)

population <- population1 %>%
  left_join(regions, by = "region") %>%
  left_join(psus, by = "psu") %>%
  left_join(shapes, by = "shape") %>%
  left_join(colours, by = "colour") %>%
  mutate(latent = inv.logit(scale(mystery_var + region_y + psu_y + shape_y + colour_y + 
                                             rnorm(n(), 0, 0.05))) / 2) %>%
  mutate(y = rbinom(n(), size = 1, prob = latent))

plot(density(population$latent))

population %>%
  select(mystery_var:y) %>%
  sample_n(10000) %>%
   cor %>%
  round(2)




#==================sampling======================
# something wrong here

# number of psus sampled per region:
npsu <- 10
# number of people sampled per psu:
npeop <- 15
# total sample size:
length(unique(population$region)) * npsu * npeop


region_pops <- population %>%
  group_by(region) %>%
  mutate(psus_in_region = length(unique(psu))) %>%
  group_by(region, psu) %>%
  summarise(people_in_psu = n(),
            prob_from_psu = npeop / people_in_psu,
            prob = npsu / unique(psus_in_region) * prob_from_psu,
            wt = 1 / prob)
  
  
psu_sampled <- population %>%
  distinct(region, psu) %>%
  group_by(region) %>%
  mutate(seq = rnorm(n())) %>%
  arrange(seq) %>%
  slice(1:npsu) %>%
  select(-seq)
  
sampled <- population %>%
  inner_join(psu_sampled, by = c("region", "psu")) %>%
  group_by(psu) %>%
  mutate(seq = rnorm(n())) %>%
  slice(1:npeop) %>%
  left_join(region_pops, by = c("region", "psu"))

expect_equal(nrow(sampled), length(unique(population$region)) * npsu * npeop)

des <- svydesign(~psu, strata = ~region, data = sampled, weights = ~wt)
svymean(~y, des, deff = TRUE)


#===========modelling============



full_pop_model_pois <-  glm(y ~ region + shape + colour, family = quasipoisson(log), data = population)

full_pop_model_bin <-  glm(y ~ region + shape + colour, family = quasibinomial(log), data = population,
                           start = coef(full_pop_model_pois))

round(exp(cbind(coef(full_pop_model_bin), coef(full_pop_model_pois))), 2)


samp_model_pois <-  glm(y ~ region + shape + colour, family = quasipoisson(log), data = sampled)

samp_model_bin <-  glm(y ~ region + shape + colour, family = quasibinomial(log), data = sampled,
                           start = coef(samp_model_pois))

round(exp(cbind(coef(samp_model_bin), coef(samp_model_pois),
                coef(full_pop_model_bin), coef(full_pop_model_pois))), 2)

# psot strat weighting

samp_model_pois_svy <-  svyglm(y ~ region + shape + colour, family = quasipoisson(log), design = des)
samp_model_bin_svy <-  svyglm(y ~ region + shape + colour, family = quasibinomial(log), design = des)



round(exp(cbind(coef(samp_model_bin), coef(samp_model_pois),
                coef(samp_model_bin_svy), coef(samp_model_pois_svy),
                coef(full_pop_model_bin), coef(full_pop_model_pois))), 2)

round(exp(confint(samp_model_bin)), 2)
round(exp(confint(samp_model_bin_svy)), 2)



res <- mutate(tidy(confint(samp_model_bin)), model = "naive") %>%
  rbind(mutate(tidy(confint(samp_model_bin_svy)), model = "survey"))  %>%
  rename(lower = X2.5..,
         upper = X97.5..,
         names = .rownames) %>%
  left_join(tidy(coef(full_pop_model_bin), by = "names")) %>%
  mutate(true_value = x,
         y_pos = ifelse(model == "naive", 
                        as.numeric(as.factor(names)) + 0.1,
                        as.numeric(as.factor(names)) - 0.1))

res %>%
  ggplot() +
  geom_segment(aes(yend = y_pos, y = y_pos,
                   x = lower, xend = upper, colour = model)) +
  geom_point(aes (y = as.numeric(as.factor(names)), x = x)) +
  scale_y_continuous(labels = unique(res$names), breaks = 1:length(unique(res$names)))
