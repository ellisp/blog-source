
library(tidyverse)
library(scales)
library(survey)

url <- "https://raw.githubusercontent.com/ellisp/blog-source/refs/heads/master/_working/simulated-survey.csv"
df <- "simulated-survey.csv"
if(!file.exists(df)){
  download.file(url, destfile = df)
}
sim_surv <- read_csv("simulated-survey.csv")

sim_surv_des <- svydesign(~neighborhood + ind_id, weights = ~fweight, 
                          strata = ~province, fpc = ~nb_in_province + pop_in_neighborhood,
                          data = sim_surv, nest = TRUE)

svymean(~likes_cats, design = sim_surv_des, deff = TRUE)

svyby(~likes_cats, by = ~province, design = sim_surv_des, FUN = svymean, deff = TRUE)



sim_surv |>
  group_by(province, nb_in_province) |>
  summarise(n = n(),
            sampled_neighborhoods = length(unique(neighborhood))) |>
  ungroup() |>
  mutate(frac = sampled_neighborhoods / nb_in_province) |>
  select(province, nb_in_province, sampled_neighborhoods, frac, n)

# weights aren't calibrated to the population in each neighborhood
sim_surv |>
  group_by(province, neighborhood) |>
  summarise(unique(pop_in_neighborhood), sum(fweight))


results1 <- list()
results2 <- list()
reps <- 200

# Simple random sample from the population as a whole
# Note that to simulate a SRS we actually
for(i  in 1:reps){
  sim1 <- sim_surv |>
    sample_n(n(), replace = TRUE, weight = fweight)
  results1[[i]] <- sim1 |>
    group_by(province) |>
    summarise(estimate = mean(likes_cats)) |>
    mutate(simulation = i)
}

# Simple random sample from each stratum
for(i  in 1:reps){
  sim2 <- sim_surv |>
    group_by(province) |>
    sample_n(n(), replace = TRUE, weight = fweight) |>
    ungroup()
  
  results2[[i]] <- sim2 |>
    group_by(province) |>
    summarise(estimate = mean(likes_cats)) |>
    mutate(simulation = i)
}

r0 <- svyby(~likes_cats, by = ~province, design = sim_surv_des, FUN = svymean, deff = TRUE) |>
  mutate(se_srs_implied = sqrt(se ^ 2 / DEff.likes_cats)) |>
  select(province,
         point_original = likes_cats,
         se_complex = se,
         se_srs_implied)


r1 <- results1 |>
  bind_rows() |>
  group_by(province) |>
  summarise(point_sim_1 = mean(estimate),
            se_sim_1 = sd(estimate))
results2 |>
  bind_rows() |>
  group_by(province) |>
  summarise(point_sim_2 = mean(estimate),
            se_sim_2 = sd(estimate)) |>
  left_join(r1, by = "province") |>
  left_join(r0, by = "province") |>
  select(province, point_original, point_sim_1, point_sim_2, se_complex, se_srs_implied, se_sim_1, se_sim_2)


# so what is going on here is that if you did a SRS from the full population you would get
# many less people in province A and G than you actually do get under the stratified sample.
# So the stratification of the actual survey design does mean that the observed

sim_surv |>
  group_by(province) |>
  summarise(avg_weight = mean(fweight),
            p = weighted.mean(likes_cats, w = fweight),
            n = n(),
            N = sum(fweight)) |>
  ungroup() |>
  mutate(even_n = round(N / sum(N) * sum(n)),
         se_with_actual_n = sqrt(p  * (1-p ) / n * (N - n) / (N - 1)),
         se_with_even_n = sqrt(p  * (1-p ) / even_n * (N - even_n) / (N - 1)),
         se_complex = svyby(~likes_cats, by = ~province, design = sim_surv_des, deff = TRUE, FUN = svymean)$se,
         DEff_1 = (se_complex / se_with_even_n) ^ 2,
         DEff_2 = (se_complex / se_with_actual_n) ^ 2)
