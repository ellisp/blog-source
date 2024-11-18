
library(tidyverse)
library(scales)
library(survey)

url <- "https://raw.githubusercontent.com/ellisp/blog-source/refs/heads/master/_working/simulated-survey.csv"
df <- "simulated-survey.csv"
if(!file.exists(df)){
  download.file(url, destfile = df)
}
sim_surv <- read_csv("simulated-survey.csv")

# Specify survey design - stratified by province, and 2 stages, both with finite populations
sim_surv_des <- svydesign(~neighborhood + ind_id, weights = ~fweight, 
                          strata = ~province, 
                          fpc = ~nb_in_province + pop_in_neighborhood,
                          data = sim_surv, nest = TRUE)

svymean(~likes_cats, design = sim_surv_des, deff = TRUE)

# this is the equivalent of Stata's estat effect, srssubpop
svyby(~likes_cats, by = ~province, design = sim_surv_des, FUN = svymean, deff = TRUE)
# so what is happening with Stata's estat effect? It has the same standard
# errors but different design effects.


# so what is going on here is that if you did a SRS from the full population you would get
# many less people in province A and G than you actually do get under the stratified sample.
# So the stratification of the actual survey design does mean that the observed

results <- sim_surv |>
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


p <- results |>
  mutate(ratio_n = n / even_n,
         ratio_deff = DEff_2 / DEff_1) |>
  ggplot(aes(x = ratio_n, y = ratio_deff, label = province)) +
  geom_abline(slope =1 , intercept = 0, colour = "grey") +
  geom_text(colour = "steelblue", fontface = "bold") +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Oversampling ratio for actual sample\n(>1 is oversampled, <1 is undersampled)",
       y = "Design effect ratio",
       subtitle = "Ratio of 'srssubpop' design effect to Stata default design effect
Grey line shows equality of the two ratios.",
       title = "Two types of design effect") +
  coord_equal()


# # need to check I have all the labels the right way around here
# p <- results |>
#   mutate(ratio = n / even_n) |>
#   ggplot(aes(x = DEff_1, y = DEff_2, label = province, fill = ratio)) +
#   geom_abline(intercept = 0, slope = 1, colour = "steelblue") +
#   geom_label(fontface = "bold", colour = "white") +
#   scale_fill_viridis_c(option = "C", direction = -1) +
#   coord_equal() +
#   annotate("text", x = c(1, 2.7), y = c(2.6, 1.4), label = 
#              c("Bigger design effect\nwith actual sampling", 
#                "Bigger design effect\nwith random sampling")) +
#   labs(x = str_wrap("Design effect based on sample sizes from simple random sampling", 40),
#        y = str_wrap("Design effect calculated based on actual sample sizes", 30),
#        fill = str_wrap("Ratio of actual sample size to hypothetical one from random sampling", 30),
#        subtitle = "In Stata, this is the different between estat effect and estat effect, srssubpop",
#        title = "Calculating design effects for an estimate partitioned by a categorical variable")

svg_png(p, "../img/0263-scatter", w = 8, h = 7)

#-----------------simulation to explore how that works---------------
# not sure I want this

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


