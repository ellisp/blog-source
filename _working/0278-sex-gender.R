library(tidyverse)
library(readxl)
library(glue)


download.file("https://www.abs.gov.au/statistics/people/people-and-communities/general-social-survey-summary-results-australia/2020/GSS_Table5.xlsx",
              destfile = "gss_table5.xlsx", mode = "wb")

d <- read_excel("gss_table5.xlsx", skip = 6, sheet = "Table 5.1_Estimate") |>
  rename(variable = ...1) |>
  filter(!is.na(variable)) |>
  mutate(category = ifelse(is.na(Heterosexual), variable, NA)) |>
  tidyr::fill(category, .direction = "down") |>
  filter(!is.na(Heterosexual)) |>
  mutate(sequence = 1:n()) |>
  gather(sexuality, value, -variable, -category, -sequence) |>
  mutate(value = as.numeric(value)) |>
  group_by(sexuality, category) |>
  mutate(prop = value / sum(value)) |>
  mutate(variable = fct_reorder(variable, sequence),
         var_wrap = fct_reorder(str_wrap(variable, 20), sequence)) |>
  ungroup()

d
View(d)

# some categories have long answers and are difficult to present on a chart
difficult_cats <- c("Community involvement", "Cultural tolerance and discrimination",
                    "Family and community support", "Crime and safety", "Stressors")

d |>
  filter(!category %in% difficult_cats) |>
  filter(sexuality != "Total persons") |>
  filter(variable != "Persons aged 15 years and over") |>
  ggplot(aes(x = var_wrap, y = prop, fill = sexuality)) +
  geom_col(position = "dodge") +
  facet_wrap(~str_wrap(category, 35), scales = "free") +
  labs(x = "", fill = "") +
  scale_y_continuous(label = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.8, 0.1))


d2 <- d |>
  select(category, variable, sexuality, prop) |>
  spread(sexuality, prop) |>
  mutate(ratio = `Gay, Lesbian or Bisexual` / Heterosexual) |>
  mutate(label = glue("{category} is '{variable}'")) |>
  select(label, ratio)

# The ten things with higher proportion of LBTIQ+ in it
d2 |> arrange(desc(ratio))  |> slice(1:10)
  
# the ten things with lower proportion
d2 |> arrange(ratio) |> slice(1:10)



#------can you deduce population subset sizes from the confidence intervals------
# You *can* but it's very rough
library(survey)
library(tidyverse)

data(nhanes)
nhanes$random_cat <- sample(c("cat", "dog"), size = nrow(nhanes), replace = TRUE, prob = c(1,5))

design <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR, 
                    nest = TRUE, data = nhanes)

point_est <- svyby(~HI_CHOL, ~race, design, FUN = svymean, na.rm = TRUE, deff = TRUE)
ci_race <- confint(point_est)

nhanes |>
  group_by(race) |>
  summarise(n = n(),
            pop = sum(WTMEC2YR),
            avg_weight = mean(WTMEC2YR)) |>
  ungroup() |>
  mutate(ci_size = apply(ci_race, 1, diff),
         implied_prop = 1 / (ci_size ^ 2),
         implied_n = nrow(nhanes) * implied_prop / sum(implied_prop),
         deff_corrected_implied_n = implied_prop * point_est$DEff.HI_CHOL / 
           sum(implied_prop * point_est$DEff.HI_CHOL) * nrow(nhanes),
         implied_pop = implied_n / sum(implied_n) * sum(pop)) |>
  select(race, avg_weight, pop, real_n = n, implied_n, deff_corrected_implied_n, implied_pop) |>
  mutate(across(everything(), round)) |>
  mutate(error1 = percent(implied_n / real_n - 1),
         error2 = percent(deff_corrected_implied_n / real_n - 1),
         error3 = percent(implied_pop / pop - 1))

# note that if you didn't have the design effects you'd only have the 
# implied_n column, which isn't very good; and even after correcting
# for design effects, race 2 is still >20% out. So from this method
# probably should give actual confidence interval as x/2, x*2

# if you want to deduce the population (implied_pop and error3) it is even
# worse because there's not only design effects but the very different
# weights in this case - because race 1 is underrepresented in the sample

# If we do this with age it works out badly because the young people's cholestrol
# is so low, so the confidence interval sizes aren't proportionate to n. We 
# can correct a bit for this if we standardise the confidence interval sizes
# by sqrt(p (1 - p))
point_est_age <- svyby(~HI_CHOL, ~agecat, design, FUN = svymean, na.rm = TRUE, deff = TRUE)
ci_age <- confint(point_est_age)

p <- point_est_age$HI_CHOL

nhanes |>
  group_by(agecat) |>
  summarise(n = n(),
            pop = sum(WTMEC2YR),
            avg_weight = mean(WTMEC2YR)) |>
  ungroup() |>
  mutate(ci_size = apply(ci_age, 1, diff) / sqrt((p * (1-p))),
         implied_prop = 1 / (ci_size ^ 2),
         implied_n = nrow(nhanes) * implied_prop / sum(implied_prop),
         deff_corrected_implied_n = implied_prop * point_est_age$DEff.HI_CHOL / 
           sum(implied_prop * point_est_age$DEff.HI_CHOL) * nrow(nhanes),
         implied_pop = implied_n / sum(implied_n) * sum(pop)) |>
  select(agecat, avg_weight, pop, real_n = n, implied_n, deff_corrected_implied_n, implied_pop) |>
  mutate(across(where(is.numeric), round)) |>
  mutate(error1 = percent(implied_n / real_n - 1),
         error2 = percent(deff_corrected_implied_n / real_n - 1),
         error3 = percent(implied_pop / pop - 1))
# this is ok for sample sizes, but we still have quite a lot of error in deducing
# the population size - this time because of the young people being oversampled
# and so hence having a different average weight

# but if we were doing this with some characteristic that isn't related to the
# sampling it is probably much better:
point_est_rnd <- svyby(~HI_CHOL, ~random_cat, design, FUN = svymean, na.rm = TRUE, deff = TRUE)
ci_rnd <- confint(point_est_rnd)

p <- point_est_rnd$HI_CHOL

nhanes |>
  group_by(random_cat) |>
  summarise(n = n(),
            pop = sum(WTMEC2YR),
            avg_weight = mean(WTMEC2YR)) |>
  ungroup() |>
  mutate(ci_size = apply(ci_rnd, 1, diff) / sqrt((p * (1-p))),
         implied_prop = 1 / (ci_size ^ 2),
         implied_n = nrow(nhanes) * implied_prop / sum(implied_prop),
         implied_pop = implied_n / sum(implied_n) * sum(pop)) |>
  select(random_cat, avg_weight, pop, real_n = n, implied_n, implied_pop) |>
  mutate(across(where(is.numeric), round)) |>
  mutate(error1 = percent(implied_n / real_n - 1),
         error3 = percent(implied_pop / pop - 1))

# however it still gets the population wrong by 20% for the smaller category...

#---------------------deducing number of transgender people----------
# https://www.abs.gov.au/articles/mental-health-findings-lgbtq-australians#:~:text=LGB%2B%20people-,Prevalence%20of%20mental%20disorders,of%20gay%20or%20lesbian%20people

d <- tribble(~p, ~low, ~high,
             43.9, 36.3, 51.5,
             42.9, 41.8, 44,
             33.1, 25.5, 40.8,
             21.3, 20.5, 22.1,
             70.6, 61.5, 79.7,
             48.1, 46.2, 50.1,
             58.8, 45.6, 72.0,
             31.1, 29.4, 32.8,
             28.5, 19.8, 37.1,
             16.5, 15.5, 17.4,
             19.6, 12.9, 26.2,
             8.5,  8,    9) |>
  mutate(across(everything(), function(x){x / 100})) |>
  mutate(issue = rep(LETTERS[1:6], each = 2)) |>
  mutate(gender_exp = rep(c("Trans", "Cis"), 6),
         ci_width = high - low,
         correction = sqrt(p * (1 - p)),
         correction = correction / mean(correction),
         ci_width_std = ci_width / correction)

d |>
  group_by(issue) |>
  summarise(ratio = ci_width_std[gender_exp == "Trans"] / 
              ci_width_std[gender_exp == "Cis"]) |>
  mutate(ratio_sq = ratio ^ 2,
         implied_prop_cis = ratio_sq / (1 + ratio_sq),
         implied_prop_trans = 1 - implied_prop_cis)

# so somewhere between 1% and 4% of the sample is trans
