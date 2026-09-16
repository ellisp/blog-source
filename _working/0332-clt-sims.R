library(nortest)
library(tidyverse)
library(actuar)
library(glue)
library(scales)

set.seed(123)

today_reps <- 10000
N <- 1e6

sim_clt <- function(
  x,
  n = 30,
  reps = today_reps,
  replace = TRUE,
  plot = TRUE,
  conf = 0.95,
  ...
) {
  true_mean <- mean(x)

  samples <- replicate(
    reps,
    sample(x = x, size = n, replace = replace),
    simplify = FALSE
  )
  means <- sapply(samples, mean)

  covered <- sapply(samples, function(s) {
    # rely on asymptotic normality to estimate a confidence interval and check
    # for coverage for each sample
    se <- stats::sd(s) / sqrt(length(s))
    ci <- mean(s) + c(-1, 1) * qnorm((1 - conf) / 2 + conf) * se
    ci[1] <= true_mean & true_mean <= ci[2]
  })

  if (plot) {
    qqnorm(means, bty = "l", ...)
    qqline(means, col = "steelblue")
    grid()
  }

  return(list(
    ad_stat = nortest::ad.test(means)$statistic,
    coverage = mean(covered)
  ))
}

qqplot1 <- function() {
  set.seed(123)
  par(font.main = 1, family = "Roboto")
  sim_clt(rnorm(N), n = 5, main = "Mean from mormal dist, n = 5")
}
# note that coverage here is still low, because using normal rather than t
# distribution. Because we don't know sigma, have to estimate it

lnpop <- exp(rnorm(N))

qqplot2 <- function() {
  par(font.main = 1, family = "Roboto")
  sim_clt(lnpop, n = 5, main = "Mean from log-normal dist, n = 5")
}
# now the coverage is low both because of unknown sigma, but more importantly
# because the shape is skewed enough the CLT hasn't really kicked in

qqplot3 <- function() {
  par(font.main = 1, family = "Roboto")
  sim_clt(lnpop, n = 100, main = "Mean from log-normal dist, n = 100")
}

svg_png(qqplot1, "../img/0332-qqplot1")
svg_png(qqplot2, "../img/0332-qqplot2")
svg_png(qqplot3, "../img/0332-qqplot3")

ns <- c(5:30, 50, 100, 300, 1000, 2000, 5000, 10000, 20000, 40000)
sims <- list()

pop_dists <- list(
  "exp(rnorm(N))" = function() exp(rnorm(N)),
  "exp(rnorm(N, sd = 2))" = function() exp(rnorm(N, sd = 2)),
  "rnorm(N)" = function() rnorm(N),
  "rcauchy(N)" = function() rcauchy(N),
  "rpareto(N, shape = 2.1, scale = 1)" = function() {
    rpareto(N, shape = 2.1, scale = 1)
  },
  "rexp(N)" = function() rexp(N),
  "c(rexp(N*0.6), exp(rnorm(N*0.4)))" = function() {
    c(rexp(N * 0.6), exp(rnorm(N * 0.4)))
  },
  "c(rnorm(N*0.9,0,1), rnorm(N*0.1,3,3))" = function() {
    c(rnorm(N * 0.9, 0, 1), rnorm(N * 0.1, 3, 3))
  }
)


all_sims <- lapply(names(pop_dists), function(lbl) {
  pop <- pop_dists[[lbl]]()

  results <- lapply(ns, function(n) sim_clt(pop, n = n, plot = FALSE))

  tibble(
    n = ns,
    ad_stat = sapply(results, `[[`, "ad_stat"),
    coverage = sapply(results, `[[`, "coverage"),
    pop_dist = lbl
  )
}) |>
  bind_rows()

normal_band <- all_sims |>
  filter(pop_dist == "rnorm(N)") |>
  pull(ad_stat) |>
  quantile(probs = c(0.025, 0.975))

p1 <- all_sims |>
  mutate(pop_dist = fct_reorder(pop_dist, ad_stat)) |>
  ggplot(aes(x = n, y = ad_stat)) +
  facet_wrap(~pop_dist, nrow = 2) +
  annotate(
    "rect",
    xmin = min(ns),
    xmax = Inf,
    ymin = normal_band[1],
    ymax = normal_band[2],
    alpha = 0.5,
    fill = "orange"
  ) +
  geom_point() +
  scale_x_log10(label = comma, breaks = ns[ns >= 30 | ns %in% c(5, 10, 20)]) +
  scale_y_log10(label = comma) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "plain", size = 9)
  ) +
  labs(
    x = "Sample size",
    y = "Anderson-Darling statistic",
    subtitle = glue(
      "Points represent average from {comma(today_reps)} simulations of given sample size. Shaded area covers 95% of values from a normal distribution."
    ),
    title = "Increasing sample size and growing effectiveness of the central limit theorem - Anderson-Darling statistic"
  )

svg_png(p1, "../img/0332-ad-stat", w = 10, h = 6)


p2 <- all_sims |>
  mutate(pop_dist = fct_reorder(pop_dist, ad_stat)) |>
  ggplot(aes(x = n, y = coverage)) +
  facet_wrap(~pop_dist, nrow = 2) +
  annotate(
    "rect",
    xmin = min(ns),
    xmax = Inf,
    ymin = 0.95,
    ymax = 1,
    alpha = 0.5,
    fill = "orange"
  ) +
  geom_point() +
  scale_x_log10(label = comma, breaks = ns[ns >= 30 | ns %in% c(5, 10, 20)]) +
  scale_y_continuous(label = percent) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "plain", size = 9)
  ) +
  labs(
    x = "Sample size",
    y = "Coverage of a 95% confidence interval based on t distribution",
    subtitle = glue(
      "Points represent average from {comma(today_reps)} simulations of given sample size. Shaded area shows 95% and higher, as desired."
    ),
    title = "Increasing sample size and growing effectiveness of the central limit theorem - confidence interval coverage"
  )

svg_png(p2, "../img/0332-coverage", w = 10, h = 6)

p3 <- p2 +
  labs(
    y = "Coverage of a 95% confidence interval based on t distribution\nY axis truncated to only start at 80%; some points excluded because of that."
  ) +
  coord_cartesian(ylim = c(0.8, 1))

svg_png(p3, "../img/0332-coverage-trunc", w = 10, h = 6)
