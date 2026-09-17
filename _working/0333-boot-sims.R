library(tidyverse)
library(actuar)
library(glue)
library(scales)
library(boot)

# set the below to TRUE if running for the first time
run_sims <- FALSE

set.seed(123)

today_reps <- 1000
N <- 1e6

sim_clt <- function(
  x,
  n = 30,
  reps = today_reps,
  replace = TRUE,
  conf = 0.95,
  boot_R = 3001,
  ...
) {
  true_mean <- mean(x)

  samples <- replicate(
    reps,
    sample(x = x, size = n, replace = replace),
    simplify = FALSE
  )
  means <- sapply(samples, mean)

  covered_clt <- sapply(samples, function(s) {
    # rely on asymptotic normality to estimate a confidence interval and check
    # for coverage for each sample
    se <- stats::sd(s) / sqrt(length(s))
    ci <- mean(s) + c(-1, 1) * qnorm((1 - conf) / 2 + conf) * se
    ci[1] <= true_mean & true_mean <= ci[2]
  })

  covered_boot <- sapply(samples, function(s) {
    b <- boot::boot(
      data = s,
      statistic = function(x, w) {
        mean(x[w])
      },
      R = boot_R
    )
    ci_boot_res <- boot::boot.ci(b, conf = conf, type = "bca")
    ci_boot <- ci_boot_res$bca[4:5]
    ci_boot[1] <= true_mean & true_mean <= ci_boot[2]
  })

  return(list(
    coverage_clt = mean(covered_clt),
    coverage_boot = mean(covered_boot)
  ))
}

pops <- list(
  exp(rnorm(N)),
  exp(rnorm(N, sd = 2)),
  exp(rexp(N, rate = 2))
)

ns <- c(10, 30, 200, 1000)

if (run_sims) {
  results <- expand_grid(pop = 1:3, n = ns) |>
    mutate(coverage_clt = NA, coverage_boot = NA)

  # this - obviously when you think about what it's doing - will take a long time
  # (~2 hours) to run. It's embarassingly parallel so could consider parallelising
  # it easily enough, but there is a lot of demands on memory so for my laptop is
  # probably not going to be worth trying this as the machine wouldn't be able to
  # do multiple goes of the 3000 rep bootstrap, 1000 rep simulation from a 1e6
  # population at once.
  for (i in 1:nrow(results)) {
    cat(i)
    param <- results[i, ]
    tmp <- sim_clt(pops[[param$pop]], n = param$n)
    results[i, ]$coverage_clt <- tmp$coverage_clt
    results[i, ]$coverage_boot <- tmp$coverage_boot
  }

  save(results, file = glue("0333-boot-results-{Sys.Date()}.rda"))
} else {
  lf <- sort(
    list.files(pattern = "0333-boot-results.*\\.rda$"),
    decreasing = TRUE
  )
  load(lf[1])
}


pop_labs <- c("log normal(0,1)", "log normal(0,2)", "exponential(2)")
p <- results |>
  mutate(lab = pop_labs[pop]) |>
  mutate(lab = fct_reorder(lab, coverage_boot)) |>
  ggplot(aes(x = coverage_clt, y = coverage_boot, colour = lab)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey50") +
  geom_point(size = 2) +
  geom_text_repel(aes(label = comma(n)), seed = 123, alpha = 0.5) +
  coord_equal() +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = percent) +
  labs(
    x = "Confidence interval from asymptotic normality includes the mean",
    y = "Confidence interval from BCa bootstrap includes the mean",
    title = "Bootstrap outperforms asymptotic normality assumption with smaller n.",
    subtitle = "Labelled numbers indicate sample sizes. Diagonal line shows equal performance.",
    colour = "Population distribution:"
  )

svg_png(p, "../img/0333-sims-results", w = 9, h = 6)

# Claude advises: The percentile interval is only accurate when the bootstrap
# distribution of the statistic is symmetric (or can be made so by a monotone
# transformation). With n = 30 draws from a heavily right-skewed population, the
# bootstrap distribution of the mean is itself skewed, so the percentile
# interval inherits that bias — coverage tends to be asymmetric and generally
# below nominal, in the same direction the CLT-based interval already fails.

# BCa adjusts the percentile cutoffs using a bias-correction term (how far the
# bootstrap median of the statistic sits from the original estimate) and an
# acceleration term (estimated via jackknife, capturing how the standard error
# changes across the range of the statistic — i.e., skewness). That's exactly
# the failure mode being probed here, so it should show a real improvement over
# percentile at n = 30 from a lognormal-type population.
