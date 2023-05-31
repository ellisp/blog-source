# attempt to recreate Figure 4 from
# https://journals.sagepub.com/doi/10.1177/25152459221095823
library(tidyverse)
library(ggdag)
library(patchwork)
#----------------simulating data-------------

# In the below I call a variable `z` that in the diagrams is called `C`

#' Simulate a 3 variable situation where a nuisance variable is a confounder
#' 
#' @param zx coefficient for impact of z on x
#' @param xy coefficient for impact of x on y
#' @param zy coefficient for impact of z on y
#' @param n sample size
#' @param seed random seed set for reproducibility
#' @returns a tibble of three variables x, y and z. x causes y 
#' and z is a confounder ie it impacts on both x and y
sim_confounder <- function(zx, xy = 0.15, zy = 0.5, n = 1000, seed = 123){
  set.seed(seed)
  z <- rnorm(n)
  x <- zx * z + rnorm(n)
  y <- xy * x + zy * z + rnorm(n)
  return(tibble::tibble(x, y, z))
}


#' Simulate a 3 variable situation where a nuisance variable is a confounder
#' 
#' @param xz coefficient for impact of x on z
#' @param xy coefficient for impact of x on y
#' @param yz coefficient for impact of y on z
#' @param n sample size
#' @param seed random seed set for reproducibility
#' @returns a tibble of three variables x, y and z. x causes y 
#' and z is a collider ie it is impacted on by both x and y
sim_collider <- function(xz, xy = 0.15, yz = 0.5, n = 1000, seed = 123){
  set.seed(seed)
  x <- rnorm(n)
  y <- xy * x + rnorm(n)
  z <- xz * x + yz * y + rnorm(n)
  return(tibble::tibble(x, y, z))
}

#' Simulate a 3 variable situation where a nuisance variable is a confounder
#' 
#' @param xz coefficient for impact of x on z
#' @param xy coefficient for impact of x on y
#' @param zy coefficient for impact of z on y
#' @param n sample size
#' @param seed random seed set for reproducibility
#' @returns a tibble of three variables x, y and z. x causes y 
#' and z is a mediator ie x impacts on z and z impacts on y, so some of the 
#' impact of x on y comes via z
sim_mediator <- function(xz, xy = 0.15, zy = 0.5, n = 1000, seed = 123){
  set.seed(seed)
  x <- rnorm(n)
  z <- xz * x + rnorm(n)
  y = xy * x + zy * z + rnorm(n)
  return(tibble::tibble(x, y, z))
}

# Correlations of example different datasets:
round(cor(sim_confounder(0.3, n = 10000)), 2)
round(cor(sim_collider(0.3, n = 10000)), 2)
round(cor(sim_mediator(0.3, n = 10000)), 2)
# other than xy correlation being low for the collider, you couldn't tell these apart


#-----------generate data and fit regressions, for various values of a

the_a <- seq(from= -0.7, to = 0.7, length.out = 10)
the_n <- 10000

res_conf <- lapply(the_a, sim_confounder, n = the_n) |>
  bind_rows() |>
  mutate(a = rep(the_a, each = the_n),
         var = "Confounder") |>
  group_by(a, var) |>
  summarise(`Simple coefficient` = coef(lm(y ~ x))[['x']],
            `Partial coefficient` = coef(lm(y ~ x + z))[['x']],
            `Causal effect` = 0.15)

res_coll <- lapply(the_a, sim_collider, n = the_n) |>
  bind_rows() |>
  mutate(a = rep(the_a, each = the_n),
         var = "Collider") |>
  group_by(a, var) |>
  summarise(`Simple coefficient` = coef(lm(y ~ x))[['x']],
            `Partial coefficient` = coef(lm(y ~ x + z))[['x']],
            `Causal effect` = 0.15)

res_medi <- lapply(the_a, sim_mediator, n = the_n) |>
  bind_rows() |>
  mutate(a = rep(the_a, each = the_n),
         var = "Mediator") |>
  group_by(a, var) |>
  summarise(`Simple coefficient` = coef(lm(y ~ x))[['x']],
            `Partial coefficient` = coef(lm(y ~ x + z))[['x']],
            `Causal effect` = `Simple coefficient`)

res <- bind_rows(res_conf, res_coll, res_medi) |>
  ungroup()

#------------------draw plot-------------------
the_font <- "Calibri"
tg <- guide_legend(direction = "vertical", keywidth = unit(3, "cm"))
p <- res |>
  gather(method, coefficient, -a, -var) |>
  mutate(method = fct_relevel(method, "Causal effect", after = Inf),
         var = fct_relevel(var, "Confounder")) |>
  mutate(method = fct_recode(method, "Causal effect (X causing Y)" = "Causal effect")) |>
  ggplot(aes(x = a, y = coefficient, colour = method, linetype = method)) +
  facet_wrap(~var, ncol = 1) +
  geom_line(linewidth = 1.5) +
  scale_x_continuous(breaks = seq(from = -0.7, to = 0.7, by = 0.2)) +
  scale_y_continuous(position = "right") +
  scale_linetype_manual(values = c(1, 1,2), guide = tg) +
  scale_colour_manual(values = c("yellow", "lightgreen", "black"), guide = tg) +
  labs(colour = "", linetype = "",
       y = expression("Coefficient "~beta),
       caption = "Reproducing (inexactly) a figure by Wysocki, Lawson and Rhemtulla in \n'Statistical Control Requires Causal Justification'. http://freerangestats.info") +
  theme_light(base_family = main_font) + 
  theme(plot.caption = element_text(colour = "grey50"),
        strip.text = element_text(size = rel(1), face = "bold"),
        legend.position = "top")


#------------------draw DAGs---------------
arrow_col <- "grey70"
set.seed(123)
d1 <- dagify(
  Y ~ X + C,
  X ~ C
) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges_fan(aes(label = c("a", 0.5, 0.15, NA)),
                     edge_colour = arrow_col,
                     label_colour = "black") +
  geom_dag_text(colour = "black") +
  theme_dag() 

set.seed(123)
d2 <- dagify(
  C ~ X + Y,
  Y ~ X
) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges_fan(aes(label = c(NA, "a", 0.15, 0.5)),
                     edge_colour = arrow_col,
                     label_colour = "black") +
  geom_dag_text(colour = "black") +
  theme_dag()


set.seed(123)
d3 <- dagify(
  C ~ X,
  Y ~ X + C
) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  #geom_dag_point() +
  geom_dag_edges_fan(aes(label = c(0.5, "a", 0.15, NA)),
                     edge_colour = arrow_col,
                     label_colour = "black") +
  geom_dag_text(colour = "black") +
  theme_dag()

design <- c(
  area(1,1,2,2),
  area(3,1,4,2),
  area(5,1,6,2),
  area(1,3,6,5)
)


print_p <- function(){
  print(d1 + d2 + d3 + p + 
          plot_layout(design = design) +
          plot_annotation(title = "Statistical control requires causal justification",
                          subtitle = "Only when the variable is a confounder is it correct to 'control' for it in a regression.",
                          theme = theme(text = element_text(family = the_font)))
      )
}

svg_png(print_p, "../img/0247-results-with-dags", w = 6, h = 7)
