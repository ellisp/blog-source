


# Wood, Terence, 2020, "Replication Data for The effect of geostrategic
# competition on public attitudes to aid", https://doi.org/10.7910/DVN/3VVWPL,
# Harvard Dataverse, V1, UNF:6:a3zSXQF/lkQQhkGYNqchGg== [fileUNF]

library(tidyverse)
library(haven)
library(glue)
library(stargazer)
library(kableExtra)
library(emmeans)
library(patchwork)
library(boot)
library(mice)
library(ggdag)
library(clipr)

the_caption <- "Reproduction data from Terence Wood et al, 'The Effect of Geostrategic Competition on Public Attitudes to Aid'"

aust <- read_stata("1 Australia data JEPS FINAL.dta") %>%
  mutate(treatment_group = as_factor(treatment_group))

nz <- read_stata("2 NZ data JEPS FINAL.dta") %>%
  mutate(treatment_group = as_factor(treatment)) %>%
  rename(too_much_aid = toomuchaid,
         more_to_pac = morepac,
         favour_nz = favournz)


# nz gets this error, but it is only a problem for the print method
# Error in gsub(finish, start, ..., fixed = TRUE) : 
#   input string 3 is invalid UTF-8

#-------------Australian models--------------------

aust_mods <- list()
all_response_vars <- c("too_much_aid", "more_to_pac", "favour_aus")
all_response_labs <- c("Too much aid", "More to Pacific", "Help Australia")

for(i in 1:length(all_response_vars)){
  form <- as.formula(glue("{all_response_vars[[i]]} ~ treatment_group"))
  
  aust_mods[[i]] <- lm(form, data = aust)
  
}

# Regression results:
stargazer(aust_mods[[1]], aust_mods[[2]], aust_mods[[3]], 
          type = "html",
          dep.var.labels = all_response_labs,
          title = "Regression results for Australians") %>%
  write_clip()


#---------------------New Zealand models------------------
nz_mods <- list()
all_response_vars_nz <- c("too_much_aid", "more_to_pac", "favour_nz")
all_response_labs_nz <- c("Too much aid", "More to Pacific", "Help New Zealand")

for(i in 1:length(all_response_vars_nz)){
  form <- as.formula(glue("{all_response_vars_nz[[i]]} ~ treatment_group"))
  
  nz_mods[[i]] <- lm(form, data = nz)
  
}

# Regression results:
stargazer(nz_mods[[1]], nz_mods[[2]], nz_mods[[3]], 
          type = "html",
          dep.var.labels = all_response_labs_nz,
          title = "Regression results for New Zealanders") %>%
  write_clip()


#----------Australian Contrasts--------------
rbind(
  as_tibble(pairs(emmeans(aust_mods[[1]], "treatment_group"))[3, ]),
  as_tibble(pairs(emmeans(aust_mods[[2]], "treatment_group"))[3, ]),
  as_tibble(pairs(emmeans(aust_mods[[3]], "treatment_group"))[3, ])
) %>%
  # pairs actually gives us the Measured estimate relative to Forceful; we want
  # the reverse:
  mutate(estimate = -estimate) %>%
  # shuffle some stuff for presentation:
  select(-contrast) %>%
  rename(`'Measured' minus 'Forceful'` = estimate) %>%
  mutate(response = all_response_labs) %>%
  select(response, everything()) %>%
  kable(digits = 2) %>%
  kable_styling()


#-------------margins plot, Figure 1:---------------
p3a <- plot(emmeans(lm(favour_aus ~ treatment_group, data = aust), specs = "treatment_group"),
            comparisons = TRUE) +
  labs(x = "Aid should help Australia",
       y = "")
p3b <- plot(emmeans(lm(favour_poor ~ treatment_group, data = aust), specs = "treatment_group"),
            comparisons = TRUE) +
  labs(x = "Aid should help the poor",
       y= "")

p3f <- function(){
  print(
    p3a + 
    p3b +
    plot_annotation(title = "Telling Australians about Chinese aid might make them more focused on helping the poor",
                    subtitle = "Subjects were given a description of Chinese aid that was either forceful, measured or none (control)",
                    caption = the_caption)
  )
  }

svg_png(p3f, "../img/0198-margins")

#===============extra analysis with more variables================

#--------------Direction of causality-------------
# Direct graph (not 'acyclic' because the connections to go in circles)
dagified <- dagify(aid_attitude ~ which_vignette + political_views,
                   political_views ~ income + gender + age + academic,
                   income ~ gender + age + academic + political_views,
                   academic ~ gender + age + political_views,
                   latent = "political_views",
                   outcome = "aid_attitude"
                   )

# Draw DAG:
set.seed(123)
g1 <- tidy_dagitty(dagified) %>%
  ggplot(aes(x = x, y = y , xend = xend, yend = yend)) +
  geom_dag_edges_arc(edge_colour = "grey50") +
  geom_dag_node(colour ="grey80") +
  geom_dag_text(colour = "steelblue", family = main_font) +
  theme_void(base_family = main_font) +
  labs(title = "Simplified causal diagram of factors in this experiment and views on aid",
       caption = the_caption)

g1
svg_png(g1, "../img/0198-graph")
#-----------Bootstrap and imputation---------------------

#' Imputation and regression
#' 
#' @param d data frame with the necessary columns in it
#' @param w 'weights' used to indicate which rows of d to use
my_reg <- function(d, w = 1:nrow(d), 
                   resp_var = c("too_much_aid", "more_to_pac", "favour_aus")){
  
  resp_var <- match.arg(resp_var)
  
  d_select <- d %>%
    rename(y = {{resp_var}}) %>%
    mutate(y = as.numeric(y), # eg 1 = 'Favour Aus', 0 = 'Help overseas'
           male = as.numeric(male),
           over_fifty = as.numeric(over_fifty),
           academic = as.numeric(academic),
           log_inc_pp = log(income_per_person)) %>%
    select(y,
           treatment_group,
           male,
           over_fifty,
           academic,
           log_inc_pp) 
  
  d_imputed <- complete(
    mice(d_select[w, ], 
         m = 1, 
         printFlag = FALSE,
         method = c("cart", "cart", "cart", "cart", "cart", "norm"))
    )
  
  tmp <- dim(with(d_imputed, table(y, treatment_group)))
  if(length(tmp) == 2 && tmp[1] == 2 && tmp[2] == 3){
    
    full_mod <- glm(y ~ treatment_group + male + over_fifty + academic + log_inc_pp, 
                   data = d_imputed, family = "quasibinomial")
    
    return(coef(full_mod)) 
  } else {
    return(NULL)
  }
}

# Demo use:
my_reg(aust, resp_var = "more_to_pac")

# Apply to all three different response variables, bootstrapped 999 times each
boot_reg <- lapply(all_response_vars, function(v){
  set.seed(123)
  boot(aust, my_reg, R = 999, resp_var = v)
})

boot_plots <- list()

for(j in 1:length(boot_reg)){
  x <- lapply(1:7, function(i){boot.ci(boot_reg[[j]], type = "perc", index = i)$percent[4:5]})
  
  set.seed(322)
  boot_plots[[j]] <- do.call(rbind, x)  %>%
    as.data.frame() %>%
    mutate(variable = c("Intercept", "Measured vignette re China", "Forceful vignette re China", 
                        "Male", "Over fifty", "Academic", "Log Income Per Person"),
           var_type = rep(c("doesn't matter", "Total", "Mediated"), c(1, 2, 4))) %>%
    cbind(point = my_reg(aust, resp_var = all_response_vars[[j]])) %>%
    filter(variable != "Intercept") %>%
    rename(lower = V1,
           upper = V2) %>%
    mutate(variable = fct_reorder(variable, point)) %>%
    ggplot(aes(x = point, y = variable)) +
    geom_vline(xintercept = 0, size = 2, colour = "brown", alpha = 1/4) +
    geom_segment(aes(xend = lower, x = upper, yend = variable, colour = var_type), 
                 size = 4, alpha = 0.25) +
    geom_point() +
    guides(color = guide_legend(override.aes = list(alpha = 1) ) ) +
    labs(caption = the_caption,
         x = glue("Impact (on log-odds scale) on probability of saying '{all_response_labs[j]}'"),
         y = "",
         colour = "How to interpret the effect of the variable:",
         title = "The effect of geostrategic competition on public attitudes to aid",
         subtitle = str_wrap(glue("Impact of a measured or forceful vignette about Chinese aid, and other secondary variables, on likelihood
         of supporting '{all_response_labs[j]}'"), 80))
}

svg_png(boot_plots[[1]], "../img/0198-bs-too-much")
svg_png(boot_plots[[2]], "../img/0198-more-pacific")
svg_png(boot_plots[[3]], "../img/0198-support-aus")
