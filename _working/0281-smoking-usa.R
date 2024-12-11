library(jsonlite)
library(tidyverse)
library(mgcv)
library(glmnet)
library(nlme)
library(patchwork)
library(Synth)
library(rstan)
options(mc.cores = min(8, parallel::detectCores()))

# see also https://www.aeaweb.org/articles?id=10.1257/pol.20170144
# for an alternative case study of synthetic control

conflicts_prefer(readr::col_factor)
conflicts_prefer(dplyr::collapse)
conflicts_prefer(scales::discard)
conflicts_prefer(tidyr::expand)

# Visiting this:
# https://towardsdatascience.com/causal-inference-with-synthetic-control-in-python-4a79ee636325
# which itself is basically taken straight from:
# https://matheusfacure.github.io/python-causality-handbook/15-Synthetic-Control.html

# Note that they think they are using OLS first time they
# make the weights but they're not! (OLS wouldn't work
# as p > n) - sklearn strikes again with its default of regularisation


#---------original data from the Python Causality Handbook--

orig <- read_csv("https://raw.githubusercontent.com/matheusfacure/python-causality-handbook/refs/heads/master/causal-inference-for-the-brave-and-true/data/smoking.csv")



#-------------smoking purchases download--------
fn <- snlmefn <- "smoking-usa.json"

if(!file.exists(fn)){
  download.file("https://data.cdc.gov/resource/7nwe-3aj9.json?&$where=year<2001&$limit=50000",
                destfile = fn)
}

smoking <- read_json(fn)

# number obs:
length(smoking)

# # check we got the right submeasre - 
# stopifnot(unique(sapply(smoking, function(x){x$submeasuredesc})) == 
#             "Cigarette Consumption (Pack Sales Per Capita)")


#-------------convert to rectangles-----------
smoking_all <- lapply(smoking, as_tibble) |>
  bind_rows() |>
  # geolocation has 3 elements so creates three rows for
  # each observation, so we just drop it column and take
  # distinct values to hack around this
  select(-geolocation) |>
  distinct() |>
  mutate(year = as.numeric(year),
        value = as.numeric(data_value),
        is_ca = ifelse(locationdesc == "California", "California", "Other")) |>
  arrange(locationdesc, year) 

smoking_df <- smoking_all |>
  filter(submeasuredesc %in% c(
    "Average Cost per pack",
    "Cigarette Consumption (Pack Sales Per Capita)"
  ))

stopifnot(length(unique(smoking_df$submeasuredesc)) == 2)

# which are the states that had "similar measures"?
# it's not obvious at all from the taxes. In fact I don't
# like these exclusions, but  I'm just going to do it 
# the same way as the original for now
orig70 <- orig |>
  filter(year %in% 1970:1972) |>
  select(state, cigsale, year) |>
  mutate(cigsale = round(cigsale, 1)) |>
  spread(year, cigsale)

state_lookup <- smoking_df |>
  filter(year %in% 1970:1972 & 
           submeasuredesc == "Cigarette Consumption (Pack Sales Per Capita)") |>
  select(locationdesc, locationabbr, cigs_pp = value, year) |>
  mutate(cigs_pp = round(cigs_pp, 1)) |>
  spread(year, cigs_pp) |>
  left_join(orig70, by = join_by(`1970`, `1971`, `1972`)) |>
  select(state, locationdesc, locationabbr)

non_matches <- state_lookup |>
  filter(is.na(state)) |>
  pull(locationdesc)

smoking_df <- filter(smoking_df, !locationdesc %in% non_matches)

# check we have the right number of states as the original
stopifnot(length(unique(smoking_df$locationdesc)) == 
            length(unique(orig$state)))

cigsales <- smoking_df |>
  filter(submeasuredesc == "Cigarette Consumption (Pack Sales Per Capita)")  |>
  mutate(cigs_pp = value)

# scale the prices so they are the same mean and SD as sales, otherwise they
# have no impact on our later regression to make weights for a synthetic control
smoking_df <- smoking_df |>
  group_by(submeasuredesc) |>
  mutate(value = scale(value) * sd(cigsales$value) + mean(cigsales$value))
# Note that in the causality book he didn't need to do this becuase his
# orig$retprice data already has a mean that is similar to the cigarette sales

# I discovered that my price of packs is obbiouvsly different to his
# so the next line basically discards my CDC data to use the one in the book,
# for reproducibility (but not replicability!) reasons
smoking_df <- orig |>
  select(state, year, cigsale, retprice) |>
  left_join(state_lookup, by = "state") |>
  select(-state) |>
  gather(submeasuredesc, value, -year, -locationdesc, -locationabbr) |>
  mutate(submeasuredesc = case_when(
    submeasuredesc == "cigsale" ~ "Cigarette Consumption (Pack Sales Per Capita)",
    submeasuredesc == "retprice" ~ "Average Cost per pack"
  )) |>
  mutate(is_ca = ifelse(locationdesc == "California", "California", "Other"))

cigsales <- smoking_df |>
  filter(submeasuredesc == "Cigarette Consumption (Pack Sales Per Capita)")  |>
  mutate(cigs_pp = value)


cigsales |>
  ggplot(aes(x = year, y = cigs_pp, group = locationdesc, colour = is_ca)) +
  geom_line() +
  # redraw just hte California line so it is at the forefront
  geom_line(data = filter(cigsales, is_ca == "California"), size = 2) +
  geom_vline(xintercept = 1988, linetype = 2) +
  scale_colour_manual(values = c(California = "blue", Other = "red"))


cigsales |>
  group_by(year, is_ca) |>
  summarise(cigs_pp = mean(cigs_pp)) |>
  ggplot(aes(x = year, y = cigs_pp, colour = is_ca)) +
  geom_line() +
  scale_colour_manual(values = c(California = "blue", Other = "red"))


smoking_w <- smoking_df |>
  select(locationabbr, year, value, submeasuredesc) |>
  group_by(year, submeasuredesc) |>
  mutate(state_avg = mean(value[locationabbr != "CA"])) |> 
  spread(locationabbr, value) |>
  mutate(post_88 = as.numeric(year > 1988)) |>
  ungroup()
  

cigsales_w <- smoking_w |>
  filter(submeasuredesc == "Cigarette Consumption (Pack Sales Per Capita)") |>
  select(-submeasuredesc)

model1 <- lm(CA ~ state_avg + post_88, data = cigsales_w)
anova(model1)

# the diagnostics are ok, not brilliant - bit of non-normal shape
par(mfrow = c(2, 2), bty = "l")
plot(model1)

ggplot(tibble(x = residuals(model1)), aes(x = x)) +
  geom_density() +
  geom_rug()
# if we were worried about this we might either transform
# both CA and the state average as logarithms; or you
# could redo it as a GLM with a quasipoisson distribution

# Let's consider this as a starting point and say how to improve it?
# so the problems with this naive comparison with the state average:
# - autocorrelated residuals
# - doesn't allow a gradual change over time of the relationship
#   of California to the other states (this is realted to the autocoreelation issue)
# - non-normal residuals
# - gives equal weight to all the states

# Only on that last point doe sthe "synthetic control" method
# have anything to say. But I think this last point is literally the 
# least of the problems

# we can fix the autocorrelation problem easily
model2 <- gls(CA ~ state_avg + post_88, data = cigsales_w,
              correlation = corAR1())
summary(model2)
# no longer significant

# or we can fix the problem of a gradually changing
# relationship of California to the rest by adding
# a spline on year
model3 <- gam(CA ~ s(year) + post_88 + state_avg, data = cigsales_w)
anova(model3)
summary(model3)
# similar result to allowing for autocorrelation.

# of course we should do *both*

model4 <- gamm(CA ~ s(year) + post_88 + state_avg, 
               data = cigsales_w, correlation = corAR1(form = ~year))

# not sure which of these two is the right one to use, but both
# say the post_88 number is not significant
summary(model4$gam)
summary(model4$lme)


#===================with the more sophisticated synthetic control======  
# here we are basically getting a set of weights for a weighted average
# and we are choosing the weights based on some combination of the states
# that add up to our state. I don't get why we would do this for both cigarette
# sales and for price though! because why would being a good 'donor' state
# for predicting your own states cigarette prices mean you are good at creating
# cigarette sales estimates. Surely if we want to forecast cigarette sales,
# we should train the model just on cigarette sales.

x <- smoking_w |>
  filter(post_88 == 0) |>
  select(AL:WY) |>
  select(-CA) |>
  as.matrix()

y <- smoking_w |>
  filter(post_88 == 0) |>
  pull(CA)



# alpha = 1 means Lasso; this is different to the alpha
# in sklearn.Lasso which is closer lambda in glmnet

# Try multiple values of lambda and use cross-validation to get the optimal
# amount of regularisation
synth_mod_ols <- lm(y ~ x - 1)

synth_mod_lasso <- cv.glmnet(x = x, y = y, alpha = 1, 
                          standardize = FALSE, intercept = FALSE,
                          grouped = FALSE, lower.limits = 0)


synth_mod_ridge <- cv.glmnet(x = x, y = y, alpha = 0, 
                             standardize = FALSE, intercept = FALSE,
                             grouped = FALSE, lower.limits = 0, upper.limits = 1)

# this next model is one with absolutely minimal regularization, but unlike
# the OLS just constrains the coefficients to be between zero and one. This
# is the result that comes out most looking like the one in the Causality
# book
synth_mod_ridge2 <- glmnet(x = x, y = y, alpha = 0, lambda = 0.001,
                            standardize = FALSE, intercept = FALSE,
                            grouped = FALSE, lower.limits = 0, upper.limits = 1)



all_coefs <- tibble(state = rownames(coef(synth_mod_lasso)),
       lasso = coef(synth_mod_lasso)[,1],
       ridge = coef(synth_mod_ridge)[,1],
       ridge2 = coef(synth_mod_ridge2)[,1],
       ols = c(0, coef(synth_mod_ols))) |>
  # intercept is still in the coef() even though it was set to zero
  filter(state != "(Intercept)") |>
  arrange(desc(ridge)) 

GGally::ggpairs(select(all_coefs, -state))

all_coefs |>
  ggplot(aes(x = lasso, y = ridge, label = state)) +
  geom_text() +
  labs(x = "Coefficient from Lasso model",
       y = "Coefficient from ridge regression model",
       title = "Comparison of two models to create a synthetic control similar to California",
       subtitle = "Cigarette packs purchased per capita 1970 to 1988")

xnew <- smoking_w |>
  select(AL:WY) |>
  select(-CA) |>
  as.matrix()

smoking_w2 <- smoking_w |>
  mutate(synth_lasso = predict(synth_mod_lasso, xnew),
         synth_ridge = predict(synth_mod_ridge, xnew),
         synth_ridge2 = predict(synth_mod_ridge2, xnew),
         synth_ols = xnew %*% coef(synth_mod_ols))

cigsales_w2 <- smoking_w2 |>
  filter(submeasuredesc == "Cigarette Consumption (Pack Sales Per Capita)") |>
  select(-submeasuredesc)


cigsales_w2 |>
  select(synth_lasso, synth_ridge, state_avg, synth_ols, synth_ridge2, year, CA) |>
  rename(`Synthetic control weighted\naverage from Lasso` = synth_lasso,
         `Synthetic control weighted\naverage from ridge regression` = synth_ridge,
         `Simple average` = state_avg,
         California = CA) |> 
  gather(variable, value, -year) |>
  mutate(variable = fct_reorder(variable, -value, .fun = last)) |>
  ggplot(aes(x = year, y = value, colour = variable)) +
  annotate("rect", xmin = 1988.5, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_line(linewidth = 2) +
  theme(legend.position = "right") +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "",
       y = "Cigarette packs per capita")


cigsales_w2 |>
  select(state_avg, synth_ridge2, year, CA) |>
  rename(`Synthetic control weighted\naverage from lightly regularized regression` = synth_ridge2,
         `Simple average` = state_avg,
         California = CA) |> 
  gather(variable, value, -year) |>
  mutate(variable = fct_reorder(variable, -value, .fun = last)) |>
  ggplot(aes(x = year, y = value, colour = variable)) +
  annotate("rect", xmin = 1988.5, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_line(linewidth = 2) +
  theme(legend.position = "right") +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "",
       y = "Cigarette packs per capita")


model1 <- lm(CA ~ synth_ridge + post_88, data = cigsales_w2)
anova(model1)

model2 <- gls(CA ~ synth_ridge + post_88, data = cigsales_w2,
              correlation = corAR1())
summary(model2)


model1 <- lm(CA ~ synth_lasso + post_88, data = cigsales_w2)
anova(model1)

model2 <- gls(CA ~ synth_lasso + post_88, data = cigsales_w2,
              correlation = corAR1())
summary(model2)


model1 <- lm(CA ~ synth_ridge2 + post_88, data = cigsales_w2)
anova(model1)

model2 <- gls(CA ~ synth_ridge2 + post_88, data = cigsales_w2,
              correlation = corAR1())
summary(model2)
# hmmm! so now the post_88 is significant. I may need to rethink things!




model4 <- gamm(CA ~ s(year) + post_88 + synth_ridge2, 
               data = cigsales_w2, correlation = corAR1(form = ~year))

# not sure which of these two is the right one to use, but both
# say the post_88 number is not significant
summary(model4$gam)
summary(model4$lme)


# in other words, it doesn't matter how we create the synthetic control - as a
# simple average, from ridge regression or from Lasso - what matters is just
#
names(cigsales_w2)
cigsales_w2 |>
  mutate(diff_ca = CA - synth_ridge2) |>
  ggplot(aes(x = year, y = diff_ca)) +
  annotate("rect", xmin = 1988.5, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_line()


#-----------------repeating for all states------------
# first turn the approach above into a function

calc_state <- function(the_state, FUN = cv.glmnet, alpha = 0, lambda = NULL,
                       standardize = TRUE, just_response = FALSE){
  smoking_w$state_avg <- NULL
  
  if(identical(FUN, glmnet) & is.null(lambda)){
    warning("Setting lambda to zero i.e. no regularization, just constraint on coefs being > 0")
    lambda <- 0
  }

  if(identical(FUN, glmnet) & length(lambda) != 1){
    stop("If using glmnet, must specify a single value of lambda to use")
  }
  
  if(just_response){
    # only use the ultimate response variable in determining weights
    smoking_w <- smoking_w |>
      filter(submeasuredesc == "Cigarette Consumption (Pack Sales Per Capita)") 
  }
  
  if(standardize){
    cigs <- smoking_w |>
      filter(submeasuredesc == "Cigarette Consumption (Pack Sales Per Capita)") |>
      select(AL:WY) |>
      as.matrix()
    
    smoking_w <- smoking_w |>
      gather(state, value, -year, -submeasuredesc, -post_88) |>
      group_by(submeasuredesc) |>
      mutate(value = (value - mean(value)) / sd(value),
             value = value * sd(cigs) + mean(cigs)) |>
      spread(state, value) |>
      ungroup()
  }
  
  x <- smoking_w |>
    filter(post_88 == 0) |>
    select(AL:WY) 
  
  x[, the_state] <- NULL
  x <- as.matrix(x)
  
  y <- smoking_w |>
    filter(post_88 == 0) |>
    pull(the_state)
  
  
  synth_mod <- FUN(x = x, y = y, alpha = alpha, lambda = lambda,
                             standardize = FALSE, intercept = FALSE,
                             grouped = FALSE, lower.limits = 0)
  
  newx <- smoking_w |>
    select(AL:WY) 
  
  newx[, the_state] <- NULL
  newx <- as.matrix(newx)
  smoking_w$synth <- as.numeric(predict(synth_mod, newx))
  smoking_w$actual <- smoking_w[[the_state]]

  cigsales_w2 <- smoking_w |>
    filter(submeasuredesc == "Cigarette Consumption (Pack Sales Per Capita)") |>
    select(-submeasuredesc)
  
  net_change <- cigsales_w2 |>
    filter(post_88 == 1) |>
    summarise(d = sum(actual - synth)) |>
    pull(d)
  
  output_df <- cigsales_w2 |>
    select(synth, actual, year) |>
    mutate(state = the_state)  
  
  return(list(output_df = output_df, 
              net_change = net_change,
              coefs = coef(synth_mod)))
}

# constrained coefs with no regularisation, no standardisation
p1 <- calc_state("CA", standardize = FALSE, FUN = glmnet, lambda = 0)[[1]] |>
  gather(variable, value, -year, -state) |>
  ggplot(aes(x = year, y = value, colour = variable)) +
  geom_line()

# constrained coefs with no regularisation, this time with standardisation  
p2 <- calc_state("CA", standardize = TRUE, FUN = glmnet, lambda = 0)[[1]] |>
  gather(variable, value, -year, -state) |>
  ggplot(aes(x = year, y = value, colour = variable)) +
  geom_line()

p1 + p2


# Lasso, standardised
calc_state("CA", alpha = 1)[[1]] |>
  gather(variable, value, -year, -state) |>
  ggplot(aes(x = year, y = value, colour = variable)) +
  geom_line()

# Ridge regression, standardised
calc_state("CA", alpha = 0)[[1]] |>
  gather(variable, value, -year, -state) |>
  ggplot(aes(x = year, y = value, colour = variable)) +
  geom_line()

# ridge regression, only using packs in the choice of weights for synthetic control
calc_state("CA", alpha = 0)[[1]] |>
  gather(variable, value, -year, -state) |>
  ggplot(aes(x = year, y = value, colour = variable)) +
  geom_line()

# lasso, only using packs in the choice of weights for synthetic control
calc_state("CA", alpha = 1)[[1]] |>
  gather(variable, value, -year, -state) |>
  ggplot(aes(x = year, y = value, colour = variable)) +
  geom_line()

# sum of the coefficients for New Hampshire, biggest buying state
sum(calc_state("NH")[[3]])
sum(calc_state("KY")[[3]])
sum(calc_state("CA")[[3]])


state_results <- lapply(unique(smoking_df$locationabbr), function(x){
  calc_state(x, FUN = cv.glmnet, alpha = 1, just_response = TRUE)
})

state_res_d <- lapply(state_results, function(x){x[[1]]}) |>
  bind_rows() |>
  mutate(difference = actual - synth) 

state_res_d |>
  ggplot(aes(x = year, y = difference, colour = state)) +
  annotate("rect", xmin = 1988.5, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_line() +
  geom_line(data = filter(state_res_d, state == "CA"), colour = "black") +
  geom_text(data = filter(state_res_d, year %in% range(year) | 
                            difference %in% range(difference) |
                          difference < -25), 
            aes(label = state),
            hjust = 0, nudge_x = 0.5) +
  theme(legend.position = "none")

state_res_d |>
  group_by(state) |>
  summarise(difference = sum(difference)) |>
  arrange(difference)

state_res_d |>
  filter(year > 1988) |>
  group_by(year) |>
  mutate(p = rank(difference),
         p = p /max(p)) |>
  group_by(state) |>
  summarise(p = mean(p)) |>
  arrange(p)

# so my objections to how it is done in the book:
# - no need to make the coefficients add to 1 and this just means it will only work with average states
# - shouldn't just remove the states it doesn't work for
# - in fact there's nothing wrong with a weight more than 1 (alhtough it's not
#   needed, even for NH, so long as the total coefficients add to 1.4)
# - he doesn't do enough regularising so I am pretty sure he is overfitting

#----------------relationship of original retail price to that from CDC--

orig |>
  filter(california) |>
  left_join(select(filter(smoking_w, submeasuredesc == "Average Cost per pack"), 
                   year, CA)) |>
  mutate(CA_sc = as.numeric(scale(CA)) * sd(retprice) + mean(retprice)) |>
  select(retprice, CA, CA_sc) |>
  ggplot(aes(x = retprice, y = CA_sc)) +
  geom_point() +
  labs(x = "Retail price in Causality Book's data",
       y = "Cost per pack in CDC data\n(scaled to match original)") +
  geom_abline(slope = 1, intercept = 0)


#---------------version with Stan--------------

stan_data <- list(
  N = length(y),
  K = ncol(x),
  x = x,
  y = y,
  start_props = rep(1 / ncol(x), ncol(x))
)
conflicts_prefer(rstan::extract)
conflicts_prefer(jsonlite::flatten)
conflicts_prefer(extrafont::font_install)
conflicts_prefer(dplyr::lag)
conflicts_prefer(scales::modulus_trans)
conflicts_prefer(tidyr::pack)
conflicts_prefer(jsonlite::prettify)
conflicts_prefer(Matrix::unpack)

stan_res <- stan(file= "0281-smoking.stan", data = stan_data)


sum(apply(extract(stan_res, "b")$b,2, mean))
plot(0:100/100, dbeta(0:100/100, 1, 5), type = "l")

#--------------version with synth package-----------




