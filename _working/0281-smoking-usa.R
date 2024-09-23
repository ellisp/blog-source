library(jsonlite)
library(tidyverse)
library(mgcv)
library(glmnet)
library(nlme)

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
  download.file("https://data.cdc.gov/resource/7nwe-3aj9.json?submeasureid=453CGS&$where=year<2001&$limit=5000",
                destfile = fn)
}

smoking <- read_json(fn)


# number obs:
length(smoking)

# check we got the right submeasre - 
stopifnot(unique(sapply(smoking, function(x){x$submeasuredesc})) == 
            "Cigarette Consumption (Pack Sales Per Capita)")

#--------smoking tax download-----------
fn2 <- snlmefn <- "smoking-tax-usa.json"

if(!file.exists(fn2)){
  download.file("https://data.cdc.gov/resource/7nwe-3aj9.json?submeasureid=451CGS&$where=year<2001&$limit=5000",
                destfile = fn2)
}

smoking_tax <- read_json(fn2)
# check we got the right submeasre - 
stopifnot(unique(sapply(smoking_tax, function(x){x$submeasuredesc})) == 
            "State Tax per pack")

tax_df <- lapply(smoking_tax, as_tibble) |>
  bind_rows() |>
  # geolocation has 3 elements so creates three rows for
  # each observation, so we just drop it column and take
  # distinct values to hack around this
  select(-geolocation)  |>
  distinct()  |>
  mutate(tax_per_pack = as.numeric(data_value),
         year = as.numeric(year))|>
  select(locationdesc, year, tax_per_pack)

tax_df |>
  filter(year == max(year)) |>
  group_by(locationdesc) |>
  summarise(avg_tax = mean(as.numeric(tax_per_pack))) |>
  arrange(desc(avg_tax)) |> View()

tax_df |>
  ggplot(aes(x = year, y = data_value, group = locationdesc)) +
  geom_line()

#-------------convert to rectangles-----------
smoking_df <- lapply(smoking, as_tibble) |>
  bind_rows() |>
  # geolocation has 3 elements so creates three rows for
  # each observation, so we just drop it column and take
  # distinct values to hack around this
  select(-geolocation) |>
  distinct() |>
  mutate(year = as.numeric(year),
        cigs_pp = as.numeric(data_value),
        is_ca = ifelse(locationdesc == "California", "California", "Other")) |>
  left_join(tax_df, by = c("year", "locationdesc")) |>
  arrange(locationdesc, year)

smoking_df  |>
  mutate(locationdesc = fct_reorder(locationdesc, cigs_pp))   |>
  ggplot(aes(x = tax_per_pack, y = cigs_pp, colour = year)) +
  geom_path()  +
  facet_wrap(~locationdesc) +
  scale_colour_viridis_c(direction = -1) +
#  geom_point() +
  scale_x_log10(label = dollar) + 
  scale_y_log10()

# to do - which are the states that had "similar measures"?
# it's not obvious at all from the taxes
orig70 <- orig |>
  filter(year %in% 1970:197) |>
  select(state, cigsale, year) |>
  mutate(cigsale = round(cigsale, 1)) |>
  spread(year, cigsale)

non_matches <- smoking_df |>
  filter(year %in% 1970:1972) |>
  select(locationdesc, cigs_pp, year) |>
  mutate(cigs_pp = round(cigs_pp, 1)) |>
  spread(year, cigs_pp) |>
  anti_join(orig70)
length(unique(smoking_df$locationdesc))
length(unique(orig$state))

View(matches)
smoking_df |>
  ggplot(aes(x = year, y = data_value, group = locationdesc, colour = is_ca)) +
  geom_line() +
  # redraw just hte California line so it is at the forefront
  geom_line(data = filter(smoking_df, is_ca == "California"), size = 2) +
  geom_vline(xintercept = 1988, linetype = 2)


smoking_df |>
  group_by(year, is_ca) |>
  summarise(data_value = mean(data_value)) |>
  ggplot(aes(x = year, y = data_value, colour = is_ca)) +
  geom_line()

smoking_w <- smoking_df |>
  select(locationabbr, year, data_value) |>
  group_by(year) %>% 
  mutate(state_avg = mean(data_value[locationabbr != "CA"])) %>% 
  spread(locationabbr, data_value) |>
  mutate(post_88 = as.numeric(year > 1988))


model1 <- lm(CA ~ state_avg + post_88, data = smoking_w)
anova(model1)
par(mfrow = c(2, 2), bty = "l")
plot(model1)

# Let's consider this as a starting point and say how to improve it?
# so the problems with this naive comparison with the state average:
# - autocorrelated residuals
# - doesn't allow a gradual change over time of the relationship
#   of California to the other states (this is realted to the autocoreelation issue)
# - uses all the states, doesn't exclude those that had similar intervnetions
# - gives equal weight to all the states

# Only on that last point doe sthe "synthetic control" method
# have anything to say

# we can fix the autocorrelation problem easily
model2 <- gls(CA ~ state_avg + post_88, data = smoking_w,
              correlation = corAR1())
summary(model2)
# no longer significant

# or we can fix the problem of a gradually changing
# relationship of California to the rest by adding
# a spline on year
model3 <- gam(CA ~ s(year) + post_88 + state_avg, data = smoking_w)
anova(model3)
summary(model3)
# similar result to allowing for autocorrelation.

# of course we should do *both*

model4 <- # whatever you do wiht a GAM to have autocorrelation...
plot(model)
dim(smoking_w)
range(smoking_w$year)

names(smoking_w)
