# Follows on from 0286-voting-and-depression
# extra stuff on spatial side

#----------roll-your-own lag3 spatial autocorrelation----------

counties <- select(combined2, county_fips, x, y)

# find the three counties shortest distance from each country
county_pairs <- expand_grid(from = counties$county_fips,
                            to = counties$county_fips) |>
  filter(from != to) |>
  left_join(counties, by =c("from" = "county_fips")) |>
  rename(fx = x, fy = y) |>
  left_join(counties, by =c("to" = "county_fips")) |>
  rename(tx = x, ty = y) |>
  mutate(distance = sqrt((fx - tx) ^ 2 + (fy - ty) ^ 2))  |>
  group_by(from) |>
  arrange(distance) |>
  slice(1:3) |>
  ungroup()
  
# combine with the residuals of the model with no spatial autocorrelation,
# but it does have
combined3  <- county_pairs |>
  select(from, to) |>
  left_join(mutate(select(combined2, county_fips),
                   res = residuals(model5, type = "response")), 
            by = c("to" = "county_fips")) |>
  mutate(lag = rep(c("lag1", "lag2", "lag3"), n() / 3)) |> 
  select(-to) |>
  spread(lag, res) |>
  rename(county_fips = from) |>
  left_join(combined4, by = "county_fips")
              
  
combined3 |>
  select(per_gop,lag1:lag3) |>
  ggpairs()


# thought - should we have logit(lag1), or just on the ordinary scale?
# or could use a Gaussian family and identity link
model8 <- gam(per_gop ~ cpe +
                lag1 + lag2 + lag3 + s(state_name, bs = 're') , 
              family = quasibinomial, weights = total_votes,
              data = combined3)

summary(model5)
summary(model6)
summary(model7)
summary(model8)

c(nothing = as.numeric(coef(model5)['cpe']),
  rubber = as.numeric(coef(model6)['cpe']),
  rubber_race = as.numeric(coef(model7)['cpe']),
  three_lags = as.numeric(coef(model8)['cpe'])
) |> cbind()


#---------------based on coordinates------------
# not really happy with using a home-made link here

logit <- function(p){
  log(p / (1 -p))
}

model9 <- lme(logit(per_gop) ~ cpe,
              random = ~ 1 | state_name, 
              data = combined3,
              weights = ~total_votes)

# this tutorial useful:
# https://www.flutterbys.com.au/stats/tut/tut8.4a.html

# should be able to use a variaogram to help decide what 
# correlation structure to use, but it's a bit tricky!

# so there's a *lot* of spatial correlation here
plot(Variogram(model9, form = ~ x + y, resType = "normalized"))
# difficult to judge from this shape what to use though, so advice is
# to fit with each different correlation structure and choose that with
# best AIC

model10a <- lme(logit(per_gop) ~ cpe,
              random = ~ 1 | state_name,
              correlation = corExp(form = ~x + y),
              data = combined3,
              weights = ~total_votes)

model10b <- update(model10a, correlation = corGaus(form = ~x + y))
model10c <- update(model10a, correlation = corLin(form = ~x + y))
model10d <- update(model10a, correlation = corRatio(form = ~x + y))
model10e <- update(model10a, correlation = corSpher(form = ~x + y))

AIC(model9, model10a, model10b, model10c, model10d, model10e)
# Ratio has the lowest AIC so we will stick with that


# finally, th emodel with spatial autocorrelation AND the
# race variables
model11 <- lme(logit(per_gop) ~ cpe + white_alone + hispanic + hispanic_multi,
               random = ~ 1 | state_name,
               correlation = corRatio(form = ~x + y),
               data = combined4,
               weights = ~total_votes)

summary(model11)

#--------------with mgcv----------------
# if we use mgcv::gamm we are able to still have the state level,
# and the rubber mat, AND spatial autocorrelation. It's just a bit
# slow, because AFAICT, it iterates between gam and lme a bit like
# I've been doing by hand here.
model12 <- gamm(per_gop ~ cpe + white_alone + hispanic + hispanic_multi + 
                 s(x, y) + s(state_name, bs = 're') , 
               family = quasibinomial, weights = total_votes,
               data = combined4)

summary(model12$lme)
summary(model12$gam)

# this is too slow:
# model13 <- gamm(per_gop ~ cpe + white_alone + hispanic + hispanic_multi + 
#                   s(x, y) + s(state_name, bs = 're') , 
#                 family = quasibinomial, weights = total_votes,
#                 correlation = corRatio(form = ~x + y),
#                 data = combined4)


# note might be better to give the state_name random effect
# as an argument random = list(~ 1 | state_name), and then
# the correlation could have that grouping too (might speed things up)

# So this is actually the full, best model
model14 <- gamm(per_gop ~ cpe + white_alone + hispanic + hispanic_multi + 
                  s(x, y), 
                random = list(state_name = ~1),
                family = quasibinomial, weights = total_votes,
                correlation = corRatio(form = ~x + y),
                data = combined4)

summary(model14$lme)
summary(model14$gam)

plot(model14$gam, scheme = 1)


# same model but no race variables
model15 <- gamm(per_gop ~ cpe + 
                  s(x, y), 
                random = list(state_name = ~1),
                family = quasibinomial, weights = total_votes,
                correlation = corRatio(form = ~x + y),
                data = combined4)
summary(model15$gam)
summary(model15$lme)
# cpe still significant at this point. so it's really clear
# it's the race variables that make a big change here

# even simpler model, this should be the same result as model6
model16 <- gamm(per_gop ~ cpe + 
                  s(x, y), 
                random = list(state_name = ~1),
                family = quasibinomial, weights = total_votes,
                data = ungroup(combined4))
# and they're pretty similar:

summary(model16$lme)$coefficients$fixed
summary(model6)$p.table

# but I don't understand the random effects here. What is g?
ranef(model16$lme)





# model with no rubber mat but does have spatial correlation
# This should be essentially similar to model11, but note
# that with model11 I used logit() on the fly rather than via
# a link function in a glm, which this is doing better at 
# approximating
model17 <- gamm(per_gop ~ cpe + white_alone + hispanic + hispanic_multi, 
                random = list(state_name = ~1),
                correlation = corRatio(form = ~x + y),
                family = quasibinomial, weights = total_votes,
                data = combined4)

# it's still quite different, but the significance tests come out the same
summary(model17$lme)$coefficients$fixed
summary(model11)$coefficients$fixed



#------------based on contiguous counties--------------





