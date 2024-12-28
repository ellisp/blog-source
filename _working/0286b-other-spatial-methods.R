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
combined5  <- county_pairs |>
  select(from, to) |>
  left_join(mutate(select(combined4, county_fips),
                   res = residuals(model5, type = "response")), 
            by = c("to" = "county_fips")) |>
  mutate(lag = rep(c("lag1", "lag2", "lag3"), n() / 3)) |> 
  select(-to) |>
  spread(lag, res) |>
  rename(county_fips = from) |>
  left_join(combined4, by = "county_fips")
              
  
combined5 |>
  select(per_gop,lag1:lag3) |>
  ggpairs()


# thought - should we have logit(lag1), or just on the ordinary scale?
# or could use a Gaussian family and identity link
model8 <- gam(per_gop ~ cpe +
                white_alone + hispanic + hispanic_multi + 
                lag1 + lag2 + lag3 + s(state_name, bs = 're') , 
              family = quasibinomial, weights = total_votes,
              data = combined5)

summary(model5)
summary(model6)
summary(model7)
summary(model8)

c(nothing = as.numeric(coef(model5)['cpe']),
  rubber = as.numeric(coef(model6)['cpe']),
  rubber_race = as.numeric(coef(model7)['cpe']),
  three_lags_race = as.numeric(coef(model8)['cpe'])
) |> cbind()


#---------------based on coordinates------------










#------------based on contiguous counties--------------





