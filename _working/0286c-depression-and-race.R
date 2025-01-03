library(GGally)
library(ggdag)
library(patchwork)
library(kableExtra)

# you need to have run the code from the previous blog first

if(!exists("combined2")){
  source("0286-voting-and-depression.R")
}

# three things wrong with the first version:
# should have random slopes not just intercepts
# better treatment of spatial autocorrelation
# add in whiteness

#---------------moving from gam to gamm--------------
# start with the same model as our final one in the last post, but estimated differently:
model6b <- gamm(per_gop ~ cpe + s(x, y) + s(state_name, bs = "re"), 
                weights = total_votes, family = quasibinomial, data = combined2)

# we can't compare the AIC of models created with gam and gamm, see
# https://stats.stackexchange.com/questions/70512/huge-%CE%94aic-between-gam-and-gamm-models

# some differences eg effective degrees of freedom less in the GAMM. But the
# main conclusions (significance of cpe) the same
summary(model6)
summary(model6b$gam)

# move the state random effect into the things to be estimated by nlme:
model6c <- gamm(per_gop ~ cpe + s(x, y),
                random = list(state_name = ~1),
                weights = total_votes, family = quasibinomial, data = combined2)

# fixed coefficients are identical to 6b, but fit was much faster
summary(model6c$gam)

# AIC identical:
AIC(model6c$lme)
AIC(model6b$lme)

#----------------random slopes--------------------
model7 <- gamm(per_gop ~ cpe + s(x, y),
                random = list(state_name = ~1 + cpe),
                weights = total_votes, family = quasibinomial, data = combined2)

# AIC is 200 less so worth having the random intercepts
AIC(model7$lme, model6c$lme)
summary(model7$lme)

preds7 <- predict(model7$gam, se.fit = TRUE, type = "response")

p7 <- combined2 |>
  mutate(fit = preds7$fit,
         se = preds7$se.fit,
         lower = fit - 1.96 * se,
         upper = fit + 1.96 * se) |>
  ggplot(aes(x = cpe, group = state_name)) +
  geom_point(aes(y = per_gop, colour = state_name), alpha = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "black", alpha = 0.5) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 1), label = percent) +
  scale_x_continuous(label  = percent)  +
  labs(x = "Crude prevalence estimate of depression",
       y = "Percentage vote for Trump in 2024 election",
       subtitle = "Grey ribbons are 95% confidence intervals from quasibinomial generalized additive model with spatial effect and state-level random intercept effect",
       title = "Counties with more depression voted more for Trump",
       caption = the_caption) +
  facet_wrap(~state_name)

svg_png(p7, "../img/0286c-random-slopes", w = 11, h = 8)

#---------------with spatial autocorrelation------------------

# first check if there's any left to explain and what shape it might be:
plot(Variogram(model7$lme, form = ~ x + y, resType = "normalized"), grid = TRUE)
# hmm, not much pattern there and I don't think it is working. I suspect
# the x and y it has aren't the right ones

# let's roll our own on a similar concept to see what's happening
counties <- select(combined2, county_fips, x, y)

# find the counties' distance from each other country
county_pairs <- expand_grid(from = counties$county_fips,
                            to = counties$county_fips) |>
  filter(from > to) |>
  left_join(counties, by =c("from" = "county_fips")) |>
  rename(fx = x, fy = y) |>
  left_join(counties, by =c("to" = "county_fips")) |>
  rename(tx = x, ty = y) |>
  mutate(distance = sqrt((fx - tx) ^ 2 + (fy - ty) ^ 2))  

res7 <- combined2 |>
  mutate(res = residuals(model7$lme, type = "response")) |>
  select(county_fips, res) 

p7a <- county_pairs |>
  left_join(rename(res7, from_res = res), by = c("from" = "county_fips")) |>
  left_join(rename(res7, to_res = res), by = c("to" = "county_fips")) |>
  mutate(distance = cut(distance, breaks = c(0, 0.5, 1,2,3, 4,6, 8,12,24,48,108)))  |>
  group_by(distance)  |>
  summarise(correlation = cor(from_res, to_res),
            n = n()) |>
  ungroup() |> 
  ggplot(aes(x = distance, y = correlation, size = n)) +
  geom_point(colour = "red") +
  scale_size_area(label = comma) +
  labs(title = "Correlation between pairs of counties' residuals",
       x = "Distance between two counties",
       y = "Correlation in residuals from model7",
       size = "Number of county-pairs")

svg_png(p7a, "../img/0286c-spatial-correlation", w = 9, h = 5)

# "Semivariance" is an annoying concept just in spatial statistics, it's 
# basically the same idea as above (bin pairs of a number based on the distance
# apart and then calculate a measure of similarity) but instead of the correlation
# it uses "half the variance of the differences between all possible points spaced a constant distance apart."
# see https://www.kgs.ku.edu/Tis/surf3/s3krig2.html
library(sp)
library(gstat)

sp_data <- combined2 |>
  mutate(res = residuals(model7$lme))
coordinates(sp_data) <- ~x+y

png("../img/0286c-variogram.png", w = 9 * 600, h = 5 * 600, res = 600, type = "cairo-png")
plot(variogram(res ~ 1, data = sp_data), 
       main = "Variogram for pairs of counties' residuals",
       xlab = "Distance between pairs of counties")
dev.off()

# o help us decide the shape of the spatial autocorrelation
model8 <- list()

model8[[1]] <- gamm(per_gop ~ cpe + s(x, y),
               random = list(state_name = ~1 + cpe),
               correlation = corExp(form = ~x +y),
               weights = total_votes, family = quasibinomial, data = combined2)

model8[[2]] <- gamm(per_gop ~ cpe + s(x, y),
                random = list(state_name = ~1 + cpe),
                correlation = corGaus(form = ~x +y),
                weights = total_votes, family = quasibinomial, data = combined2)

model8[[3]] <- gamm(per_gop ~ cpe + s(x, y),
                random = list(state_name = ~1 + cpe),
                correlation = corLin(form = ~x +y),
                weights = total_votes, family = quasibinomial, data = combined2)

model8[[4]] <- gamm(per_gop ~ cpe + s(x, y),
                random = list(state_name = ~1 + cpe),
                correlation = corRatio(form = ~x +y),
                weights = total_votes, family = quasibinomial, data = combined2)

model8[[5]] <- gamm(per_gop ~ cpe + s(x, y),
                random = list(state_name = ~1 + cpe),
                correlation = corSpher(form = ~x +y),
                weights = total_votes, family = quasibinomial, data = combined2)



sapply(model8, \(m){round(AIC(m$lme), -1)})
AIC(model7$lme)

# model8a (corExp) has the lowest AIC. And for the first time, cpe isn't
# 'significant' (value 3.37, se 1.99, sd 12.09)

sapply(model8, \(m){summary(m$gam)$p.t})

sapply(model8, \(m){c(summary(m$gam)$p.coeff[2], 
                      se = summary(m$gam)$se[2],
                      t_stat = summary(m$gam)$p.t[2])})


summary(model8[[1]]$lme)
      
sqrt(mean(ranef(model8[[1]]$lme)$state_name$cpe ^ 2))

tibble(slope = ranef(model8[[1]]$lme)$state_name$cpe,
       state_name = row.names(ranef(model8[[1]]$lme)$state_name)) |>
  mutate(state_name = gsub("1/", "", state_name)) |>
  mutate(state_name = fct_reorder(state_name, slope)) |>
  ggplot(aes(y = state_name, x = slope)) +
  geom_col()


# maybe we are doing too much on spatial autocorrelation now. What if we remove
# the rubber mat, just leave the correlation structure?
# no rubber mat, no race, but does have spatial autocorrelation
model9 <- gamm(per_gop ~ cpe,
                    random = list(state_name = ~1 + cpe),
                    correlation = corExp(form = ~x +y),
                    weights = total_votes, family = quasibinomial, data = combined2)

# AIC a bit worse without the rubber mat
AIC(model9$lme, model8[[1]]$lme)

# slope is now 'significant' (t value of 2.16)
summary(model9$lme)

#-----------diagram--------------
dag <- dagify(
  Vote ~ Race + Depression + 'Other factors',
  Depression ~ Race + 'Other factors'
)

set.seed(125)
p1 <- ggdag(dag, edge_type = "link", node = FALSE) +
  theme_dag_blank()  +
  geom_dag_node(colour = "lightgreen", shape = c(19, 19, 19, 17)) +
  geom_dag_edges(edge_colour = rep(c("lightblue", "lightblue", "grey", "black", "black"), each = 100),
                 edge_width = rep(c(1.5, 3, 0.8, 1.5, 3), each = 100),
                 edge_linetype = rep(c(1,1, 3, 1, 1), each = 100)) +
  geom_dag_text(colour = "steelblue")

# can we label the edges?

svg_png(p1, "../img/0286c-dag", w = 8, h = 4)

#---------------------data on 'race'-------------

# county characteristis from US Census Bureau
# see https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2023/CC-EST2023-ALLDATA.pdf
# for metadata



df <- "cc-est2023-alldata.csv"
if(!file.exists(df)){
  download.file("https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/asrh/cc-est2023-alldata.csv",
                destfile = df)
}

# key columns:
# TOT_POP total population
# WA_MALE "White alone" male
# WAC_MALE "white alone or in combination" male
# H_MALE Hispanic male
# HTOM_MALE Hispanic or more races male

race <- read_csv(df) |>
  # just 2023 and TOTAL age group:
  filter(YEAR == 5 & AGEGRP == 0) |>
  mutate(white_alone = (WA_MALE + WA_FEMALE) / TOT_POP,
         white_all = (WAC_MALE + WAC_FEMALE) / TOT_POP,
         hispanic = (H_MALE + H_FEMALE) / TOT_POP,
         hispanic_multi = (HTOM_MALE + HTOM_FEMALE) / TOT_POP) |>
  mutate(county_fips = paste0(STATE, COUNTY)) |>
  select(white_alone:county_fips, CTYNAME)


p8 <- function(){
  race |>
#  sample_n(500) |>
  select(-county_fips, -CTYNAME) |>
  ggpairs() |>
    print()
}

svg_png(p8, "../img/0286c-pairs", w = 9, h = 7)
# White-alone and white-all are too closely correlated (.991) to use them
# both so will drop white-all (this decision taken without looking at
# relationship to the response variable)


combined4 <- combined2 |>
  left_join(race, by = "county_fips")

# visual check that the counties joined correctly:
select(combined4, county_name, CTYNAME)


# check for linearity of relationships to the response variable

logit <- function(p){
  log(p / (1 -p))
}

p9 <- combined4 |>
  mutate(logit_gop = logit(per_gop)) |>
  select(logit_gop, total_votes, `
         Depression incidence` = cpe, 
         `Proportion only white` = white_alone, 
         `Proportion only hispanic` = hispanic, 
         `Proportion hispanic plus another` = hispanic_multi) |>
  gather(variable, value, -logit_gop, -total_votes) |>
  ggplot(aes(y = logit_gop, x =value)) +
  geom_point(aes(size = total_votes), alpha = 0.5) +
  geom_smooth(method = "lm", aes(weight = total_votes)) +
  facet_wrap(~variable, scales = "free_x") +
  scale_x_continuous(label = percent) +
  labs(x = "Explanatory variable value",
       y = "logit of vote for Trump, 2024")

svg_png(p9, "../img/0286c-exp-v-response", w = 9, h = 7)


# no rubber mat, but race:
model10 <- gamm(per_gop ~ cpe  + white_alone + hispanic + hispanic_multi,
               random = list(state_name = ~1 + cpe),
               correlation = corExp(form = ~x +y),
               weights = total_votes, family = quasibinomial, data = combined4)

# no random slopes or rubber mat, but race:
model11 <- gamm(per_gop ~ cpe  + white_alone + hispanic + hispanic_multi,
                random = list(state_name = ~1),
                correlation = corExp(form = ~x +y),
                weights = total_votes, family = quasibinomial, data = combined4)

# no random slope, rubber mat or spatial autocorrelation fix at all. this model
# is definitely illegitimate in that it makes no effort to fix for spatial issues.
model12 <- gamm(per_gop ~ cpe  + white_alone + hispanic + hispanic_multi,
                random = list(state_name = ~1),
                weights = total_votes, family = quasibinomial, data = combined4)


# rubber mat, race, random slope for cpe
model13 <- gamm(per_gop ~ cpe  + s(x, y) + white_alone + hispanic + hispanic_multi,
                random = list(state_name = ~1 + cpe),
                correlation = corExp(form = ~x +y),
                weights = total_votes, family = quasibinomial, data = combined4)

# rubber mat, race, only random intercept
model14 <- gamm(per_gop ~ cpe  + s(x, y) + white_alone + hispanic + hispanic_multi,
                random = list(state_name = ~1),
                correlation = corExp(form = ~x +y),
                weights = total_votes, family = quasibinomial, data = combined4)

# fullest model so far PLUS giving random slopes to the nuisance variables
# rubber mat, race, random slope for cpe & random slope for the two main
# race variables:
model15 <- gamm(per_gop ~ cpe  + s(x, y) + white_alone + hispanic + hispanic_multi,
                random = list(state_name = ~1 + cpe + white_alone + hispanic),
                correlation = corExp(form = ~x +y),
                weights = total_votes, family = quasibinomial, data = combined4)


# compare the no-rubber mat models (expect #15 to be best, with random slopes
# for CPE and race - the most flexibility)
tibble(model = 8:15,
       AIC = c(AIC(model8[[1]]$lme),
               AIC(model9$lme), 
               AIC(model10$lme), 
               AIC(model11$lme), 
               AIC(model12$lme), 
               AIC(model13$lme), 
               AIC(model14$lme), 
               AIC(model15$lme)),
       rubber_mat = c(1,0,0,0,0,1,1,1),
       random_cpe_slope = c(1,1,1,0,0,1,0,1),
       race = c(0,0, 1,1,1,1,1,1),
       random_race_slope = c(0,0,0,0,0,0,0,1),
       SAC_fix = c(1,1,1,1,0,1,1,1),
       cpe_p_value = c(
         summary(model8[[1]]$gam)$p.pv['cpe'],
         summary(model9$gam)$p.pv['cpe'],
         summary(model10$gam)$p.pv['cpe'],
         summary(model11$gam)$p.pv['cpe'],
         summary(model12$gam)$p.pv['cpe'],
         summary(model13$gam)$p.pv['cpe'],
         summary(model14$gam)$p.pv['cpe'],
         summary(model15$gam)$p.pv['cpe']),
       cpe_t_stat = c(
         summary(model8[[1]]$gam)$p.t['cpe'],
         summary(model9$gam)$p.t['cpe'],
         summary(model10$gam)$p.t['cpe'],
         summary(model11$gam)$p.t['cpe'],
         summary(model12$gam)$p.t['cpe'],
         summary(model13$gam)$p.t['cpe'],
         summary(model14$gam)$p.t['cpe'],
         summary(model15$gam)$p.t['cpe'])
       )  |>
  mutate(cpe_p_value = round(cpe_p_value, 4)) |>
  arrange(AIC)  |>
  # for space decided not to show this column, can just use t stat
  select(-cpe_p_value) |> 
  knitr::kable(format = "html") |> 
  kable_styling()|>
  writeClipboard()

# dramatic drop of AIC for including race

# so these are the best two models, and cpe isn't significant here:
summary(model10$lme)
summary(model13$lme)
summary(model15$lme)

# this model with a rubber mat and SAC fix and race, but no random slope,
# still has significant cpe effect:
summary(model14$lme)

# these two models that don't have random slope do still have cpe as significant
summary(model11$lme)
summary(model12$lme)


# what about something much more naive and simple?
model16 <- glm(per_gop ~ cpe  + state_name + white_alone + hispanic + hispanic_multi,
                weights = total_votes, data = combined4, family = quasibinomial)
anova(model16)
summmary(model16)
# in this case, cpe is still 'significant' and positive. So to make it not significant you
# need to introduce race *and* to have a random slope for depression, *and*
# some correction for spatial autocorrelation

# need a chart to summarise the results in model13


#--------------------partial charts-------------------
# model 8[[1]] is the full model except for race. we also need a full model except
# for depression (cpe). then we will use the residuals from each for some charts

model17 <- gamm(per_gop ~ s(x, y) + white_alone + hispanic + hispanic_multi,
                random = list(state_name = ~1 + white_alone + hispanic),
                correlation = corExp(form = ~x +y),
                weights = total_votes, family = quasibinomial, data = combined4)

# Difference between this and model 15 is no depression variable
c(AIC(model17$lme), AIC(model15$lme))

p5 <- combined4 |>
  mutate(after_cpe = residuals(model8[[1]]$gam, type = "response"))  |>
  ggplot(aes(x = white_alone, y = after_cpe)) +
  geom_point(alpha = 0.5, aes(size = total_votes)) +
  geom_smooth(aes(weight = total_votes), method = "lm") +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = percent, limits = c(-0.4, 0.6)) +
  scale_size_area(label = comma_format(suffix = "m", scale = 1e-6)) +
  labs(x = "Percentage of county that is 'white' as its only race",
       y = "Residual vote for Trump",
       subtitle = "After controlling for counties' depression incidence",
       title = "Partial relationship of 'whiteness' and Trump vote",
       size = "Total votes, 2024:")
  
  
p6 <- combined4 |>
  mutate(after_race = residuals(model17$gam, type = "response"))  |>
  ggplot(aes(x = cpe, y = after_race)) +
  geom_point(alpha = 0.5, aes(size = total_votes)) +
  geom_smooth(aes(weight = total_votes), method = "lm") +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = percent, limits = c(-0.4, 0.6)) +
  scale_size_area(label = comma_format(suffix = "m", scale = 1e-6)) +
  labs(x = "Incidence of diagnosed depression in each country",
       y = "Residual vote for Trump",
       subtitle = "After controlling for counties' racial composition",
       title = "Partial relationship of depression incidence and Trump vote",
       size = "Total votes, 2024:",
       caption = "Source: voting from tonmcg, depression from CDC, race from US Census Bureau; analysis by freerangestats.info")

svg_png(p5 + p6, "../img/0286c-partials", w = 11, h = 5)

