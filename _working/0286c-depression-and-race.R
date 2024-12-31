library(GGally)
library(ggdag)

# you need to have run source("0286-voting-and-depression.R") first

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
AIC(model7$lme)
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


#---------------with spatial autocorrelation------------------

# simplified model just to help us decide the shape of the spatial autocorrelat
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
model9 <- gamm(per_gop ~ cpe,
                    random = list(state_name = ~1 + cpe),
                    correlation = corExp(form = ~x +y),
                    weights = total_votes, family = quasibinomial, data = combined2)

# AIC a bit worse without the rubber mat
c(AIC(model9$lme), AIC(model8[[1]]$lme))

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

svg_png(p1, "../img/0286c-dag", w = 8, h = 6)

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
  select(white_alone:county_fips)


race |>
  sample_n(500) |>
  select(-county_fips) |>
  ggpairs()

# White-alone and white-all are too closely correlated (.991) to use them
# both so will drop white-all (this decision taken without looking at
# relationship to the response variable)


combined4 <- combined2 |>
  left_join(race, by = "county_fips")

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


# compare the no-rubber mat models (expect #10 to be best, with random slopes
# for CPE plus fixed slopes for race)
tibble(model = 9:14,
       AIC = c(AIC(model9$lme), 
               AIC(model10$lme), 
               AIC(model11$lme), 
               AIC(model12$lme), 
               AIC(model13$lme), 
               AIC(model14$lme)),
       rubber_mat = c(0,0,0,0,1,1),
       random_cpe_slope = c(1,1,0,0,1,0),
       race = c(0, 1,1,1,1,1),
       sac_fix = c(1,1,1,0,1,1),
       cpe_p_value = c(
         summary(model9$gam)$p.pv['cpe'],
         summary(model10$gam)$p.pv['cpe'],
         summary(model11$gam)$p.pv['cpe'],
         summary(model12$gam)$p.pv['cpe'],
         summary(model13$gam)$p.pv['cpe'],
         summary(model14$gam)$p.pv['cpe'])
       )  |>
  mutate(cpe_p_value = round(cpe_p_value, 4)) |>
  arrange(AIC)

# dramatic drop of AIC for including race

# so these are the best two models, and cpe isn't significant here:
summary(model10$lme)
summary(model13$lme)

# this model with a rubber mat and SAC fix and race, but no random slope,
# still has significnat cpe effect:
summary(model14$lme)

# these two models that don't have random slope do still have cpe as significant
summary(model11$lme)
summary(model12$lme)


# what about something much more naive and simple?
model15 <- lm(per_gop ~ cpe  + state_name + white_alone + hispanic + hispanic_multi,
                weights = total_votes, data = combined4)
anova(model15)
# in this case, cpe is still 'significant'

