
library(tidyverse)
library(scales)
library(patchwork)
library(MASS)
library(rio)
library(survey)
library(Cairo)
library(broom)


#--------------CDC annual Behavioral Risk Factor Surveillance System (BRFSS)  survey-

# 69.5 MB download, so only uncomment this and run it once:
# download.file("https://www.cdc.gov/brfss/annual_data/2018/files/LLCP2018XPT.zip",
#               destfile = "LLCP2018XPT.zip", mode = "wb")
# 
# unzip("LLCP2018XPT.zip")

llcp <- import("LLCP2018.XPT")
dim(llcp) # 437,436 respondents and 275 questions!

# weights. These are very unbalanced so take care:
summary(llcp$"_LLCPWT")

# Codebook
# https://www.cdc.gov/brfss/annual_data/2018/pdf/codebook18_llcp-v2-508.pdf

code_vars <- function(d){
  d %>%
    # height in metres:
    mutate(height = case_when(
            HEIGHT3 >= 9000 & HEIGHT3 <= 9998 ~ as.numeric(str_sub(HEIGHT3, 2, 4)) / 100,
            HEIGHT3 < 800 ~ (as.numeric(str_sub(HEIGHT3, 1, 1)) * 12 + 
                               as.numeric(str_sub(HEIGHT3, 2, 3))) * 2.54 / 100,
            TRUE ~ NA_real_
          )) %>%
    # weight in kg:
    mutate(weight = case_when(
      WEIGHT2 < 1000 ~ WEIGHT2 * 0.453592,
      WEIGHT2 >= 9000 & WEIGHT2 <= 9998 ~ as.numeric(str_sub(WEIGHT2, 2, 4)),
      TRUE ~ NA_real_
    ),
      bmi = weight / height ^ 2,
      trefethen = 1.3 * weight / height ^ 2.5,
    # BMI cutoff points from https://www.who.int/gho/ncd/risk_factors/bmi_text/en/
      who_cat = cut(bmi, c(0, 18.5, 25, 30, Inf), labels = c("Underweight", "Healthy",
                                                             "Overweight", "Obese"))) %>%
    mutate(sex = case_when(
      SEX1 == 1 ~ "Male",
      SEX1 == 2 ~ "Female",
      TRUE ~ NA_character_
    )) %>%
    mutate(race = case_when(
      `_HISPANC` == 1 ~ "Hispanic, Latino/a, or Spanish origin",
      `_PRACE1` == 1 ~ "White, non-Hispanic",
      `_PRACE1` == 2 ~ "Black or African American",
      `_PRACE1` == 3 ~ "American Indian or Alaskan Native",
      `_PRACE1` == 4 ~ "Asian",
      `_PRACE1` == 5 ~ "Pacific Islander",
      `_PRACE1` == 6 ~ "Other",
      TRUE ~ NA_character_
    ),
    race = fct_relevel(race, "White, non-Hispanic")) %>%
    mutate(race2 = fct_collapse(race,
                                "Other" = c("Pacific Islander", "Other", 
                                            "American Indian or Alaskan Native")),
           race2 = str_wrap(race2, 20),
           race2 = fct_reorder(race2, `_LLCPWT`, .fun = sum)) %>%
    mutate(age = case_when(
      `_AGE65YR` == 1 ~ "Age 18 to 64",
      `_AGE65YR` == 2 ~ "Age 65 or older",
      NA ~ NA_character_
      
    )) %>%
    mutate(race3 = str_wrap(race2, 20),
           race3 = fct_reorder(race3, `_LLCPWT`, .fun = sum)) %>%
    mutate(hhinc = case_when(
      INCOME2 == 1 ~ "Less than $10k",
      INCOME2 == 2 ~ "$10k to $15k",
      INCOME2 == 3 ~ "$15k to $20k",
      INCOME2 == 4 ~ "$20k to $25k",
      INCOME2 == 5 ~ "$25k to $35k",
      INCOME2 == 6 ~ "$35k to $50k",
      INCOME2 == 7 ~ "$50k to $75k",
      INCOME2 == 8 ~ "More than $75k",
      TRUE ~ NA_character_
    ),
    hhinc = fct_reorder(hhinc, INCOME2)) %>%
    mutate(education = case_when(
      EDUCA == 1 ~ "Never attended school or only kindergarten",
      EDUCA == 2 ~ "Grades 1 through 8",
      EDUCA == 3 ~ "Grades 9 through 11",
      EDUCA == 4 ~ "Grade 12 GED",
      EDUCA == 5 ~ "College 1 to 3 years",
      EDUCA == 6 ~ "College 4 years or more",
      TRUE ~ NA_character_
    ),
    education = fct_reorder(education, EDUCA)) %>%
    # remove people taller than the world record height:
    filter(height < 2.72) %>%
    rename(psu = `_PSU`,
           survey_weight = `_LLCPWT`) %>%
    dplyr::select(height, weight, sex, race, race2, race3, age, hhinc, education, bmi, psu, survey_weight)
}

the_caption = "Source: http://freerangestats.info analysis of the US CDC Behavioral Risk Factor Surveillance System Survey for 2018"

set.seed(123)
llcp_small <- llcp %>%
  sample_n(20000, weight = `_LLCPWT`, replace = TRUE) %>%
  code_vars() 

nrow(distinct(llcp_small)) # 17,370 - remembering higher weight observations likely to be resampled twice, and dropping NAs

llcp_all <- llcp %>%
  code_vars()



# Make a data frame for shading of healthy BMI, to get around problems with rectangles and log axes. See
# https://stackoverflow.com/questions/52007187/background-fill-via-geom-rect-is-removed-after-scale-y-log10-transformation
healthy <- tibble(
  xmin = 0.9,
  xmax = 2.6,
  ymin = 18.5,
  ymax = 25,
  race3 = unique(llcp_small$race3)
) %>%
  drop_na()


# data frame to use for shading of healthy weight implied by BMI
healthy_by_height <- healthy %>%
  dplyr::select(ymin, ymax, race3) %>%
  gather(variable, bmi, -race3) %>%
  mutate(link = 1) %>%
  full_join(tibble(height = seq(from = 0.9, to = 2.3, length.out = 100), link = 1), by = "link") %>%
  mutate(weight = bmi * height ^ 2) %>% 
  dplyr::select(-link, -bmi) %>%
  spread(variable, weight)


p1b <- llcp_all %>%
  dplyr::select(height, weight, sex, age, race3, survey_weight) %>%
  drop_na() %>%
  ggplot(aes(x = height)) +
  facet_grid(age ~ race3) +
  geom_ribbon(data = healthy_by_height, aes(ymin = ymin, ymax = ymax),
              colour = NA, fill = "grey90") +
  geom_jitter(alpha = 0.02, aes(y = weight, colour = sex, size = survey_weight)) +
  geom_smooth(method = "rlm", se = FALSE, aes(y = weight, colour = sex)) +
  #scale_x_log10(breaks = c(0.9, 1.1, 1.4, 1.8, 2.3)) + 
  #scale_y_log10() +
  scale_size_area(label = comma) +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Relationship between self-reported weight and height in USA adults",
       subtitle = "Men's weight is more dependent on their height than is the case for women.
Grey ribbon shows WHO-recommended healthy weight. Straight lines show empirical relationship of weight and height.",
       x = "Height (m)",
       y = "Weight (kg)",
       caption = the_caption,
       colour = "",
       size = "Each point represents X Americans:") +
  guides(size = guide_legend(override.aes = list(alpha = 1)))

png("../img/0166-full-data-weight.png", 12 * 600, 7 * 600, res = 600, type = "cairo-png")
  print(p1b) 
dev.off()



p3 <- llcp_all %>%
  drop_na() %>%
  ggplot(aes(x = height, y = bmi, colour = sex)) +
  facet_grid(age ~ race3) +
  geom_rect(data = healthy, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            colour = NA, fill = "grey90", inherit.aes = FALSE) +
  geom_jitter(alpha = 0.02, aes(size = survey_weight)) +
  geom_smooth(method = "rlm", se = FALSE, size = 0.5) +
  scale_size_area(label = comma) + 
  scale_x_log10(breaks = c(0.9, 1.1, 1.4, 1.8, 2.3)) + 
  scale_y_log10() +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Relationship between BMI and height in USA adults",
       subtitle = bquote("Taller people tend to have lower BMIs because on average weight is proportionate to less than the square of height (in fact, "~weight%prop%height^1.6~")"),
       x = "Height (metres), logarithmic scale",
       y = bquote("Body Mass Index: "~frac(weight,  height ^ 2)~", logarithmic scale"),
       caption = the_caption, 
       colour = "",
       size = "Each point represents X Americans:") +
  guides(size = guide_legend(override.aes = list(alpha = 1)))

png("../img/0166-full-data-bmi-log.png", 12 * 600, 7 * 600, res = 600, type = "cairo-png")
  print(p3) 
dev.off()


#------------

p4 <- llcp_all %>%
  mutate(h2 = height ^ 2,
         h2.5 = height ^ 2.5) %>%
  dplyr::select(h2, h2.5, weight, survey_weight) %>%
  drop_na() %>%
  gather(variable, value, -weight, -survey_weight) %>%
  mutate(variable = gsub("h2", expression(height^2), variable)) %>%
  ggplot(aes(x = value, y = weight)) +
  facet_wrap(~variable, scales = "free_x") +
  geom_jitter(alpha = 0.02, aes(size = survey_weight)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, k = 5)) +
  scale_size_area(label = comma) +
  labs(title = bquote("Weight compared to "~height^2~"and"~height^2.5),
       subtitle = "It's not possible to choose visually which of the two better expresses the relationship.",
       y = "Weight (kg)",
       x = "Height to the power of 2.0 or 2.5",
       size = "Each point represents X Americans:") +
  guides(size = guide_legend(override.aes = list(alpha = 1)))
  

png("../img/0166-power-scatter.png", 12 * 600, 7 * 600, res = 600, type = "cairo-png")
  print(p4) 
dev.off()

#-------Models---------------
llcp_svy <- svydesign(~psu, weights = ~survey_weight, data = llcp_all)

model0 <- svyglm(log(weight) ~ log(height), design = llcp_svy)


model1 <- svyglm(log(weight) ~ log(height) + sex + race + age, design = llcp_svy)

anova(model1)
summary(model1)
exp(coef(model1))
# best exponent for height is 1.55, not 2 (this is the raw coefficient for height)
# men of the same race, age and height weigh 6% more than women (exp(0.056))
# Asians weight 89% of white non-hispanic of same sex and age
# old people weight 99% of 18-64 year old (prob a cohort effect)


#--------------more stuff------------

model2 <- lm(log(weight) ~ log(height) + sex + race + age + hhinc + education, data = llcp_all)
anova(model2)
summary(model2)
exp(coef(model2))


llcp_small %>%
  drop_na() %>%
  ggplot(aes(y = height, colour = education, x = hhinc)) +
  geom_boxplot()

model3 <- lm(log(height) ~ sex + age + race + hhinc + education, data = llcp_all)
model4 <- lm(log(height) ~ age + race + sex * (hhinc + education), data = llcp_all)
anova(model3, model4)
summary(model4)




#--------------compare the 2.5 figures...-----------------------------


model5 <- lm(weight ~ I(height ^ 2.5) - 1, data = llcp_small)


with(llcp_small, mean(weight / height ^ 2.5, na.rm = TRUE))
with(llcp_small, mean(weight / height ^ 2, na.rm = TRUE))

#===========================Episode 2=============================
     


#-------------prep - small additions to last week--------
library(broom)
library(ggExtra)

llcp_releveled <- llcp_all %>%
  mutate(hhinc = fct_explicit_na(hhinc),
         education = fct_explicit_na(education),
         race = fct_explicit_na(race),
         age = fct_explicit_na(age)) %>%
  mutate(hhinc = fct_relevel(hhinc, "$25k to $35k"),
         education = fct_relevel(education, "Grade 12 GED"))

llcp_svy <- svydesign(~psu, weights = ~survey_weight, data = llcp_releveled)


#----reminders----------

p20 <- llcp_small %>%
  ggplot(aes(x = height, y = weight)) +
  geom_jitter(size = 1, alpha = 0.2) +
  geom_smooth(method = "gam", se = FALSE) +
  labs(title = "A non-linear relationship, and heteroskedastic response variable",
       subtitle = "A standard challenge for the assumptions justifying ordinary least squares as an estimation method.",
       caption = the_caption,
       x = "Height (m)",
       y = "Weight (kg)")

p21 <- function(){print(ggMarginal(p20, fill = "steelblue", alpha = 0.5, colour = "white"))}
svg_png(p21, "../img/0167-scatter", w = 10, h = 7)

p22 <- p20 +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Log-log transformations can fix two problems at once",
       subtitle = "Marginal densities are now symmetrical and closer to Normal; a linear relationship now shows elasticity.",
       x = "Height (m) (logarithmic scale)",
       y = "Weight (kg) (logarithmic scale)")

p23 <- function(){print(ggMarginal(p22, fill = "steelblue", alpha = 0.5, colour = "white"))}
svg_png(p23, "../img/0167-scatter-log", w = 10, h = 7)


#------------------Different error structure---

model1 <- svyglm(log(weight) ~ log(height) + sex + race + age, design = llcp_svy)

model12 <- svyglm(weight ~ log(height) + sex + race + age, design = llcp_svy,
                  family = quasi(link = "log", variance = "mu"))

tibble(
  Variable = names(coef(model1)),
  `Log - Log Gaussian` = coef(model1),
  `Quasi GLM with log link and mu variance` = coef(model12)
) %>%
  knitr::kable() %>%
  kableExtra::kable_styling() %>%
  clipr::write_clip()


#-----------------income and education impacting on height-------------
# First, is height really dependent on (or at least correlated with) income and education?


#' Extract and relabel effects from a model. Very specific to today's problem.
get_ready <- function(m){
  # remove intercept:
  d <- suppressWarnings(tidy(confint(m))[-1, ])  %>%
    # remove height - as a continuous variable it doesn't work well
    filter(!grepl("log(height)", .rownames, fixed = TRUE)) %>%
    rename(variable = .rownames) %>%
    mutate(var_type = case_when(
      grepl("^education", variable) ~ "Education",
      grepl("^hhinc", variable) ~ "Household Income",
      grepl("^race", variable) ~ "Race",
      grepl("^sex", variable) ~ "Sex",
      grepl("^age", variable) ~ "Age"
    )) %>%
    mutate(variable = gsub("^education", "", variable),
           variable = gsub("^hhinc", "", variable),
           variable = gsub("^race", "", variable),
           variable = gsub("^sex", "", variable),
           variable = gsub("^age", "", variable)) %>%
    mutate(var_seq = 1:n()) 
}



model13 <- svyglm(height ~ sex + race + hhinc + education + age, design = llcp_svy,
                  family = quasi(link = "identity", variance = "mu"))

d13 <- get_ready(model13)
  
p13 <- d13 %>% ggplot(aes(x = X2.5..*100, xend = X97.5..*100, 
                 y = var_seq, yend = var_seq, 
                 colour = var_type,
                 alpha = I(variable == "(Missing)"))) +
  geom_vline(colour = "black", xintercept = 0) +
  geom_segment(size = 3) +
  scale_colour_brewer(palette = "Set1") +
  scale_alpha_manual(values = c(1, 0.2), guide = "none") +
  labs(x = "95% confidence interval of average difference in height (cm)\nCompared to a white, 18-64yrs, non-Hispanic woman with Year 12 education, household income $25k-$35k",
       colour = "",
       y = "",
       caption = the_caption,
       title = "Income and education are related to height even after controlling for sex, age and race",
       subtitle = "The 'effects' of today's income and education are likely to actually be indicators of socioeconomic status while young,
impacting on both height and economic outcomes.") +
  scale_y_continuous(breaks = d13$var_seq, labels = d13$variable) +
  theme(panel.grid.minor = element_blank())

svg_png(p13, "../img/0167-height-class", w = 11, h = 6)

#---------------Height / weight---------

model14 <- svyglm(weight ~ log(height) + sex + race + hhinc + education + age, 
                  design = llcp_svy,
                  family = quasi(link = "log", variance = "mu"))

d14 <- get_ready(model14)
  

p14 <- d14 %>% ggplot(aes(x = exp(X2.5..), xend = exp(X97.5..), 
                 y = var_seq, yend = var_seq, 
                 colour = var_type,
                 alpha = I(variable == "(Missing)"))) +
  geom_segment(size = 3) +
  scale_colour_brewer(palette = "Set1") +
  scale_alpha_manual(values = c(1, 0.2), guide = "none") +
  geom_vline(colour = "black", xintercept = 1) +
  annotate("text", x= 0.92, y = 9,
           label = bquote(~weight%prop%height^1.6~"(not shown on chart)")
           ) +
  labs(x = "95% confidence interval of impact on weight (expressed as a multiplier)\nCompared to a white non-Hispanic woman with Year 12 education, household income $25k-$35k",
       colour = "",
       y = "",
       caption = the_caption,
       title = "Income and education are related to weight after controlling for sex, height and race",
       subtitle = "The effects are complex, but the groups with most education and highest income have the lower weights.") +
  scale_y_continuous(breaks = d14$var_seq, labels = d14$variable) +
  theme(panel.grid.minor = element_blank()) 

svg_png(p14, "../img/0167-weight-class", w = 11, h = 6)


#-------Impact on BMI--------------
# this chart actually looks very similar to the previous one modelling weight on height etc,
#

model15 <- svyglm(bmi ~ log(height) + sex + race + hhinc + education + age, design = llcp_svy,
                  family = quasi(link = "identity", variance = "mu"))

d15 <- get_ready(model15)
  
p15 <- d15 %>%  ggplot(aes(x = X2.5.., xend = X97.5.., 
                  y = var_seq, yend = var_seq, 
                  colour = var_type,
                  alpha = I(variable == "(Missing)"))) +
  geom_segment(size = 3) +
  scale_colour_brewer(palette = "Set1") +
  scale_alpha_manual(values = c(1, 0.2), guide = "none") +
  labs(x = "95% confidence interval of impact on Body Mass Index\nCompared to a white non-Hispanic woman with Year 12 education, household income $25k-$35k",
       colour = "",
       y = "",
       caption = the_caption,
       title = "Income and education are related to weight after controlling for sex, height and race",
       subtitle = "The effects are complex, but the groups with most education and highest income have the lower weights") +
  scale_y_continuous(breaks = d15$var_seq, labels = d15$variable) +
  theme(panel.grid.minor = element_blank())

svg_png(p15, "../img/0167-bmi-class", w = 11, h = 6)

