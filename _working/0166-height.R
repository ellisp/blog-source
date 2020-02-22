
library(tidyverse)
library(scales)
library(patchwork)
library(MASS)
library(rio)
library(survey)
library(Cairo)

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
    dplyr::select(height, weight, sex, race, race2, race3, age, hhinc, education, bmi, `_LLCPWT`)
}

the_caption = "Source: http://freerangestats.info analysis of the US CDC Behavioral Risk Factor Surveillance System Survey for 2018"

set.seed(123)
llcp_small <- llcp %>%
  sample_n(20000, weight = `_LLCPWT`, replace = TRUE) %>%
  code_vars() 

nrow(distinct(llcp_small)) # 17,369 - remembering higher weight observations likely to be resampled twice, and dropping NAs

llcp_all <- llcp %>%
  code_vars()

p1 <- llcp_small %>%
  dplyr::select(height, weight, sex, age, race3) %>%
  drop_na() %>%
  ggplot(aes(x = height, y = weight, colour = sex)) +
  facet_grid(age ~ race3) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(method = "rlm", se = FALSE) +
  scale_x_log10(breaks = c(0.9, 1.1, 1.4, 1.8, 2.3)) + 
  scale_y_log10() +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Relationship between weight and height in USA adults",
       subtitle = "Subset of 20,000 observations resampled to simulate a simple random sample.",
       x = "Height (m)",
       y = "Weight (kg)",
       caption = the_caption,
       colour = "")

svg_png(p1, "../img/0166-20000",w = 12, h = 7)

# Make a data frame to get around problems with rectangles and log axes. See
# https://stackoverflow.com/questions/52007187/background-fill-via-geom-rect-is-removed-after-scale-y-log10-transformation
healthy <- tibble(
  xmin = 0.9,
  xmax = 2.6,
  ymin = 18.5,
  ymax = 25,
  race2 = unique(llcp_small$race2)
)

p3 <- llcp_all %>%
  drop_na() %>%
  ggplot(aes(x = height, y = bmi, colour = sex)) +
  facet_grid(age ~ str_wrap(race2, 20)) +
  geom_rect(data = healthy, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            colour = NA, fill = "grey90", inherit.aes = FALSE) +
  geom_jitter(alpha = 0.02, aes(size = `_LLCPWT`)) +
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
       size = "Survey weight")

png("../img/0166-full-data-bmi-log.png", 12 * 600, 7 * 600, res = 600, type = "cairo-png")
  print(p3) 
dev.off()


#-------Models---------------
model0 <- lm(log(weight) ~ log(height), data = llcp_small)
summary(mod)
# now the best exponent for height is 1.95! how do we explain this


model1 <- lm(log(weight) ~ log(height) + sex + race + age, data = llcp_small)

anova(model1)
summary(model1)
exp(coef(model1))
# best exponent for height is 1.65, not 2 (this is the raw coefficient for height)
# men weigh 5% more than women (exp(0.0499))
# Asians weight 89% of white non-hispanic
# old people weight 99% of 18-64 year old (prob a cohort effect)


model2 <- lm(log(weight) ~ log(height) + sex + race + age + hhinc + education, data = llcp_small)
anova(model2)
summary(model2)
exp(coef(model2))
# note that height ius now 5.11 ! how is this possible? Turns out height is correlated with income and eudcation

llcp_small %>%
  drop_na() %>%
  ggplot(aes(y = height, colour = education, x = hhinc)) +
  geom_boxplot()

model3 <- lm(log(height) ~ sex + age + race + hhinc + education, data = llcp_small)
model4 <- lm(log(height) ~ age + race + sex * (hhinc + education), data = llcp_small)
anova(model3, model4)
summary(model4)


model4 <- svyglm(log(height))




#--------------compare the 2.5 guy-----------------------------


model5 <- lm(weight ~ I(height ^ 2.5) - 1, data = llcp_small)


with(llcp_small, mean(weight / height ^ 2.5, na.rm = TRUE))
with(llcp_small, mean(weight / height ^ 2, na.rm = TRUE))
     