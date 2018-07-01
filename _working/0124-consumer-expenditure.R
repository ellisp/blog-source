
# mandatory reading: https://www.bls.gov/cex/pumd_novice_guide.pdf

library(haven)
library(tidyverse)
library(scales)
library(frs)    # for modulus transforms of scales
library(MASS)   # for rlm.  Watch out for dplyr::select and MASS::select clash.
library(ggExtra)
library(survey)

dir.create("bls-cd")
download.file("https://www.bls.gov/cex/pumd/data/stata/intrvw16.zip",
              destfile = "bls-cd/intrvw16.zip", mode = "wb")

download.file("https://www.bls.gov/cex/pumd/ce_pumd_interview_diary_dictionary.xlsx",
              destfile = "bls-cd/ce_pumd_interview_diary_dictionary.xlsx", mode = "wb")

unzip("bls-cd/intrvw16.zip", exdir = "bls-cd")

# The FMLI files have characteristics, income, weights, and summary expenditure
# the numbers in file names refer to year and quarter.  So intrvw16/fmli162.dta is 
# 2016, second quarter

fmli171 <- read_dta("bls-cd/intrvw16/fmli171.dta")

# looks like no attributes associated with it
str(attributes(fmli171))
attr(fmli171, "labels")
attr(fmli171, "label")

formats <- sapply(fmli171, function(x){attr(x, "format.stata")})
# which columns are  numeric:
formats[grepl("[0-9]g", formats)]

table(fmli171$inclass)

inclasses <- c("Less than $5,000", "$5,000 to $9,999", "$10,000 to $14,999", "$15,000 to $19,999",
    "$20,000 to $29,999", "$30,000 to $39,999", "$40,000 to $49,999", "$50,000 to $69,999",
    "$70,000 and over")


inclass_lu <- data_frame(
  inclass = paste0("0", 1:9),
  inclass_ch = ordered(inclasses, levels = inclasses)
) 

d <- fmli171 %>%
  mutate(gas_pp =  gasmocq / fam_size * 4,
         inc_pp = fincbtxm / fam_size,
         gas_p_inc = gasmocq * 4 / fincbtxm,
         no_gas = as.integer(gas_pp == 0),
         bls_urbn_ch = ifelse(bls_urbn == 1, "Urban", "Rural"),
         ) %>%
  left_join(inclass_lu, by = "inclass")


# parameter for how much to transform the dollar scales:
lambda <- 0.2

svg("../img/0124-densities.svg", 8, 5)
ggplot(d, aes(x = gas_pp, colour = bls_urbn_ch)) +
  geom_density() + 
  scale_x_continuous(label = dollar,
                     trans = modulus_trans(lambda),
                     breaks = modulus_breaks(lambda)) + 
  geom_rug() +
  ggtitle("Spend on petrol has a spike at $0 and a skewed distribution beyond that",
          "Unweighted, 2017 quarter 1. Horizontal scale has been transformed.") +
  labs(caption = "Source: USA Bureau of Labor Statistics Consumer Expenditure Survey",
       colour = "",
       x = "Expenditure per person on gasoline and motor oil this quarter x 4")
dev.off()

summary(fmli171$cuincome) # total income; not present
summary(fmli171$earnincx) # earnings before tax; not present
table(fmli171$earncomp) # composition of earners
table(fmli171$cutenure) # housing tenure (6 categories)
table(fmli171$inclass) # income bracket
summary(fmli171$inc_rank) # Weighted cumulative percent ranking based on total current income before taxes (for complete income reporters)
summary(fmli171$othrincm) # other income
summary(fmli171$ffrmincx) # farm income, not present
summary(fmli171$fincbtax) # total amount of family income before taxes in the past 12 months - collected only
summary(fmli171$fincbtxm) # total amount of family income before taxes - imputed as well as collected
summary(fmli171$finlwt21) # final calibrated weight

svg("../img/0124-densities-inc.svg", 8, 5)
ggplot(d, aes(x = inc_pp, colour = bls_urbn_ch)) +
  geom_density() +
  geom_rug() +
  ggtitle("After imputation, income has a skewed distribution with no spike at zero",
          "Unweighted, 2017 quarter 1. Horizontal scale has been transformed.") +
  scale_x_continuous(label = dollar,
                     trans = modulus_trans(lambda),
                     breaks = modulus_breaks(lambda)) +
  labs(caption = "Source: USA Bureau of Labor Statistics Consumer Expenditure Survey",
       colour = "",
       x = "Family income per person before taxes in the past 12 months")
dev.off()

  
svg("../img/0124-quantile.svg", 8, 6)
ggplot(d, aes(y = gas_pp, x = inc_rank, size = finlwt21)) +
  geom_point(aes(colour = inclass_ch), alpha = 0.2) + 
  geom_smooth(method = "rlm", aes(weight = finlwt21)) +
  scale_y_continuous(label = dollar,
                     trans = modulus_trans(lambda),
                     breaks = modulus_breaks(lambda)) +
  labs(x = "Income quantile") +
  ggtitle("Spend on petrol increases as income quantile of the household increases",
          "Blue line shows robust regression using M estimator.  Vertical axis is transformed.") +
  labs(caption = "Source: USA Bureau of Labor Statistics Consumer Expenditure Survey",
       colour = str_wrap("Income class of household based on income before taxes", 20),
       y = "Expenditure per person on gasoline and motor oil this quarter") +
  theme(legend.position = "right") +
  guides(colour = guide_legend(override.aes = list(size = 4, alpha = 1))) +
  scale_size_area(guide = "none")
dev.off()


p <- ggplot(d, aes(x = inc_pp, y = gas_pp, size = finlwt21)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "rlm") +
  scale_x_continuous("Income per person in household in past 12 months",
                     label = dollar,
                     trans = modulus_trans(lambda),
                     breaks = modulus_breaks(lambda)) +
  scale_y_continuous(label = dollar,
                     trans = modulus_trans(lambda),
                     breaks = modulus_breaks(lambda)) +
  theme(legend.position = "none") +
  ggtitle("Spend on petrol increases as income of the household increases",
          "Blue line shows robust regression using M estimator.  Both axes are transformed.") +
  labs(caption = "Source: USA Bureau of Labor Statistics Consumer Expenditure Survey",
       y = "Expenditure per person on gasoline and motor oil this quarter x 4") 

  
svg("../img/0124-scatter-1.svg", 8, 7)
ggMarginal(p, type = "density", fill = "grey", colour = NA)
dev.off()

svg("../img/0124-scatter-2.svg", 8, 7)
ggMarginal(p %+% filter(d, gas_pp > 0 & inc_pp > 0), 
           type = "density", fill = "grey", colour = NA)
dev.off()


svg("../img/0124-props.svg", 8, 5)
d %>%
  filter(inc_pp > 1000) %>%
  ggplot(aes(x = inc_pp, y = gas_p_inc, size = finlwt21)) +
  geom_point(alpha = 0.3) +
  scale_x_continuous("Income per person in household in past 12 months",
                     label = dollar,
                     trans = modulus_trans(lambda),
                     breaks = modulus_breaks(lambda)) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous() +
  geom_smooth(se = FALSE) +
  theme(legend.position = "none") +
  ggtitle("Poorer households spend more on petrol proportionately than do richer households") +
  labs(caption = "Source: USA Bureau of Labor Statistics Consumer Expenditure Survey",
       y = "Expenditure per person on gasoline and motor oil\nas a proportion of income") 
dev.off()

#--------------no gas----------------

p1 <- d %>%
  ggplot(aes(x = inc_pp, y = no_gas, size = finlwt21)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  scale_x_continuous("Income per person in household in past 12 months",
                     label = dollar,
                     trans = modulus_trans(lambda),
                     breaks = modulus_breaks(lambda)) +
  labs(y = "Probability of spending $0 on gasoline and motor oil this quarter\n") +
  theme(legend.position = "none") +
  labs(caption = "Source: USA Bureau of Labor Statistics Consumer Expenditure Survey 2017 Q1") +
  ggtitle("Poorer households are more likely to spend nothing on petrol at all",
          "Blue line shows logistic regression; points show American households")

svg("../img/0124-no-gas.svg", 8, 5)
print(p1)
dev.off()

p2 <- p1 %+% filter(d, inc_pp > 0) + 
  ggtitle("", "Effect holds even if restricted to households with positive income")

svg("../img/0124-no-gas2.svg", 8, 5)
print(p2)
dev.off()


# crude approximation of the survey design, that takes just the primary sampling units and weights
# into account but ignores the stratification and post-stratification calibration (this will be
# conservative for inference so that's ok)
dd <- svydesign(~psu, data = d, weights = ~finlwt21)

mod <- svyglm(no_gas ~ inc_pp, design = dd, family = "quasibinomial")
summary(mod)

convert_pngs("0124")

