library(tidyverse)
library(scales)
library(knitr)
library(MASS) # for corresp
library(Cairo)
library(png)
library(VGAM)
library(grid)
library(ggrepel)
library(mice)

data <- expand.grid(animals = c("lions", "tigers", "bears"),
            region = LETTERS[1:4],
            count = 1) %>%
  as_tibble() %>%
  mutate(animals = fct_relevel(animals, "lions"))

n <- nrow(data)

mm <- model.matrix(count ~ animals * region, data = data)


set.seed(123)
true_coefs <- c(2, runif(n - 1, -1, 1))
data <- data %>%
  mutate(expected = exp(mm %*% true_coefs),
         count = rpois(n, lambda = exp(mm %*% true_coefs)),
         censored_count = ifelse(count < 6, "<6", count),
         censored_count_num = as.numeric(censored_count),
         count_replaced = ifelse(count < 6, 3, count))

# data as it is published:
data %>% 
  dplyr::select(animals, region, censored_count) %>%
  spread(animals, censored_count) %>%
  kable

# actual underlying data:
data %>% 
  dplyr::select(animals, region, count) %>%
  spread(animals, count) %>%
  kable

# actual underlying data generating process expectations:
data %>% 
  dplyr::select(animals, region, expected) %>%
  mutate(expected = round(expected, 1)) %>%
  spread(animals, expected) %>%
  kable


#------------chisquare tests---------
# we can use brute force and see if the results are statistically significant
# for all possible values
tab <- data %>% 
  dplyr::select(animals, region, count) %>%
  spread(animals, count) %>%
  dplyr::select(lions, tigers, bears) %>%
  as.matrix()
rownames(tab) <- LETTERS[1:4]
tab

par(family = main_font, font.main = 1)
xl <- c(-1.2, 0.5)
yl <- c(-1.1, 0.4)
biplot(corresp(tab, nf = 2), 
       main = "Full data including suppressed values\n",
       xlim = xl, ylim = yl)

table_image <- readPNG("0138-table-image.png")

pvalues_f <- numeric()
pvalues_c <- numeric()

dir.create("tmp")
pd <- setwd("tmp")

for(i in 0:5){
  for(j in 0:5){
    tab[2, 2] <- i # tigers in B
    tab[2, 3] <- j # bears in B
    pvalues_f <- c(pvalues_f, fisher.test(tab, simulate.p.value = TRUE)$p.value)
    pvalues_c <- c(pvalues_c, chisq.test(tab)$p.value)
    CairoPNG(paste0(i, j, ".png"), 2000, 2000, res = 300)
      par(family = main_font, font.main = 1, col.axis = "grey80", fg = "grey80", mar = c(2,1,4,1))
      biplot(corresp(tab, nf = 2), col = c("orange", "steelblue"),
             main = paste0("Imputed values: tigers in B = ", i, "; bears in B = ", j, ".\n"),
             xlim = xl, bty = "n", 
             ylim = yl)
      grid.raster(table_image, x = 0.66, y = 0.2, width = 0.3)
    dev.off()

  }
}
system('magick -loop 0 -delay 40 *.png "0138-corresp.gif"')
setwd(pd)
unlink("tmp")

summary(cbind(pvalues_f, pvalues_c))

#--------------------different approaches to dealing with the missing data-----------------------

set.seed(123)
true_coefs <- c(2, runif(n - 1, -1, 1))
data <- data %>%
  mutate(expected = exp(mm %*% true_coefs),
         count = rpois(n, lambda = exp(mm %*% true_coefs)),
         censored_count = ifelse(count < 6, "<6", count),
         censored_count_num = as.numeric(censored_count),
         count_replaced = ifelse(count < 6, 3, count))

# straightforward:
mod0 <- glm(count ~ animals * region, data = data, family = "poisson")
mod1 <- glm(censored_count_num ~ animals * region, data = data, family = "poisson")
mod2 <- glm(count_replaced ~ animals * region, data = data, family = "poisson")

# with censored poisson regression:
# (this method does so badly that I can only assume I am using it wrongly - so I will need another
# post to investigate it)
data$z <- pmax(6, data$count)
data$lcensored <- is.na(data$censored_count_num )
mod3 <- vglm(SurvS4(z, lcensored, type = "left") ~ animals * region, family = cens.poisson, data = data)

# with multiple imputation
data_imp <- mice(data[ , c("censored_count_num", "animals", "region")])
imp_y <- data_imp$imp$`censored_count_num` 
# summary(imp_y)
data_imp$imp$`censored_count_num` <- as.data.frame(apply(imp_y, 2, function(x){pmin(5, x)})) 
# tried this many times but the original imputation is nearly always 6 or more, so in effect
# multiple imputation reduces to just imputing 5 for everything. Note that this indicates
# there is probalby some real information in how low the real observations are (as we know is
# actually the case)

# compare
d <- data_frame(underlying = true_coefs, 
           `Using full data including suppressed values` = coef(mod0),
  #         `Dropping suppressed values altogether and setting over-determined parameters to zero` = coef(mod1),
           `Replacing suppressed values with 3` = coef(mod2),
           `Left-censored survival-based method` = coef(mod3),
           labels = names(coef(mod0))) %>%
  mutate(labels = gsub("animals", "", labels),
         labels = gsub("region", "", labels)) %>%
  gather(variable, value, -underlying, -labels) %>%
  mutate(variable = str_wrap(variable, 25)) %>%
  mutate(value = ifelse(is.na(value), 0, value)) 

d %>%
  ggplot(aes(x = underlying, y = value, label = labels)) +
  facet_wrap(~variable, nrow = 1) +
  geom_abline(slope = 1, intercept = 0, colour = "grey50") +
  geom_point() +
  geom_text_repel(colour = "steelblue") +
  labs(x = "Correct value of coefficient in underlying data generating process",
       y = "Value estimated from a GLM with one particular method of dealing with censored data") +
  coord_equal()




#===========================With Stan==========================
# If on windows, make sure stan was installed this way: 
# https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Windows.
# You can't just install.packages("rstan")


# This turned out to be a dead end.
# I thought I could model the misisng values as poisson parameters in their own way;
# but Stan can't get estimates for integer parameters. If we say they can be real,
# then we can't model y as poisson at all. If I then try to model y as normal but
# with variance equal to the mean, I have a non-linear transformation to deal with
# that is beyond my competence (but this does still seem to run)
library(rstan)
library(rstanarm)
# caution that the next line tells Stan to use all the cores available.  Makes sense on my
# laptop, may not if you're on a supercomputer...
options(mc.cores = parallel::detectCores(), auto_write = TRUE)

# get data into shape for Stan. First, the model matrix but without the intercept
stan_data <- list(
  X = mm,
  p = length(true_coefs), # number of slopes to estimate including intercept
  n = n,                  # number of observations
  y = data$count)

moda <- stan("0138-moda.stan", data = stan_data, control = list(max_treedepth = 10))

moda_coefs <- apply(as.array(moda), 3, mean)[1:12]


moda_glm <- glm(count ~ animals * region, family = poisson, data = data)
plot(coef(moda_glm), moda_coefs[1:12])

# diagnostics:
# launch_shinystan(moda)

modb <- stan("0138-modb.stan", data = stan_data, control = list(max_treedepth = 10))
modb

modb_coefs <- apply(as.array(modb), 3, mean)[1:12]
pairs(data.frame(true_coefs, stan1 = moda_coefs, stan2 = modb_coefs, glm = coef(moda_glm)))
