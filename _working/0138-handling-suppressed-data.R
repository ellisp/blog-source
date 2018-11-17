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
library(testthat)

#---------random rounding base 3-----------------

rel84 <- function(lambda){dpois(8, lambda) / dpois(4, lambda)}
CairoSVG("../img/0138-8-v-4.svg", 8, 4)
par(family = main_font, bty = "l", font.main = 1, fg = "grey", col.axis = "grey",
    mar = c(5,5,4,2))
plot(1:20, rel84(1:20), type = "b", col = "steelblue",
     main = "An eight is much more likely than a four for most tables",
     xlab = "Mean value of counts in a Poisson distribution",
     ylab = "Probability of an 8,\ndivided by probability of a 4")
dev.off()

# probability of observing a six, given actual values 4, 5, 6, 7, 8
prob <- c(1,2,3,2,1) / 3
prior15 <- dpois(4:8, 15)
prior30 <- dpois(4:8, 30)

rr3_data <- data_frame(
  Value = 4:8,
  `Equal probabilities` = prob / sum(prob),
  `Poisson with mean of 15` = prob * prior15 / sum(prior15 * prob),
  `Poisson with mean of 30` = prob * prior30 / sum(prior30 * prob)
) %>%
  gather(prior, post_prob, -Value)

# check our probabilities add up to 1
expect_equal(as.vector(tapply(rr3_data$post_prob, rr3_data$prior, sum)),
             c(1,1,1))

CairoSVG("../img/0138-rr3-priors.svg", 8, 4)
rr3_data %>%
  ggplot(aes(x = Value, weight = post_prob)) +
  geom_bar(fill = "steelblue", alpha = 0.8) +
  facet_wrap(~prior) +
  labs(y = "Posterior probability this is the actual value") +
  ggtitle("A value that has been random rounded (base 3) for statistical disclosure control",
          "Distribution of true value when published value is 6, with three different prior expectations")
dev.off()

#-----------------missing data example----------
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


#------------correspondence analysis---------

tab <- data %>% 
  dplyr::select(animals, region, count) %>%
  spread(animals, count) %>%
  dplyr::select(lions, tigers, bears) %>%
  as.matrix()
rownames(tab) <- LETTERS[1:4]

CairoSVG("../img/0138-corresp-real.svg", 7, 8)
par(family = main_font, font.main = 1, col.axis = "grey80", fg = "grey80")
xl <- c(-1.2, 0.5)
yl <- c(-1.1, 0.4)
biplot(corresp(tab, nf = 2), 
       main = "Full data including suppressed values\n", 
       col = c("orange", "steelblue"),,
       xlim = xl, ylim = yl)
dev.off()

table_image <- readPNG("0138-table-image.png")

pvalues_f <- numeric()
pvalues_c <- numeric()

dir.create("tmp")
pd <- setwd("tmp")

# we can use brute force and see if the results are statistically significant
# for all possible values.  We'll do chi square tests and correspondence analysis
# at the same time
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
file.rename("0138-corresp.gif", "../../img/0138-corresp.gif")
setwd(pd)
unlink("tmp", recursive = TRUE)

summary(cbind(pvalues_f, pvalues_c))

#====================different approaches to dealing with the missing data===================

#---------------regenerate data-----------
# doing this here to make this chunk of code easier for when I want to generalise this whole approach
# to multiple simulations by changing the seed
set.seed(123)
true_coefs <- c(2, runif(n - 1, -1, 1))
data <- data %>%
  mutate(expected = exp(mm %*% true_coefs),
         count = rpois(n, lambda = exp(mm %*% true_coefs)),
         censored_count = ifelse(count < 6, "<6", count),
         censored_count_num = as.numeric(censored_count),
         count_replaced = ifelse(count < 6, 3, count))

#--------------straightforward methods-----------
mod0 <- glm(count ~ animals * region, data = data, family = "poisson")
mod1 <- glm(censored_count_num ~ animals * region, data = data, family = "poisson")
mod2 <- glm(count_replaced ~ animals * region, data = data, family = "poisson")

#------------with censored poisson regression-------------------------
# (note that we indicated a complete observation with a 1, and a censored one with a 0)
data$z <- pmax(6, data$count)
data$not_lcensored <- as.numeric(!is.na(data$censored_count_num ))
mod3 <- vglm(SurvS4(z, not_lcensored, type = "left") ~ animals * region, family = cens.poisson, data = data)

#------------------ with multiple imputation----------
#' Imputation function for suppressed data for use with mice - Poisson-based
#'
#' @param y vector to be imputed
#' @param ry logical vector of observed (TRUE) and missing (FALSE) values of y
#' @param x design matrix. Ignored but is probably needed for mice to use.
#' @param wy vector that  is TRUE where imputations are created for y. Not sure when this is different
#' to just !ry (which is the default).
mice.impute.desuppress <- function (y, ry, x, wy = NULL, max_value = 5, ...) {
  # during dev:
  # y <- data$censored_count_num; ry <- !is.na(y)
  if (is.null(wy)){ 
    wy <- !ry
  }
  
  # What are the relative chances of getting values from 0 to the maximum allowed value,
  # if we have a Poisson distribution  which we have estimated the mean of via trimmed mean?
  # (this is very approximate but probably better than just giving equal chances to 0:5)
  probs <- dpois(0:max_value, mean(y[ry], tr = 0.2))
  
  return(sample(0:max_value, sum(wy), prob = probs, replace = TRUE))
}

#' Imputation function for suppressed data for use with mice - simple
#'
mice.impute.uniform <- function (y, ry, x, wy = NULL, max_value = 5, ...) {
  return(sample(0:max_value, sum(wy), replace = TRUE))
}

m <- 20
data_imp1 <- mice(data[ , c("censored_count_num", "animals", "region")], 
                  method = "desuppress", print = FALSE, m = m)
imp_y1 <- data_imp1$imp$`censored_count_num` 
imp_y1

data_imp2 <- mice(data[ , c("censored_count_num", "animals", "region")], 
                  method = "uniform", print = FALSE, m = m)
imp_y2 <- data_imp2$imp$`censored_count_num` 
imp_y2

mod_mice1 <- with(data_imp1, glm(censored_count_num ~ animals * region, family = "poisson"))
coef_mice1 <- pool(mod_mice1)$pooled$estimate

mod_mice2 <- with(data_imp2, glm(censored_count_num ~ animals * region, family = "poisson"))
coef_mice2 <- pool(mod_mice2)$pooled$estimate

#---------------results----------------------
# comparison data
d <- data_frame(underlying = true_coefs, 
                `Using full data including suppressed values (not usually possible!)` = coef(mod0),
                #         `Dropping suppressed values altogether and setting over-determined parameters to zero` = coef(mod1),
                `Replacing suppressed values with 3` = coef(mod2),
                `Left-censored survival-based method` = coef(mod3),
                `MICE with Poisson proportional probabilities` = coef_mice1,
                `MICE with uniform probabilities` = coef_mice2,
                labels = names(coef(mod0))) %>%
  mutate(labels = gsub("animals", "", labels),
         labels = gsub("region", "", labels)) %>%
  gather(method, value, -underlying, -labels) %>%
  mutate(method = str_wrap(method, 25)) %>%
  mutate(value = ifelse(is.na(value), 0, value)) 

# summary data:  
d2 <- d %>%
  mutate(square_error = (value - underlying) ^ 2) %>%
  group_by(method) %>%
  summarise(mse = mean(square_error),
            trmse = mean(square_error, tr = 0.2)) %>%
  ungroup()  %>%
  mutate(method = fct_reorder(method, mse))

# summary graphic:  

CairoSVG("../img/0138-summary.svg", 8, 5)
ggplot(d2, aes(x = mse, y = trmse, label = method)) +
  geom_point(size = 2) +
  geom_text_repel(colour = "steelblue") +
  labs(x = "Mean squared error of coefficient estimates",
       y = "Trimmed mean squared error of coefficient estimates") +
  ggtitle("Comparison of different methods of handling suppressed counts in a frequency table",
          "Summary results of models fitted after different treatment methods")
dev.off()  

expand <- 1.5
CairoSVG("../img/0138-coefs.svg", 8 * expand, 6 * expand)
d %>%
  mutate(method = factor(method, levels = levels(d2$method))) %>%
  ggplot(aes(x = underlying, y = value, label = labels)) +
  facet_wrap(~method, nrow = 1) +
  geom_abline(slope = 1, intercept = 0, colour = "grey50") +
  geom_point() +
  geom_text_repel(colour = "steelblue") +
  labs(x = "Correct value of coefficient in underlying data generating process",
       y = "Value estimated from a GLM with one particular method of dealing with censored data") +
  coord_equal() +
  ggtitle("Comparison of different methods of handling suppressed counts in a frequency table",
          "Coefficient estimates from models fitted after different treatment methods")
dev.off()


convert_pngs("0138")


