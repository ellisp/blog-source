
#========================GPA example========================


library(nlme)
library(glmmTMB)
library(tidyverse)
library(lme4)
library(brms)

# also see https://bbolker.github.io/mixedmodels-misc/notes/corr_braindump.html

# data from https://m-clark.github.io/mixed-models-with-R/random_intercepts.html#example-student-gpa

download.file("https://github.com/m-clark/mixed-models-with-R/raw/master/data.zip",
              destfile = "mmr_data.zip",
              mode = "wb")
unzip("mmr_data.zip")
gpa <- read_csv("data/gpa.csv")

# what does data look like
gpa |>
  ggplot(aes(x = occasion, y = gpa)) +
  geom_line(aes(group = student))
# students generally get better GPAs on later occasions

# random intercept
ri1 <- lmer(gpa ~ occasion + (1 | student), data = gpa)
ri2 <- lme(gpa ~ occasion, random = ~ 1 | student, data = gpa)
ri3 <- glmmTMB(gpa ~ occasion + (1 | student), data = gpa)
ri4 <- brm(gpa ~ occasion + (1 | student), data = gpa)

# random intercept with autoregression
ria2a <- lme(gpa ~ occasion, 
             random = ~ 1 | student, 
             data = gpa, 
             correlation = corAR1())
# seems identical to this:
ria2b <- lme(gpa ~ occasion, 
             random = ~ 1 | student, 
             data = gpa, 
             correlation = corAR1(form = ~ 1 | student))
ria4a <- brm(gpa ~ occasion + (1 | student) + ar(p = 1), data = gpa)

# Deprecated approach - gets  warning to specify it in formula, and not to use cor_brms
# But gets the same result
ria4b <- brm(gpa ~ occasion + (1 | student), 
          autocor=cor_ar(formula = ~1, p = 1, cov = FALSE),
          data = gpa)

# basically Gaussian?
plot(density(residuals(ri1)))
qqnorm(residuals(ri1)) # student-occasions
qqnorm(ranef(mod1)[,1]) # students



# random slope
rs1a <- lmer(gpa ~ occasion + (1 + occasion | student), data = gpa)
rs1b <- lmer(gpa ~ occasion + (occasion | student), data = gpa) # identical to the last one even though the 1 not explicitly included
rs2 <- lme(gpa ~ occasion, random = ~ occasion | student, data = gpa)
rs3 <- glmmTMB(gpa ~ occasion + (occasion | student), data = gpa)
rs4 <- brm(gpa ~ occasion + (occasion | student), data = gpa)

# random slope with autoregression
# probably doesn't make sense, are "occasions" equally spaced? but ok
# for demo
rsa2a <- lme(gpa ~ occasion, 
    random = ~ occasion | student, 
    data = gpa,
    correlation = corAR1())

# we specify form explicitly in the below but it seems to make zero difference
rsa2b <- lme(gpa ~ occasion, 
             random = ~ occasion | student, 
             data = gpa,
             correlation = corAR1(form = ~ occasion | student))

rsa2c <- lme(gpa ~ occasion, 
             random = ~ occasion | student, 
             data = gpa,
             correlation = corAR1(form = ~ 1 | student))


rsa4 <- brm(gpa ~ occasion + (occasion | student) + ar(p = 1), data = gpa)
