
#========================GPA example========================


library(nlme)
library(glmmTMB)
library(tidyverse)
library(lme4)
library()

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
# students gneerally get better GPAs on later occasions

# random intercept
lmer(gpa ~ occasion + (1 | student), data = gpa)
lme(gpa ~ occasion, random = ~ 1 | student, data = gpa)
glmmTMB(gpa ~ occasion + (1 | student), data = gpa)

# basically Gaussian?
mod1 <- lme(gpa ~ occasion, random = ~ 1 | student, data = gpa)
plot(density(residuals(mod1)))
qqnorm(residuals(mod1)) # student-occasions
qqnorm(ranef(mod1)[,1]) # students



# random slope
lmer(gpa ~ occasion + (1 + occasion | student), data = gpa)
lmer(gpa ~ occasion + (occasion | student), data = gpa) # identical to the last one even though the 1 not explicitly included
lme(gpa ~ occasion, random = ~ occasion | student, data = gpa)
glmmTMB(gpa ~ occasion + (occasion | student), data = gpa)


# random slope with autoregression
# probably doesn't make sense, are "occasions" equally spaced? but ok
# for demo
lme(gpa ~ occasion, 
    random = ~ occasion | student, 
    data = gpa,
    correlation = corAR1())
