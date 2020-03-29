data(bactgrowth)
xyplot(value ~ time|strain+as.factor(conc), data = bactgrowth,
       groups = replicate, pch = 16, cex = 0.5)


splitted.data <- multisplit(bactgrowth, c("strain", "conc", "replicate"))
dat <- splitted.data[[1]]

head(dat)
with(dat, plot(time, value))

fit <- fit_easylinear(dat$time, dat$value)
fit

plot(fit)
lines(exp(predict(fit)))


fit2 <- fit_growthmodel(FUN = grow_twostep, time = dat$time, y = dat$value)
?fit_growthmodel

p     <- c(y0 = 0.01, mumax = 0.2, K = 0.1)
lower <- c(y0 = 1e-6, mumax = 0,   K = 0)
upper <- c(y0 = 0.05, mumax = 5,   K = 0.5)

fit1 <- fit_growthmodel(FUN = grow_logistic, p = p, dat$time, dat$value,
                        lower = lower, upper = upper)

plot(fit1)

p <- c(mumax = 0.1, k = 1e4, alpha = 1, beta = 1, gamma = 1)
fit2 <- fit_growthmodel(FUN = grow_genlogistic, p = p, 
                        time = the_data$days_since, 
                        y = the_data$cum_cases)

# simple one to fit
fit <- fit_easylinear(the_data$days_since, the_data$cum_cases)
plot(fit)

#-------------logistic growth ie non-linear---------
p <- c(y0 = 10, mumax = 0.1, K = 4000)
fit3 <-  fit_growthmodel(FUN = grow_logistic, p = p, 
                         time = the_data$days_since, 
                         y = log(the_data$cum_cases))
slotNames(fit3)

fit3@fit$par
names(fit3@fit)

plot(fit3)
# check this does what I think!
lines(the_data$days_since, log(the_data$cum_cases) + residuals(fit3), type = "l")


plot(the_data$days_since, the_data$cum_cases)
lines(the_data$days_since, exp(log(the_data$cum_cases) + residuals(fit3)), type = "l")


fit3@FUN

#-------------generalised logistic growth----------------
p <- c(y0 = min(log(the_data$cum_cases)),
       mumax = 0.2, 
       K = max(log(the_data$cum_cases)),
       alpha = 1, beta = 1, gamma = 1)

# caution - these need to be in the same order as p!
lower <- c(y0 = 0, mumax = 0, K = max(log(the_data$cum_cases)) - 1, alpha = 0.5, beta = 0.5, gamma = 0.5)
upper <- c(y0 = 10, mumax = 0.5, K = 20, alpha = 1.5, beta = 1.5, gamma = 1.5)

fit4 <-  fit_growthmodel(FUN = grow_genlogistic, p = p, 
                         time = the_data$days_since, 
                         y = log(the_data$cum_cases), lower = lower, upper = upper)
plot(fit4)

# clearly the generalized logistic fit is better:
plot(the_data$days_since, the_data$cum_cases)
lines(the_data$days_since, exp(log(the_data$cum_cases) + residuals(fit3)), type = "l")
lines(the_data$days_since, exp(log(the_data$cum_cases) + residuals(fit4)), type = "l", col = "red")

exp(fit4@par[["K"]])
slotNames(fit4)
