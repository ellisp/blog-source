library(forecast)
library(dplyr)
library(ggplot2)

# Simulate data:
set.seed(125)
n <- 100
x <- ts(cumsum(arima.sim(list(ar = 0.5, ma = 0.5), n = n)))
y = ts(0.4 * x + cumsum(arima.sim(list(ar = 0.5, ma = 0.5), n = n)))
z = ts(0.4 * y + cumsum(arima.sim(list(ar = 0.5, ma = 0.5), n = n)))
z <- c(0, z[1:(n - 1)])
  
d <- cbind(x, y, z)

# Exploratory plot:
p1 <- autoplot(d) +
  ggtitle("Three simulated, related, time-series",
          "x causes y and y causes z; our job is to forecast y.") +
  labs(colour = "", y ="")

svg_png(p1, "../img/0156-ts")

# Auto correlations:
p2 <- ggPacf(d) + 
  theme(panel.grid = element_blank()) +
  ggtitle("Partial autocorrelations and cross correlations", 
          "Original series")

p3 <- ggPacf(diff(d)) + 
  theme(panel.grid = element_blank()) +
  ggtitle("Partial autocorrelations and cross correlations", 
          "First differenced series")

svg_png(p2, "../img/0156-pacf1")
svg_png(p3, "../img/0156-pacf2")


mod1 <- auto.arima(y, d = 1)
mod2 <- auto.arima(y, xreg = x, d = 1)
mod3 <- auto.arima(y, xreg = z, d = 1)
mod4 <- auto.arima(y, xreg = cbind(x, z), d = 1)


knitr::kable(AIC(mod1, mod2, mod3, mod4))
# gets a warning - because the differencing is different in the two models


#---------Cross validation---------------
#' auto.arima forecast function for time series cross validation
#' 
#' adapted from https://gist.github.com/robjhyndman/d9eb5568a78dbc79f7acc49e22553e96 
aafc <- function(y, h, xreg = NULL, ...){
  if(!is.null(xreg)){
    
    ncol <- NCOL(xreg)
    X <- matrix(xreg[1:length(y), ], ncol = ncol)
    if(NROW(xreg) < length(y) + h)
      stop("Not enough xreg data for forecasting")
    newX <- matrix(xreg[length(y) + (1:h), ], ncol = ncol)
    
    fit <- auto.arima(y, xreg=X, ...)
    return(forecast(fit, xreg = newX, h = h))
    
  } else {
    
    fit <- auto.arima(y, ...)
    return(forecast(fit, h = h))
  }
}

# this CV takes about 50 seconds
system.time({
  aa1_cv <- tsCV(y, aafc, d = 1)
  aa2_cv <- tsCV(y, aafc, xreg = x, d = 1)
  aa3_cv <- tsCV(y, aafc, xreg = z, d = 1)
  aa4_cv <- tsCV(y, aafc, xreg = cbind(x, z), d = 1)
  
  })

rbind(accuracy(ts(y[-1] - aa1_cv[-n]), ts(y[-1])),
      accuracy(ts(y[-1] - aa2_cv[-n]), ts(y[-1])),
      accuracy(ts(y[-1] - aa3_cv[-n]), ts(y[-1])),
      accuracy(ts(y[-1] - aa4_cv[-n]), ts(y[-1]))) %>%
  as.data.frame() %>%
  mutate(model = c("Univariate", 
                   "With x regressor", 
                   "With z regressor", 
                   "Both x and z as regressors")) %>%
  dplyr::select(model, everything()) %>%
  knitr::kable()


# mean error of univariate model:
mean(aa1_cv, na.rm = TRUE)

# root mean square error of x regressor model
sqrt(mean(aa2_cv ^ 2, na.rm = TRUE))

# mean absolute error of z regressor model
mean(abs(aa3_cv), na.rm = TRUE)
