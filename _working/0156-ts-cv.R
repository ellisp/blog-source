library(forecast)






set.seed(123)
n <- 100
x <- ts(cumsum(arima.sim(list(ar = 0.5, ma = 0.5), n = n)))
y = ts(c(0, 0.4 * x[-n] + cumsum(arima.sim(list(ar = 0.5, ma = 0.5), n = n - 1))))

autoplot(cbind(x, y))

mod1 <- auto.arima(y)
mod2 <- auto.arima(y, xreg = x)

AIC(mod1, mod2)
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

# this CV takes about 20 seconds
system.time({
  aa1_cv <- tsCV(y, aafc)
  aa2_cv <- tsCV(y, aafc, xreg = x)
  })

rbind(accuracy(y + aa1_cv, y),
      accuracy(y + aa2_cv, y)) %>%
  as.data.frame() %>%
  mutate(model = c("Univariate", "With x regressor")) %>%
  dplyr::select(model, everything())

