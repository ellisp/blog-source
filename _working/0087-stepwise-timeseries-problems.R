library(tidyverse)
library(scales)
library(forecast)
library(glmnet)
library(forcats)
library(directlabels)

#===========generate data============
n <- 110
h <- 12

set.seed(123)
y <- ts(cumsum(arima.sim(model = list(ar = c(0.5, -0.2), ma = 0.9), n = n) + 0.2))
x1 <- cumsum(arima.sim(model = list(ar = c(0.5, -0.2), ma = 0.9), n = n + h) - 0.2)
x2 <- cumsum(arima.sim(model = list(ar = c(0.5, -0.2), ma = 0.9), n = n + h))

orig_data <- data_frame(Value = c(y, x1, x2),
           Variable = rep(c("y", "x1", "x2"), times = c(n, n + h, n + h)),
           TimePeriod = c(1:n, 1:(n+h), 1:(n+h))) 

svg("../img/0087-origdata.svg", 8, 4)
orig_data %>%
   ggplot(aes(x = TimePeriod, y = Value)) +
   geom_line() +
   facet_wrap(~Variable) +
   ggtitle("Original data",
           "Three unrelated (but that is unknown to us) univariate time series")
dev.off()

svg("../img/0087-pairs.svg", 7, 5)
GGally::ggpairs(data.frame(x1 = x1[1:n], x2 = x2[1:n], y = as.numeric(y)))
dev.off()

X_full <- cbind(x1, x2)
X_historical <- X_full[1:n, ]
X_future <- X_full[-(1:n), ]


# A version that has been differenced once....
YX_diff_lags <- data_frame(
   y_d = c(diff(y), rep(NA, h)),
   x1_d = diff(x1),
   x2_d = diff(x2))

# And has lagged variables up to lag period of 20 for each variable:
lagnumber <- 20

for (i in 1:lagnumber){
   YX_diff_lags[, ncol(YX_diff_lags) + 1] <- lag(YX_diff_lags$y_d, i)
   YX_diff_lags[, ncol(YX_diff_lags) + 1] <- lag(YX_diff_lags$x1_d, i)
   YX_diff_lags[, ncol(YX_diff_lags) + 1] <- lag(YX_diff_lags$x2_d,i)
}

names(YX_diff_lags)[-(1:3)] <- 
   paste0(names(YX_diff_lags)[1:3], "_lag_", rep(1:lagnumber, times = rep(3, lagnumber)))

if(ncol(YX_diff_lags) != 3 + 3 * lagnumber){
   stop("Wrong number of columns; something went wrong")
}
#===================Modelling options========

#-------------univariate-----------------
mod1 <- auto.arima(y)
fc1 <- forecast(mod1, h = h)

svg("../img/0087-fc1.svg", 8, 4)
autoplot(fc1)
dev.off()

#------------xreg, no lags----------------
mod2 <- auto.arima(y, xreg = X_historical)
fc2 <- forecast(mod2, xreg = X_future)

svg("../img/0087-fc2.svg", 8, 4)
autoplot(fc2)
dev.off()

#-----------xreg, lags----------------
YX_hist <- YX_diff_lags[complete.cases(YX_diff_lags), ]

mod3 <- lm(y_d ~ ., data = YX_hist)

#' Forecast from a regression model with lags one row at a time
#' Assumes the existence of: YX_diff_lags & y.
#' Has to forecast one row at a time, then populate the explanatory data frame
#' with the lagged values possible after that forecast.
onerow_forecast <- function(model){
   prediction_data <- YX_diff_lags[is.na(YX_diff_lags$y_d), ]
   y_names <- names(YX_diff_lags)[grepl("y_d", names(YX_diff_lags))]
   
   for(i in 1:nrow(prediction_data)){
      for(j in 2:nrow(prediction_data)){
         for(k in 2:length(y_names)){
            prediction_data[j , y_names[k]] <- prediction_data[j-1 , y_names[k - 1]]
         }
      }
      
      if(class(model) == "cv.glmnet"){
         new_y <- predict(model, newx = as.matrix(prediction_data[i, -1]))
      } else {
         new_y <- predict(model, newdata = prediction_data[i, -1])   
      }
      
      prediction_data[i , "y_d"] <- new_y
   }
   
   # de-diff ie return to the original, non-differenced scale
   fc_y <- cumsum(c(as.numeric(tail(y, 1)), prediction_data$y_d))
   return(fc_y[-1])
}

fc3 <- onerow_forecast(mod3)

#-------xreg, lags, stepwise----------------
# high value of k to mimic stepwise selection based on p values:
mod4 <- step(mod3, k = 4.7, trace = 0)
fc4 <- onerow_forecast(mod4)

#-----------xreg, lags, lasso---------------
# Use cross-validation to determine best value of lambda, for how much
# shrinkage to force:
mod5 <- cv.glmnet(y = YX_hist$y_d, x = as.matrix(YX_hist[ , -1]))
fc5 <- onerow_forecast(mod5)     

#====================compare results==============


p <- data_frame(Univariate = fc1$mean, 
           SimpleXreg = fc2$mean, 
           AllLags = fc3, 
           StepLags = fc4, 
           LassoLags = fc5,
           TimePeriod = 1:h + n) %>%
   gather(Variable, Value, -TimePeriod) %>%
   mutate(Judge = ifelse(grepl("Lags", Variable) & !grepl("Lasso", Variable),
                         "Bad", "OK")) %>%
   mutate(Variable = fct_relevel(Variable, c("AllLags", "StepLags"))) %>%
   ggplot(aes(x = TimePeriod, y = Value, colour = Variable)) +
   facet_wrap(~Judge, ncol = 1) +
   geom_line(size = 1.2) +
   scale_colour_brewer(palette = "Set1") +
   theme(legend.position = "right") +
   geom_line(data = filter(orig_data, Variable == "y"), colour = "black", linetype = 3) +
   xlim(c(0, 130)) +
   annotate("text", x = 25, y = 15, label = "Historical data", 
            colour = "grey40", family = "myfont") +
   ggtitle("Excessive inclusion of lagged explanatory variables leads to overfitting",
           "Regularization by lasso, or using a simpler model in the first place, gives much better results.") +
   labs(caption = "Simulations from forecasting one time series based on two unrelated time series.")

svg("../img/0087-results.svg", 8, 6)   
direct.label(p)
dev.off()
 
convert_pngs("0087")










