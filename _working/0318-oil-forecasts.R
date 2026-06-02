


library(quantmod)
library(forecast)

oil <- getSymbols("DCOILBRENTEU", src = "FRED", auto.assign = FALSE)
oil_futures <- getSymbols("BZ=F", src = "yahoo", auto.assign = FALSE)


tail(oil_futures)
plot(oil_futures[,1])

plot(oil)
tail(oil) # only goes to 9 March, on 17 March - so one week behind
mod1 <- auto.arima(oil)
mod2 <- ets(oil)

mod3 <- auto.arima(oil_futures[, 1])

forecast(mod1)
forecast(mod2)
forecast(mod3)
plot(forecast(mod1, h = 30))

summary(mod1)
