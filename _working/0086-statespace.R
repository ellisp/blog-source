
# an oldy but a goody; good reflection on history of time series charting
# http://timelyportfolio.github.io/rCharts_time_series/history.html

# require(latticeExtra)
require(tidyverse)
require(quantmod)
# require(PerformanceAnalytics)
# require(xtsExtra)
# require(rCharts)


# get S&P 500 data from FRED (St. Louis Fed)
sp500 <- na.omit( 
   getSymbols(
      "SP500",
      src = "FRED",
      from = "1949-12-31",
      auto.assign = FALSE
   )
) 

plot(sp500)
summary(sp500)


sp500 <- getSymbols('^GSPC', src='yahoo', auto.assign = FALSE) 
plot(sp500)
