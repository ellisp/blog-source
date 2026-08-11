library(tidyverse)


library(quantmod)

getSymbols("CIVPART", src = "FRED")   # LFPR
getSymbols("UNRATE", src = "FRED")    # unemployment rate
getSymbols("U6RATE", src = "FRED")    # underemployment
getSymbols("EMRATIO", src = "FRED")   # employment-population ratio
ls()

CIVPART
par(mfrow = c(2,2))
plot(CIVPART)
plot(UNRATE)
plot(U6RATE)
plot(EMRATIO)
