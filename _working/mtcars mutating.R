
library(tidyverse)
library(microbenchmark)

mtcars2 <- slice_sample(mtcars, n = 1e7, replace = TRUE)

meth_wt <- function(){
  mtcars2 |>
    within({gear = factor(gear)
            wt = wt * 0.453
            car = rownames(mtcars)}) |>
    subset(disp >= 100, select = c(car, wt, gear)) |>
    summary()
}

meth_tr <- function(){
  mtcars2 |>
    transform(gear = factor(gear),
    wt = wt * 0.453,
    car = rownames(mtcars)) |>
    subset(disp >= 100, select = c(car, wt, gear)) |>
    summary()
}

meth_tv <- function(){
  mtcars2 |>
    mutate(gear = factor(gear),
              wt = wt * 0.453,
              car = rownames(mtcars)) |>
    filter(disp >= 100) |>
    select(car, wt, gear) |>
    summary()
}


microbenchmark(meth_wt, meth_tr, meth_tv, times = 10000L)  

