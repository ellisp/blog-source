
set.seed(123)


for(j in 1:500){
  cat(j)
  
  suppressWarnings(
    suppressPackageStartupMessages({
      library(tidyverse)
      library(duckplyr)
    })
  )
  
  use_duckplyr <- sample(c(TRUE, FALSE), 1)
  summary_op <- sample(letters[1:3], 1)
  
  if(!use_duckplyr){
    detach("package:duckplyr", unload=TRUE)
    } 
  
  n <- sample(c(1e5, 1e6, 1e7, 1e8), 1)
  
  for(i in 1:10){
  
    d <- tibble(x = sample(LETTERS, size = n, replace = TRUE),
                y = rnorm(n))
    
      if(summary_op == "a"){
        time <- system.time(
          r <- d |>
          group_by(x) |>
          summarise(s1 = mean(y),
                    s2 = median(y),
                    s3 = sd(y))
        )
      } 
      if(summary_op == "b"){
        time <- system.time(
          r <- d |>
            group_by(x) |>
            summarise(s1 = mean(y))
        )
      } 
    if(summary_op == "c"){
      time <- system.time(
        r <- d |>
          group_by(x) |>
          arrange(y) |>
          summarise(s1 = y[1])
      )
    } 
    
    write_csv(tibble(elapsed = time['elapsed'],
                     user = time['user.self'],
                     system = time['sys.self'],
                      n= n,
                     use_duckplyr = use_duckplyr,
                     summary_op = summary_op,
                     i = i),
              file = "duckplyr-results.csv",
              append = file.exists("duckplyr-results.csv"))
  }
}