library(tidyverse)


fams <- tribble(~id, ~fam,
                  1,  "Kay and Peter",
                  2,  "chez Robyn",
                  3,  "chez Adrianne",
                  4,  "Graham and Shirley",
                  5,   "Fishers")

peep <- tribble(~name,           ~fam1, ~fam2,
                "Robyn",          2,     5, 
                "Peter B",        2,     NA,
                "Nick",           2,     NA,
                "Hannah",         2,     NA,
                "Kay",            1,     5,
                "Peter E",        1,     NA,
                "Adrianne",       3,     5,
                "Bassie",         3,     NA,
                "Alex",           3,     NA,
                "Alex's friend",  3,     NA,
                "Shirley",        4,     5,
                "Graham",         4,     5
) |> 
  # remove alex and friend if we want by uncommenting next line:
  filter(!grepl("Alex", name))

no_good <- 10
seed <- 1
while(no_good > 0){
  set.seed(seed)
  seed <- seed + 1

  attempt <- select(peep, giver = name, giver_fam1 = fam1, giver_fam2 = fam2) |> 
    sample_n(size = n(), replace = FALSE) |> 
    cbind(peep) |> 
    rename(recipient = name, recipient_fam1 = fam1, recipient_fam2 = fam2)
  
  intrafam <- attempt |> 
    summarise(match = sum(giver_fam1 == recipient_fam1)) |> 
    pull(match)
  
  intrafam2 <- attempt |> 
    drop_na() |> 
    summarise(match = sum(giver_fam2 == recipient_fam2)) |> 
    pull(match)
  
  reciprocal <- attempt |> 
    inner_join(attempt, by = c("giver" = "recipient", "recipient" = "giver"))
  
  no_good <- intrafam  + nrow(reciprocal)
  
  cat(no_good)
}  

select(attempt, giver, recipient)
