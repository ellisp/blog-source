library(shiny)
library(DT)
library(dplyr)
library(tidyr)


df <- list.files("data", pattern = ".rda", full.names = TRUE)
for(f in df){
  load(f)
}