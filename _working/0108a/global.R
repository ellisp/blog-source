library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(shinyWidgets)
library(fresh)
library(stringr)


df <- list.files("data", pattern = ".rda", full.names = TRUE)
for(f in df){
  load(f)
}