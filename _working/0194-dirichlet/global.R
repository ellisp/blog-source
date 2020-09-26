# Global setup for application that draws a ternary density plot for a Dirichlet distribution
# Peter Ellis 26 September 2020

library(shiny)
library(gtools)
library(ggtern)
library(fresh) # for use_googlefont

d <- expand.grid(p1 = 1:99 /100, p2 = 1:99 / 100) 
d$p3 <- with(d, 1 - p1 - p2) 
d <- subset(d, p3 >= 0.01) 

