

library(palmerpenguins)
library(tidyverse)
library(scales)
library(boot)

penguins2 <- drop_na(penguins) %>%
  mutate(row_id = 1:n())

pc_boot <- function(d, w){
  m1 <- d[w, ] %>%
    select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) 
  
  pc1 <- prcomp(m1, scale = TRUE)
  
  return(pc1$rotation["body_mass_g", 1])
  
}

pc_boot(penguins2, 1:nrow(penguins2))

b <- boot(penguins2, pc_boot, R = 1999)

plot(density(b$t), bty = "l", main = "Distribution of body mass loading")

boot.ci(b)



# with direction check
pc_boot2 <- function(d, w, check_direction = TRUE){
  m1 <- d[w, ] %>%
    select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) 
  
  pc1 <- prcomp(m1, scale = TRUE)
  
  if(check_direction & pc1$rotation["body_mass_g", 1] < 0){
    pc1$rotation[ , 1] <-  -pc1$rotation[ , 1]    
  }

  return(pc1$rotation["body_mass_g", 1])
  
}


b2 <- boot(penguins2, pc_boot2, R = 1999)

plot(density(b2$t), bty = "l", main = "Distribution of body mass loading")

boot.ci(b2)
