library(tidyverse)
library(scales)
library(GGally)
library(mgcv)


load("follow_data_2.rda")
  ggplot(follow_data_2, aes(x = number_followers, y = trmean_ff)) + 
    geom_point(aes(colour = (number_following > 15000))) +
    geom_smooth(method = "loess") +
    scale_x_log10("Number of followers, of this person", label = comma) +
    scale_y_log10("Trimmed mean number of followers,\nof the people this person follows", label = comma) +
    ggtitle("Do people with more followers follow people with more followers?",
            "Apparently 'no'.")
  

follow_data_2 %>%
 arrange(desc(number_followers)) %>%
	View

follow_data %>%
 arrange(number_followers)

follow_data %>%
 arrange(number_following)


follow_data_l <- follow_data %>%
	filter(!is.na(number_followers)) %>%
	select(-screenName) %>%
	map_df(function(x){log(x + 1)}) 

ggpairs(follow_data_l)

mod_full <- gam(trmean_ff ~ s(number_followers, number_following), data = follow_data_l)
mod_simp <- gam(trmean_ff ~ s(number_followers) + s(number_following), data = follow_data_l)
anova(mod_full, mod_simp, test = "F")

summary(mod)
plot(mod_full, pages = 1, scheme = 1)
plot(mod_full, pages = 1, scheme = 2)

plot(mod_simp, pages = 1)
nrow(follow_data)
summary(follow_data_2_
