


library(tidyverse)
library(ggseas)
library(mgcv)

set.seed(123)


sim_n <- 10000

observed_time <- round(cumsum(rexp(250, 0.1)))

d <- tibble(time = 1:sim_n,
            y = rescale(cumsum(as.numeric(arima.sim(n = sim_n, 
                                             model = list(ar = c(0.7, 0.1), ma = c(0.5)))
                                             )))) %>%
  filter(time %in% observed_time) %>%
  mutate(observed = rbinom(n(), size = 1, prob = y))

p0 <- ggplot(d, aes(x = time, y = observed)) +
  geom_point()  +
  labs(x = "Time", 
       y = "Observations",
       subtitle = "Points at 0 and 1 show actual observations")

p1 <- p0 +
  geom_line(aes(y = y)) 

artificial_legend <- function(plot, lab1 = "True underlying process", lab2, palette = c("black", "blue")){
  names(palette) <- c(lab1, lab2)
  
   plot <- plot + 
     geom_line(data = tibble(z = c(lab1, lab2), time = 1:2, observed = 0:1), 
             aes(colour = z), size = 1.5, alpha = 0) +
     scale_colour_manual(values = palette) +
     guides(colour = guide_legend(override.aes = list(alpha = 1))) +
     labs(colour = "Smoothing methods:")

  return(plot)
}

(p1 + geom_smooth(method = "gam", se = FALSE)) %>%
  artificial_legend(lab2 = "Generalized Additive Model")
  

p0  +
  geom_smooth(method = "loess", se = FALSE)

p0 +
  geom_line(aes(y = y)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.4)


p0 +
  geom_line(aes(y = y)) +
  stat_rollapplyr(width = 10, colour = "blue", align = "center")

ggplot(d, aes(x = time, y = observed)) +
  geom_point() +
  stat_rollapplyr(width = 10, colour = "blue", align = "center")






model <- gam(observed ~ s(time), data = d)
summary(model)
plot(model)
