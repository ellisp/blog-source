library(tidyverse)
library(lubridate)
library(janitor)
library(glue)
library(ggrepel)

# https://correlatesofwar.org/data-sets/cow-war/


#----- import interstate war data----------------------

interstate <- read_csv("https://correlatesofwar.org/wp-content/uploads/Inter-StateWarData_v4.0.csv") |> 
  clean_names() |> 
  mutate(start_date = as.Date(sprintf("%04d-%02d-%02d", start_year1, start_month1, start_day1)),
         end_date = as.Date(sprintf("%04d-%02d-%02d", end_year1, end_month1, end_day1)))

interstate_wars <- interstate |> 
  group_by(war_num, war_name) |> 
  summarise(earliest_start= min(start_date),
            latest_end = max(end_date),
            bat_death = sum(bat_death)) |> 
  mutate(duration = as.numeric(latest_end - earliest_start),
         start_year = year(earliest_start)) |> 
  ungroup()

# what years covered? 1823 to 2003 at time of writing
range(interstate_wars$start_year)

#==========================plots=================
 
simple_caption <- "Source: Correlates of War, Inter-State War Data; analysis by freerangestats.info"

#-----------------distribution of duration------------
summary(interstate_wars$duration)

sim_norm <- data.frame(duration = 10 ^ (rnorm(1e6, 
                                        mean = log10(interstate_wars$duration), 
                                        sd = sd(log10(interstate_wars$duration)))))

p1 <- interstate_wars |> 
  ggplot(aes(x = duration)) +
  geom_density() +
  geom_rug() +
  geom_density(data = sim_norm, colour = "orange") +
  annotate("text", x= 5000, y = 0.18, label = "Simulated log-normal distribution", 
           colour = "orange", hjust = 0) +
  annotate("text", x= 500, y = 0.45, label = "Empirical distribution of war durations", 
           colour = "black", hjust = 0) +
  scale_x_log10(label = comma) +
  labs(x = "Duration of wars (in days, logarithmic scale)",
       y = "Density",
       title = "Distribution of war durations, 1823 to 2003",
       subtitle = "More concentrated, less-fat tails than a log-normal distribution",
       caption = simple_caption)
  
svg_png(p1, "../img/0321-density", w = 8, h = 5)

interstate_cumulative <- interstate_wars |> 
  arrange(duration) |> 
  mutate(cumulative_freq = 1:n() / n()) 

model <- loess(cumulative_freq ~ log(duration), data = interstate_cumulative)
current_dur <- 74 # as at 13 May 2025 - war started 28 February 2026
current_cf <- predict(model, newdata = data.frame(duration = current_dur))

inv_model <- loess(duration ~ x, 
                   data = data.frame(duration = interstate_cumulative$duration, 
                                     x = fitted(model)))

# of wars that last this long, what is the median cumulative frequence:
conditional_median_freq <- (1 - current_cf) / 2 + current_cf
# of wars with that median cumulative frequency, convert it back into a duration,
conditional_median_dur <- predict(inv_model, data.frame(x = conditional_median_freq))


p2 <- interstate_cumulative |> 
  ggplot(aes(x = duration, y = cumulative_freq)) +
  geom_smooth(method = "loess", colour = "grey80") +
  geom_line() +
  # note that (seems a bit odd) need to manually do the scale transform to geom_segment here:
  geom_segment(x = log10(current_dur), xend = log10(current_dur), y = -Inf, yend = current_cf, colour = "red") +
  geom_segment(x = 0, xend = log10(current_dur), y = current_cf, yend = current_cf, colour = "red") +
  geom_segment(x = log10(conditional_median_dur), xend = log10(conditional_median_dur), y = -Inf, yend = conditional_median_freq, colour = "blue") +
  geom_segment(x = 0, xend = log10(conditional_median_dur), y = conditional_median_freq, yend = conditional_median_freq, colour = "blue") +
  
  annotate("text", x = current_dur * 0.95, y = 0.39, label = "Current Iran war", colour = "red", hjust = 1) +
  annotate("text", x = conditional_median_dur * 1.05, y = 0.62, colour = "blue", hjust = 0, vjust = 1, 
           label = glue("Median expectation conditional 
on at least {current_dur} days")) +
  scale_x_log10(label = comma, breaks = c(10, current_dur, 100, conditional_median_dur, 1000)) +
  labs(x = "Total duration of war (in days, logarithmic scale)",
       y = "Cumulative frequency of wars",
       title = "Expectations of duration of Iran war, based on modern inter-state wars' duration",
       subtitle = glue("Comparison to wars from 1823 to 2003. The median war that lasts {current_dur} days goes on to last {round(conditional_median_dur)} days."),
       caption = simple_caption)

svg_png(p2, "../img/0321-cumulative-density", w = 8, h = 5)


#------------------Compare duration and number of deaths----------------
p3 <- interstate_wars |> 
  ggplot(aes(x = duration, y = bat_death, label = war_name)) +
  geom_point(aes(colour = start_year), size = 3.5) +
  geom_text_repel(colour = "grey50", size = 2) +
  scale_y_log10(label = comma) +
  scale_x_log10(label = comma) +
  scale_colour_viridis_c() +
  labs(title = "Inter-state wars, 1823-2003",
       colour = "Starting year",
       x = "Duration in days",
       y = "Number of battle deaths",
       caption = simple_caption) +
  theme(legend.position = c(0.15, 0.8))

svg_png(p3, "../img/0321-duration-deaths", w = 8, h = 5)


#------------Compare duration with when in history it happened---------------
p4 <- interstate_wars |> 
  arrange(bat_death) |> 
  ggplot(aes(x = earliest_start, y = duration)) +
  geom_hline(yintercept = current_dur, colour = "red") +
  geom_point(aes(size = bat_death), shape = 1) +
  geom_text_repel(aes(label = war_name), colour = "steelblue", size = 3) +
  annotate("text", x= as.Date("1820-01-01"), y = current_dur + 8, hjust = 0,
           label = "Duration of 2026 US-Israel-Iran war so far", colour = "red") +
  scale_y_log10(label = comma) +
  scale_size_area(label = comma, max_size = 25) +
  labs(title = "Inter-state wars, 1823-2003",
       subtitle = glue("Compared to the USA-Israel-Iran war as at {Sys.Date()}"),
       x = "Start of war",
       y = "Duration of war (days)",
       size = "Number of batlle deaths:",
       caption = simple_caption)

svg_png(p4, "../img/0321-duration-history", w = 13, h = 9)
