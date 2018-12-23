
# We know from historical articles like this one
# https://www.foreignaffairs.com/articles/1928-10-01/worlds-population
# that people were estimating the world population early in the 20th century
# 
# Our World In Data at https://ourworldindata.org/world-population-growth has
# "200 years ago there were less than one billion humans living on earth."  
# "Today, there are over 7 billion of us." Footnote 
# to http://faculty.econ.ucdavis.edu/faculty/gclark/210a/readings/kremer1993.pdf


library(tidyverse)
library(viridis)
library(gridExtra)
library(ggrepel)
library(readxl)
library(testthat)
set.seed(123)
#-----------------total population size---------------

kremer <- data_frame(
  year = c(-1000000, -300000, -25000,
           -10000, -(5:1)*1000, -500, -200,
           1, 200, 400, 600, 800, 
           10:16 * 100,
           1650, 1700, 1750, 1800, 1850,
           1875, 1900, 
           192:199 * 10),
  pop = c(0.125,1,3.34,4,5,7,14,27,50,100,150,170,190,190,200,220,
          265,320,360,360,350,425,545,545,610,720,900,1200,1325,
          1625,1813,1987,2213,2516,3019,3693,4450,5333) * 1000000
) %>%
  mutate(growth = lead((pop / lag(pop)) ^ (1 / (year - lag(year))) - 1))

# note there's an error in Kremer's 3rd growth rate: 0.000031 should be 0.0000129

p1 <- kremer %>%
  ggplot(aes(x = year, y = pop)) +
  geom_line() +
  geom_point() +
  scale_y_log10(label = comma, limits = c(1e5, 5e9))  +
  scale_x_continuous("Year") +
  labs(caption = " ")

p2 <- p1 %+% filter(kremer, year > -2000)

CairoSVG("../img/0141-global-numbers.svg", 9, 4.5)
grid.arrange(p1 + 
               labs(y = "Global human population (logarithmic scale)") + 
               ggtitle("World population",
                       "1 million BC to 1999"), 
             p2 + 
               labs(y = "") + 
               ggtitle("",
                       "1,000 BC to 1999") +
               labs(caption = "Source: Kremer, 1993, 'Population Growth and Technological Change: One Million B.C. to 1990"), 
             ncol = 2)
dev.off()

CairoSVG("../img/0141-global-growth.svg", 8, 5)
kremer %>%
  filter(year > -2000) %>%
  ggplot(aes(x = year, y = growth)) +
  geom_path() +
  geom_point() +
  geom_text_repel(aes(label = year), colour = "steelblue") +
  scale_y_continuous("Annual growth rate", label = percent) +
  labs(caption = "Source: Kremer, 1993",
       x = "Year") +
  ggtitle("World population growth rates", 
          "1000 BC to present")
dev.off()

#---------------birth rate---------------
# harder
# See https://ourworldindata.org/fertility-rate
# nothing before 1950
# The gapminder R package only has data for every 5 years, from 1952
# but the gapminder website has the full data

# crude birth rate per country per year
if(!file.exists("cbr.xlsx")){
  download.file("https://docs.google.com/spreadsheet/pub?key=tUSeGJOQhafugwUvHvY-wLA&output=xlsx",
                destfile = "cbr.xlsx", mode = "wb")
}
  cbr_orig <- read_excel("cbr.xlsx") 
names(cbr_orig)[1] <- "country"

cbr <- cbr_orig %>%
  gather(year, birth_rate, -country) %>%
  mutate(year = as.integer(year))

# population per country per year
if(!file.exists("pop.xlsx")){
  download.file("https://docs.google.com/spreadsheet/pub?key=phAwcNAVuyj0XOoBL_n5tAQ&output=xlsx",
                destfile = "pop.xlsx", mode = "wb")
}
  pop_orig <- read_excel("pop.xlsx")
names(pop_orig)[1] <- "country"

pop <- pop_orig %>%
  gather(year, total_population, -country) %>%
  mutate(year = as.integer(year))

combined <- cbr %>%
  full_join(pop, by = c("year", "country"))   %>%
  arrange(country, year) %>%
  mutate(births = total_population * birth_rate / 1000) %>%
  filter(!is.na(total_population)) 

ave_br <- combined %>%
  filter(!is.na(birth_rate)) %>%
  group_by(year) %>%
  summarise(birth_rate = sum(birth_rate * total_population) / sum(total_population),
            total_population = sum(total_population)) %>%
  mutate(year_lab = ifelse(year %% 50 == 0, year, ""),
         people_born = birth_rate * total_population / 1000)


set.seed(123)
CairoSVG("../img/0141-birth-rate.svg", 8, 5)
ave_br %>%
  ggplot(aes(x = year, y = birth_rate)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = year_lab), colour = "steelblue") +
  labs(caption = "Source: Estimated from Gapminder country level data for crude birth rate and population",
       x = "Year",
       y = "Estimated global crude birth rate per 1,000 population") +
  ggtitle("Estimated global crude birth rate",
          "1800 to present")
dev.off()

CairoSVG("../img/0141-birth-rate-csp.svg", 8, 5)
ave_br %>%
  ggplot(aes(x = total_population / 1e6, y = birth_rate, label = year_lab)) +
  geom_path() +
  geom_text_repel(colour = "steelblue") +
  scale_x_continuous("Estimated global population", label = comma_format(suffix = "m")) +
  labs(y = "Estimated global crude birth rate per 1,000 population",
       caption = "Source: Estimated from Gapminder country level data for crude birth rate and population") +
  ggtitle("Estimated global crude birth rate",
          "1800 to present")
dev.off()

CairoSVG("../img/0141-birth-numbers.svg", 8, 5)
ave_br %>%
  ggplot(aes(x = year, y = people_born / 1e6)) +
  geom_line() +
  scale_y_continuous("People born per year", label = comma_format(suffix = "m")) +
  labs(caption = "Source: Estimated from Gapminder country level data for crude birth rate and population",
       x = "Year") +
  ggtitle("Estimated global births",
          "1800 to present")
dev.off()
#------------------better source for more recent years----------
library(WDI)
WDIsearch("birth rate")
wdi_cbr <- WDI(country = "1W", indicator = "SP.DYN.CBRT.IN", start = 1950, end = 2020)

CairoSVG("../img/0141-compare.svg", 7, 6)
wdi_cbr %>%
  select(year, SP.DYN.CBRT.IN) %>%
  rename(wdi = SP.DYN.CBRT.IN) %>%
  left_join(ave_br, by = "year") %>%
  mutate(year_lab = ifelse(year %% 5 == 0, year, "")) %>%
  ggplot(aes(x = birth_rate, y = wdi, label = year_lab)) +
  geom_abline(slope = 1, intercept = 0, colour = "orange") +
  geom_path() +
  geom_text_repel(colour = "steelblue") +
  labs(x = "Population-weighted average of Gapminder country data",
       y = "World Bank's World Development Indicators") +
  coord_equal() +
  ggtitle("Comparing two sources on global birth rates")
dev.off()

#-----------cumulative births-------------
twentieth_c <- data_frame(
  year = 1901:2000,
  births = approx(ave_br$year, ave_br$people_born, xout = 1901:2000)$y  
) %>%
  mutate(cum_births = cumsum(births))

CairoSVG("../img/0141-cumulative-births.svg", 8, 5)
twentieth_c %>%
  ggplot(aes(x = year, y = cum_births / 1e6)) +
  geom_line() +
  scale_y_continuous("Cumulative births in the twentieth century", label = comma_format(suffix = "m")) +
  labs(x = "Year",
       caption = "Source: Estimates based on Gapminder country level data for crude birth rate and population") +
  ggtitle("How many people born in the twentieth century?",
          paste("An estimated", format(round(sum(twentieth_c$births / 1e6)), big.mark = ","), "million"))
dev.off()

# Write a version from 1800 onwards in case people want it:
published_data <- data_frame(year = min(ave_br$year):max(ave_br$year)) %>%
  mutate( births = round(approx(ave_br$year, ave_br$people_born, xout = year)$y)) %>%
  mutate(cum_births = cumsum(births))

write_csv(published_data, "../data/cumulative-births-1800-onwards.csv")


convert_pngs("0141")
