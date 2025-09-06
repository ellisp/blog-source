

library(tidyverse)
library(spcstyle)
library(scales)
library(ggtext)
library(RColorBrewer)
library(WDI)

#---------------download data, set up palette---------------

# Read in the ANU's PNG economic database. Download from 
# https://pngeconomic.devpolicy.org/
pnged <- read_csv("PNG economic database.csv")

glimpse(pnged)
count(pnged, Variable)

unique(pnged$Variable)

# era_cols <- brewer.pal(6, "Set1")[1:4]
era_cols <- c("grey10", "white", "grey10", "white")
gdp_cols <- brewer.pal(7, "Set1")[c(5,7)]

#-----------GDP chart with GDP deflator-----------

def_13_22 <- pnged |> 
  filter(Variable == "GDP deflator") |> 
  summarise(x = Amount[Year == 2022] / Amount[Year == 2013]) |> 
  pull(x)


# this is basically Figure 1.2 from the introduction
p1 <- pnged |> 
  filter(Variable %in% c("Non-resource GDP (constant prices, new series)", 
                         "GDP (constant prices, new series)", "Population")) |> # distinct(Variable, Units)
  select(Variable, Year, Amount) |> 
  spread(Variable, Amount) |> 
  mutate(nr_gdp_pp = `Non-resource GDP (constant prices, new series)` / Population * 1e6 * def_13_22,
        gdp_pp = `GDP (constant prices, new series)` / Population * 1e6 * def_13_22) |> 
  select(Year, nr_gdp_pp, gdp_pp) |> 
  gather(variable, value, -Year) |> 
  drop_na() |> 
  ggplot(aes(x = Year, y = value, colour = variable)) +
  annotate("rect", xmin = 1975, xmax = 1988.5, ymin = -Inf, ymax = Inf, fill = era_cols[1], alpha = 0.1) +
  annotate("rect", xmin = 1988.5, xmax = 2003.5, ymin = -Inf, ymax = Inf, fill = era_cols[2], alpha = 0.1) +
  annotate("rect", xmin = 2003.5, xmax = 2013.5, ymin = -Inf, ymax = Inf, fill = era_cols[3], alpha = 0.1) +
  annotate("rect", xmin = 2013.5, xmax = 2022.5, ymin = -Inf, ymax = Inf, fill = era_cols[4], alpha = 0.1) +
  geom_line(linewidth = 2) +
  # option, can uncomment this and you get a point showing each observation. 
  # It is helpful to see the actual point, but adds clutter.
#  geom_point(colour = "white") +
  annotate("text", label = c("'Struggle'", "'Reform'", "'Boom'", "'Bust'"), y = 13300, 
           x = c(1981.5, 1996, 2009, 2018), hjust = 0.5, fontface = 4, alpha = 0.8) +
  annotate("text", colour = gdp_cols, x = 2020, y = c(11300, 9200), 
           label = c("All GDP", "Non-resources GDP")) +
  scale_colour_manual(values = gdp_cols) +
  scale_y_continuous(label = comma, breaks = 6:13 * 1000) +
  labs(y = "Kina (2022 prices, based on GDP deflator)",
        x = "",
       title = "Real gross domestic product per person in Papua New Guinea",
       subtitle = "Annotated with the periods used in <i>Struggle, reform, boom and bust: an economic history of Papua New Guinea since independence</i>",
       caption = "Source: ANU's PNG Economic Database, https://pngeconomic.devpolicy.org/")  +
   theme(legend.position ="none",
         plot.subtitle = element_markdown())
svg_png(p1, "../img/0304-gdp", w = 9, h = 5)

file.copy("../img/0304-gdp.svg", 
          "C:/Users/petere/OneDrive - SPC/Documents/misc notes/0304-gdp.svg",
          overwrite = TRUE)
#-------------------------CPI so we see prices facing consumers---------

 # ratio of CPI to non-resource GDP deflator
 cpi_def <- pnged |> 
   filter(Variable%in% c("Non-resource GDP deflator", "CPI deflator")) |> 
   select(Variable, Year, Amount) |> 
   spread(Variable, Amount) |> 
   # rebase to a set year:
   mutate(across(`CPI deflator`:`Non-resource GDP deflator`, 
                 function(x){x / x[Year == 1990]})) |> 
   mutate(ratio = `CPI deflator` / `Non-resource GDP deflator`) 
 
 # from 1990 to 2022, CPI has increased about 40% more than the GDP deflator
 # so if you want to see the living standards of PNGans, there is a case to use
 # the CPI instead
 
p2 <- pnged |> 
   filter(Variable %in% c("Non-resource GDP (current prices, new series)", 
                          "GDP (current prices, new series)", "Population")) |> # distinct(Variable, Units)
   select(Variable, Year, Amount) |> 
   spread(Variable, Amount) |> 
   mutate(nr_gdp_pp = `Non-resource GDP (current prices, new series)` / Population * 1e6,
          gdp_pp = `GDP (current prices, new series)` / Population * 1e6 ) |> 
   select(Year, nr_gdp_pp, gdp_pp) |> 
   gather(variable, value, -Year) |> 
   drop_na() |>
   left_join(cpi_def, by = "Year") |> 
   mutate(value = value  / `CPI deflator` * filter(cpi_def, Year == 2024)$`CPI deflator`) |> 
   ggplot(aes(x = Year, y = value, colour = variable)) +
   annotate("rect", xmin = 1975, xmax = 1988.5, ymin = -Inf, ymax = Inf, fill = era_cols[1], alpha = 0.1) +
   annotate("rect", xmin = 1988.5, xmax = 2003.5, ymin = -Inf, ymax = Inf, fill = era_cols[2], alpha = 0.1) +
   annotate("rect", xmin = 2003.5, xmax = 2013.5, ymin = -Inf, ymax = Inf, fill = era_cols[3], alpha = 0.1) +
   annotate("rect", xmin = 2013.5, xmax = 2022.5, ymin = -Inf, ymax = Inf, fill = era_cols[4], alpha = 0.1) +
   geom_line(linewidth = 2) +
  # option, can uncomment this and you get a point showing each observation. 
  # It is helpful to see the actual point, but adds clutter.
  #   geom_point(colour = "white") +
   annotate("text", label = c("'Struggle'", "'Reform'", "'Boom'", "'Bust'"), y = 14100, 
            x = c(1981.5, 1996, 2009, 2018), hjust = 0.5, fontface = 4, alpha = 0.8) +
   annotate("text", colour = gdp_cols, x = 2020, y = c(10200, 7600), 
            label = c("All GDP", "Non-resources GDP")) +
   scale_colour_manual(values = gdp_cols) +
   scale_y_continuous(label = comma, breaks = 6:14 * 1000) +
   labs(y = "Kina (2024 prices, based on CPI deflator)",
        x = "",
        title = "Real gross domestic product per person in Papua New Guinea",
        subtitle = "Annotated with the periods used in <i>Struggle, reform, boom and bust: an economic history of Papua New Guinea since independence</i>",
        caption = "Source: ANU's PNG Economic Database, https://pngeconomic.devpolicy.org/")  +
   theme(legend.position ="none",
         plot.subtitle = element_markdown())
# this doesn'tmatch the figure from the Treasurer because perhaps he has different
 # population numbers
 # https://devpolicy.org/struggle-reform-boom-and-bust-a-profound-wake-up-call-for-png-20250821/


svg_png(p2, "../img/0304-gdp-with-cpi", w = 9, h = 5)

 
 # some key dates:

# * 1989 Closure of Porgera following violence in Bougainville
 # * 1995 balance of payments crisis, kina floated
# * 2004 govt revenue from resources starts growing strongly
# * 2012 sharp fall in oil prices
#   2010-2013 construction for LNG project
# * 2014 de facto rationing of foreign exchange beings
# * 2022 o8l and gas prices increase with Russian invasion of Ukraine


#----------------other variables in PNG ED------

pv <- unique(pnged$Variable)
pv[grepl("employ", pv, ignore.case = TRUE)]

pop <- pnged |> 
  filter(Variable %in% c("Population")) |> 
  select(Year, population = Amount)
  


# with CPI deflator
pnged |> 
  filter(Variable %in% c("Urban mimimum wage")) |> 
  left_join(cpi_def, by = "Year") |> 
  mutate(Amount = Amount / `CPI deflator`) |> 
  ggplot(aes(x = Year, y = Amount)) +
  geom_line()

# with population deflator
pnged |> 
  filter(Variable %in% c("Total (excluding public service) employment",
                         "Public service employment")) |> 
  left_join(pop, by = "Year") |>
  mutate(Amount = Amount / population) |>
  mutate(Variable = fct_reorder(str_wrap(Variable, 30), Amount, .desc = TRUE)) |> 
  ggplot(aes(x = Year, y = Amount, colour = Variable)) +
  geom_line() +
  scale_y_continuous(label = percent, limits = c(0, 0.06))

# no deflator, multiple variables
pnged |> 
  filter(grepl("Immunization", Variable)) |> 
  ggplot(aes(x = Year, y = Amount, colour = Variable)) +
  geom_line() +
  theme_grey()

# what would be the source for this - too frequent to be a survey - must be
# a health data admin source
# DPT, HepB3, measles

#

#-----------comparison to other countries-----------
library(WDI)
wc <- WDIcache()

d <- WDIsearch("measles")


d <- WDI(indicator = "SH.IMM.MEAS")    # does not work, '403 Forbidden'



