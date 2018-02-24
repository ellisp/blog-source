library(tidyverse)
library(scales)
library(WDI)
library(ineq) # for Gini
library(acid) # for weighted.gini
library(forcats)
library(RColorBrewer)
library(directlabels)
library(ggrepel)
library(testthat)
library(forecastHybrid)
library(stringr) # for str_wrap

# Colour palette for making sure charts use the same colours:
gini_palette <- brewer.pal(3, "Set2")
names(gini_palette)[c(1,2)] <- c("Unweighted", "Weighted")


#==============download data===========
tmp1 <- WDIsearch("GDP per capita")
tmp2 <- WDIsearch("population")
# "NY.GDP.PCAP.PP.KD"    "GDP per capita, PPP (constant 2005 international $)"  
# - this one sounds and looks like what Milanovic used but now it only goes to 1990
# In http://databank.worldbank.org/data/download/WDIrevisions.xls,
# it is stated that from May 2014 PPP data are onlyu provided back to 1990 due
# to concerns about the risk of inaccuracy the further back from the benchmark year

# alternatives:
# "NY.GDP.PCAP.KD"       "GDP per capita (constant 2000 US$)" ie not adjusted for inter-country price differences
# "GDP.PC.KD"              "GDP per Capita, constant US$, millions" - doesn't seem to exist
# "NY.GDP.PCAP.CD"       "GDP per capita (current US$)"    ie not corrected for inflation at all
#  "NY.GDP.PCAP.KN"       "GDP per capita (constant LCU)"  ie local currency
# SPPOPTOTL Population, millions at least this is straightforward...

gdp_ppp <- WDI(indicator = c("NY.GDP.PCAP.PP.KD", "SP.POP.TOTL"), start = 1960, end = 2016, extra = TRUE)
gdp_constant <-  WDI(indicator = c("NY.GDP.PCAP.KD", "SP.POP.TOTL"), start = 1960, end = 2016, extra = TRUE)


#============time series chart=================
p1 <- gdp_constant %>%
  rename(gdp = NY.GDP.PCAP.KD)  %>%
  filter(country %in% c("United States", "United Kingdom", "France", "China", "India", "Thailand", "South Africa")) %>%
  ggplot(aes(x = year, y = gdp, colour = country)) +
  geom_line() +
  scale_y_log10("GDP per capita, constant US dollars", label = dollar) +
  labs(x = "", caption = "Source: World Development Indicators") +
  ggtitle("GDP per capita over time, selected countries")

svg("../img/0105-line-charts.svg", 8, 5)
direct.label(p1)
dev.off()


#============country coverage==========
# first we need to work out which "countries" are actually regional aggregations
country_codes <- gdp_ppp %>%
  dplyr::select(iso2c, country) %>%
  distinct()
# everything begining with X except XK (Kosovo) is a grouping eg "Euro area"
# everything with a number in it is a a group eg T5 is South Asia (IDA and IBRD)
# ZF, ZG, ZQ, ZT, ZJ are regions
# OE is the OECD
# EU is the European Union

codes <- country_codes$iso2c
exes <- codes[grepl("^X", codes)]
exes <- exes[exes != "XK"]

# regs has the iso2c codes for regional aggregfations:
regs <- c(exes, codes[grepl("[0-9]", codes)], "ZF", "ZG", "ZQ", "ZT", "ZJ", "OE", "EU")

# how many countries have data each year?
svg("../img/0105-data-coverage.svg", 8, 5)
gdp_constant %>%
  filter(!iso2c %in% regs) %>%
  group_by(year) %>%
  summarise(countries = sum(!is.na(NY.GDP.PCAP.KD))) %>%
  ggplot(aes(x = year, y = countries)) +
  ggtitle("Variable GDP coverage in the World Development Indicators") +
  geom_line() +
  labs(y = "Number of countries with \nconstant US prices GDP data in the WDI", 
       caption = "Source: World Development Indicators")
dev.off()

# which countries have good data coverage?:
country_coverage <- gdp_constant %>%
  filter(!iso2c %in% regs) %>%
  group_by(country) %>%
  summarise(prop_complete = sum(!is.na(NY.GDP.PCAP.KD)) / n()) %>%
  arrange(desc(prop_complete))

# some surprises here.  For example New Zealand only has data from 1977 onwards
svg("../img/0105-countries-coverage.svg", 10, 12)
country_coverage %>%
  filter(prop_complete != 0 & prop_complete != 1) %>%
  mutate(country = fct_reorder(country, prop_complete)) %>%
  ggplot(aes(x = prop_complete, y = country)) +
  geom_point() +
  ggtitle("Countries with partial GDP data in the WDI") +
  scale_x_continuous("Percentage of years 1960-2016 with data on GDP in constant US dollars",
                     label = percent) +
  labs(y = "", caption = "Source: World Development Indicators")
dev.off()  

#================Variant of figure 4.1 on Gini over time===========
# constant prices version, which we will then rbind onto the PPP version
tmp <- gdp_constant %>%
  rename(gdp = NY.GDP.PCAP.KD) %>%
  mutate(var = "USD exchange rates")

svg("../img/0105-gini-usd.svg", 8, 5)
gdp_ppp %>%
  rename(gdp = NY.GDP.PCAP.PP.KD) %>%
  mutate(var = "Purchasing Power Parity") %>%
  rbind(tmp) %>%
  filter(country %in% filter(country_coverage, prop_complete == 1)$country) %>%
  # need to knock out the NA values in the PPP data:
  filter(!is.na(gdp)) %>%
  group_by(year, var) %>%
  summarise(Unweighted = Gini(gdp),
            Weighted = weighted.gini(gdp, SP.POP.TOTL)$bcwGini) %>%
  gather(variable, value, -year, -var) %>%
  mutate(variable = fct_reorder(variable, -value, fun = last)) %>%
  ggplot(aes(x = year, y = value, colour = variable)) +
  facet_wrap(~var) +
  geom_line() +
  theme(legend.position = "right") +
  labs(colour = "",
       y = "Gini or weighted Gini",
       caption = "Source: World Development Indicators") +
  ggtitle("Inter-country inequality over time",
          "GDP per capita, two different methods of comparing across countries
Only includes countries with full coverage of data 1960 to 2016
Inspired by Figure 4.1 from Milanovic, Global Inequality") +
  scale_x_continuous("", limits = c(1960, 2020)) +
  scale_colour_manual(values = gini_palette) 
dev.off()

#================Figure 4.2====================

# excluding Vietnam because they don't have any data now in WDI until the 1980s
# adding in some other countries by population - not clear why excluded from the original
emerging <- c("India", "Brazil", "Indonesia", "South Africa", "Pakistan", "Bangladesh", "Nigeria",
              "Mexico", "Thailand", "Philippines", "Egypt, Arab Rep.", "Turkey", "Korea, Rep.",
              "Iran, Islamic Rep.", "Myanmar", "Colombia", "Congo, Dem. Rep.")

expect_equal(sum(!emerging %in% gdp_constant$country), 0)

svg("../img/0105-growth-gap.svg", 9, 6)
gdp_constant %>% 
  rename(gdp = NY.GDP.PCAP.KD,
         pop = SP.POP.TOTL) %>%
  filter(country %in% c("European Union", "United States", "Japan", emerging)) %>%
  mutate(type = ifelse(country %in% emerging, "emerging", "advanced")) %>%
  group_by(year, type) %>% 
  # needs to be false so bars don't draw when not present
  summarise(gdp = sum(gdp * pop, na.rm = FALSE)) %>%
  group_by(type) %>%
  mutate(diffgdp = c(NA, diff(gdp)),
         growth = diffgdp / (gdp - diffgdp)) %>%
  dplyr::select(year, type, growth) %>%
  spread(type, growth) %>%
  mutate(difference = emerging - advanced) %>%  
  ggplot(aes(x = year, weight = difference)) +
  geom_bar() +
  ggtitle("GDP growth gap between 'emerging' and 'advanced' countries over time",
          "Excluding China for illustrative reasons, and Vietnam for data reasons.
Inspired by Figure 4.2 from Milanovic, Global Inequality") +
  scale_y_continuous("Difference in population-weighted growth rate between 
emerging and advanced economies, in percentage points",
                     label = percent) +
    labs(x = paste(str_wrap(paste0("Emerging countries defined as ", 
                             paste(emerging, collapse = ", ")),
                             100),
             "\n\nAdvanced countries defined as EU, USA and Japan"), 
         caption = "Source: World Development Indicators")
dev.off()

# This is much more positive in the late 1960s and 1970s than in the original,
# and perhaps this is all because he had Vietnam (during the war...) and I don't
# have data.


#====================Figure 4.3 scatter plots========
end_year <- 2016
gdp_summary <- gdp_constant %>%
  filter(!iso2c %in% regs & year >= 1970) %>%
  rename(gdp = NY.GDP.PCAP.KD,
         pop = SP.POP.TOTL) %>%
  arrange(year) %>%
  mutate(country_type = ifelse(income == "High income: OECD", "Western", "Other"),
         country_type = ifelse(region == "East Asia & Pacific (all income levels)" | 
                                 country %in% c("India", "Bangladesh", "Nepal", "Pakistan"), 
                               "Asian", country_type)) %>%
  group_by(iso2c, country, country_type) %>%
  summarise(gdp1970 = gdp[year == 1970],
            growth = (gdp[year == end_year] / gdp1970) ^ (1 / (end_year - 1970)) - 1,
            pop1970 = pop[year == 1970]) %>%
  filter(!is.na(growth))  %>%
  mutate(plot_label = ifelse(country_type == "Other", 
                                    "Excluding Asian and Western countries", 
                                    "Asian and Western countries"))

gdp_summary$plot_label <- relevel(factor(gdp_summary$plot_label), "Excluding Asian and Western countries")
gdp_summary$country_type <- fct_relevel(factor(gdp_summary$country_type), c("Asian", "Western"))

svg("../img/0105-scatter.svg", 9, 6)
gdp_summary %>%
  ggplot(aes(x =  gdp1970, y = growth)) +
  facet_wrap(~plot_label) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey40", linetype = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey40", aes(weight = pop1970)) +
  geom_point(aes(colour = country_type, size = pop1970 / 10^6), alpha = 0.5) +
  geom_point(aes(size = pop1970 / 10^6), colour = "black", shape = 1) +
  # next line would give labels for points but horribly cluttered plot
  # geom_text_repel(aes(label = iso2c)) +
  scale_x_log10("\nGDP per capita in 1970\n\nDotted line is unweighted regression; solid line is population-weighted regression.\n", 
                label = dollar, breaks = c(1000, 5000, 20000)) +
  scale_y_continuous(paste("Average growth between 1970 and", end_year), label = percent) +
  scale_size_area("Population\nin 1970 (m)", max_size = 10) +
  labs(colour = "Region",
       caption = "Source: World Development Indicators") +
  ggtitle("GDP in 1970 and subsequent growth",
          "Constant prices, US dollars\nInspired by Figure 4.3 from Milanovic, Global Inequality") +
  theme(legend.position = "right")
dev.off()

# Note the pattern is the same as in Milanovich's graphic, even though 
# the growth rates seem different





#================forecasts of global gini=============
# this is only a taster for what is described in Excursus 4.1 in Milanovic's book
# because it only deals with inter-national inequality, and uses very crude forecasts!
# but it comes up with a modestly similar result.  I see a drop in inter-national inequaltiy
# of about 0.07 whereas they find a drop in global inequality of 0.04.  These feel quite
# potentially consistent (because most forecasts for intranational inequaltiy are increases).

# could do the forecasts of Gini by forecasting GDP per capita, and using the population 
# forecasts from https://esa.un.org/unpd/wpp/Download/Standard/Population/
# but.... effort!...
# so let's just do our own forecasts

#' Forecast gdp and pop for one country gdp_constant
#' and combines it with the historical data
#' context-specific function
forecast_gdp_pop <- function(the_country, h = 20){
  print(the_country) # for debugging
  # assumes presence in environment of a data.frame called gdp_constant
  country_vars <- gdp_constant %>%
    rename(gdp = NY.GDP.PCAP.KD,
           pop = SP.POP.TOTL) %>%
    arrange(year) %>%
    filter(country == the_country) %>%
    dplyr::select(gdp, pop) %>%
    ts(start = 1960) 

  # a bit of fiddliness is needed because some countries are missing early GDP per person data.
  # maybe they should be knocked out altogether of course... but let's decide that
  # later and explicitly, this function shoudl be able to handle it either way
  years_orig <- time(country_vars)
  rows_include <- which(!is.na(country_vars[ , "gdp"]))
  # some "countries" have virtually no observations, so don't even try to forecast them...
  if(length(rows_include) > 5){ 
    
    country_vars <- country_vars[rows_include, ]
  
    mod_gdp <- hybridModel(country_vars[ , "gdp"], 
                           lambda = 0, models = "ae", verbose = FALSE)
    mod_pop <- hybridModel(country_vars[ , "pop"], 
                           lambda = 0, models = "ae", verbose = FALSE)
    
    fc_gdp <- forecast(mod_gdp, h = h)
    fc_pop <- forecast(mod_pop, h = h)
    
    years <- years_orig[rows_include]
    years <- c(years, max(years + 1):(max(years) + h))
    
    
    tmp <- data.frame(
      country = the_country,
      gdp_pp = c(fc_gdp$x, fc_gdp$mean),
      pop = c(fc_pop$x, fc_pop$mean),
      year = years
    )} else {
      tmp <- NULL
    }
  return(tmp)
}


# test cases.  Is designed to work even with countries with some NA values,
# even though when doing it for real I decided to drop them.
forecast_gdp_pop("Australia", h = 100) %>%
  gather(variable, value, -year, -country) %>%
  ggplot(aes(x = year, y = value)) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  geom_line() +
  ggtitle("Crude forecast of GDP per person and population for Australia")

forecast_gdp_pop("China", h = 5) %>%
  gather(variable, value, -year, -country) %>%
  ggplot(aes(x = year, y = value)) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  geom_line() +
  ggtitle("Crude forecast of GDP per person and population for China")

forecast_gdp_pop("Vietnam") %>%
  gather(variable, value, -year, -country) %>%
  ggplot(aes(x = year, y = value)) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  geom_line()

# vector of all the countries with complete data:
all_countries <- filter(country_coverage, prop_complete == 1)$country

all_forecasts <- lapply(all_countries, forecast_gdp_pop)

all_forecasts_df <- do.call("rbind", all_forecasts)

svg("../img/0105-gini-forecasts.svg", 8, 4)
all_forecasts_df %>%
  group_by(year) %>%
  summarise(Unweighted = Gini(gdp_pp),
            Weighted = weighted.gini(gdp_pp, pop)$bcwGini) %>%
  gather(variable, value, -year) %>%
  mutate(variable = fct_reorder(variable, -value, fun = last)) %>% 
  mutate(type = ifelse(year <= 2016, "Actual", "Forecast")) %>%
  ggplot(aes(x = year, y = value, colour = variable, linetype = type)) +
  geom_line() +
  theme(legend.position = "right") +
  labs(colour = "", linetype = "", x = "",
       y = "Gini or weighted Gini",
       caption = "Source: World Development Indicators") +
  ggtitle("Inter-country inequality over time, including crude forecasts",
          "GDP per capita, two different methods of comparing across countries
Only includes countries with complete data 1960 to 2013") +
  scale_colour_manual(values = gini_palette)
dev.off()



convert_pngs("0105")
