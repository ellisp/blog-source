library(frs)
library(tidyverse)
library(scales)
library(WDI)

# only crude death rate at https://esa.un.org/unpd/wpp/Download/Standard/Mortality/

# lifetables give "a set of values showing the mortality experience of a hypothetical 
# group of infants born at the same time and subject throughout their lifetime to the 
# specific mortality rates of a given period." 
# (from above UN DESA (ie the UN Population Division) link)


# good description of age-adjusting here: https://health.mo.gov/data/mica/CDP_MICA/AARate.html
# "A  "standard" population distribution is used to adjust death and hospitalization rates. The 
# age-adjusted rates are rates that would have existed if the population under study had the 
# same age distribution as the "standard" population."
# So it's important to use the same standard population... NCHS in the US recommends using the 
# US 2000 standard population
# 

# Note at https://en.wikipedia.org/wiki/Age_adjustment it confirms age adjustment is not needed
# for life expectancy which is calculated directly from age-specific mortality rates

# Use WDIsearch to find the indicators we want
# View(WDIsearch("life"))

death_rate <- WDI(indicator = "SP.DYN.CDRT.IN", start = 1960, end = 2100)
life_exp <- WDI(indicator = "SP.DYN.LE00.IN", start = 1960, end = 2100)

selected_countries <- c("United Arab Emirates", "Suriname", "Georgia", "Barbados", "Kiribati", 
                        "Moldova", "Ghana", "Japan", "New Zealand")

svg("../img/0123-life-death-csp.svg", 8, 7)
# connected scatter plot:
death_rate %>%
  filter(country %in% selected_countries) %>%
  inner_join(life_exp, by = c("iso2c", "country", "year")) %>%
  # take out NAs which confuse the geom_text call later:
  filter(!is.na(SP.DYN.LE00.IN)) %>%
  # order country by life expectancy so graphic facets make some sense:
  mutate(country = fct_reorder(country, SP.DYN.LE00.IN)) %>%
  ggplot(aes(x = SP.DYN.CDRT.IN, y = SP.DYN.LE00.IN, colour = country)) +
  geom_path() +
  # add labels of the year at max and min points
  geom_text(aes(label = ifelse(year %in% range(year), year, "")), colour = "grey30", size = 3) +
  facet_wrap(~country, scales = "free") +
  # these next two geom_blank calls are a hack to make sure enough space is given for the labels which
  # otherwise crash into the axes, and can't be easily controlled with scale limits:
  geom_blank(aes(x = SP.DYN.CDRT.IN * 1.1, y = SP.DYN.LE00.IN * 1.05)) +
  geom_blank(aes(x = SP.DYN.CDRT.IN * 0.9, y = SP.DYN.LE00.IN * 0.95)) +
  theme(legend.position = "none") +
  labs(x = "Crude death rate (per 1,000 people)",
       y = "Life Expectancy",
       caption = "Source: World Bank World Development Indicators") +
  ggtitle("Differing relationship of crude death rate and life expectancy",
          "Countries with aging populations (eg Georgia, Japan, in the below) can experience increases in both simultaneously.")
dev.off()


svg("../img/0123-french-rates.svg", 8, 5)
french_death_rates_2015 %>%
  gather(sex, value, -age) %>%
  ggplot(aes(x = age, y = (1000 - value) / 1000, colour = sex)) +
  geom_line() +
  labs(x = "Age", y = "Probability of surviving to the next year", colour = "",
       caption = "https://www.ined.fr/en/everything_about_population/data/france/deaths-causes-mortality/mortality-rates-sex-age/") +
  coord_cartesian(ylim = c(0.8, 1), xlim = c(0, 100)) +
  ggtitle("French death rates in 2015") +
  theme(legend.position = c(0.6, 0.6))
dev.off()

svg("../img/0123-0.svg", 8, 5)
m <- french_death_rates_2015[ , c("age", "male")]
the_age <- which(m$age == 0) # row in the data frame for this age of interest
current_level <- life_expectancy(data = m)
data.frame(
  dr = 0:1000,
  le =   sapply(0:1000, function(i){
    m[1, 2] <- i  # m is modified only within the environment of this function
    life_expectancy(data = m) 
})
) %>% 
  ggplot(aes(x = dr, y = le)) +
  geom_line() +
  annotate("point", french_death_rates_2015[the_age, "male"], y = current_level, size = 3, colour = "steelblue") +
  ggtitle("Impact of different infant mortality rates on life expectancy",
          "Keeping death rates at all other ages the same, at levels for French males in 2015") +
  labs(x = "Deaths in first year of life per thousand live births",
       y = "Life expectancy at birth for overall cohort") +
  ylim(c(0, 100))
dev.off()

svg("../img/0123-17.5.svg", 8, 5)
the_age <- which(m$age == "17.5")
data.frame(
  dr = 0:1000,
  le =   sapply(0:1000, function(i){
    m[the_age, 2] <- i
    m <- rbind(m, data.frame(age = c(16.5, 18.5), male = c(0.3, 0.3)))
    life_expectancy(data = m)  
  })
) %>%
  ggplot(aes(x = dr, y = le)) +
  geom_line() +
  annotate("point", french_death_rates_2015[the_age, "male"], y = current_level, size = 3, colour = "steelblue") +
  ggtitle("Impact of different death rates in early adulthood on life expectancy",
          "Keeping death rates at all other ages the same, at levels for French males in 2015") +
  labs(x = "Deaths at age 17.5 per thousand people living to 16.5",
       y = "Life expectancy at birth for overall cohort") +
  ylim(c(0, 100))
dev.off()

svg("../img/0123-85.svg", 8, 5)
the_age <- which(m$age == "85")
data.frame(
  dr = 0:1000,
  le =   sapply(0:1000, function(i){
    m[the_age, 2] <- i
    m <- rbind(m, data.frame(age = c(84, 86), male = c(78, 78)))
    life_expectancy(data = m)  
  })
) %>%
  ggplot(aes(x = dr, y = le)) +
  geom_line() +
  annotate("point", french_death_rates_2015[the_age, "male"], y = current_level, size = 3, colour = "steelblue") +
  ggtitle("Impact of different death rates in old age on life expectancy",
          "Keeping death rates at all other ages the same, at levels for French males in 2015") +
  labs(x = "Deaths at age 85 per thousand people living to 84",
       y = "Life expectancy at birth for overall cohort") +
  ylim(c(0, 100))
dev.off()

svg("../img/0123-85-over.svg", 8, 5)
the_age <- which(m$age == "85")
mr <- nrow(m) - 1
data.frame(
  dr = 0:1000,
  le =   sapply(0:1000, function(i){
    m[the_age:mr, 2] <- i
    life_expectancy(data = m)  
  })
) %>% 
  ggplot(aes(x = dr, y = le)) +
  geom_line() +
  annotate("point", french_death_rates_2015[the_age, "male"], y = current_level, size = 3, colour = "steelblue") +
  ggtitle("Impact of different death rates in old age on life expectancy",
          "Keeping death rates at all other ages the same, at levels for French males in 2015") +
  labs(x = "Deaths at age 85 and all older ages until 150 (when all die), per thousand people living to 84",
       y = "Life expectancy at birth for overall cohort") +
  ylim(c(0, 100))
dev.off()

convert_pngs("0123")
