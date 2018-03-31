
library(tidyverse)
library(scales)
library(readr)
library(lubridate)
library(ggrepel)
library(viridis)
library(rstan)
library(rsdmx)
library(ISOcodes)

#----------contextual data on suicides---------------
url <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/HEALTH_STAT/CICDHARM.TXCMFETF+TXCMHOTH+TXCMILTX.AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+NMEC+BRA+CHN+COL+CRI+IND+IDN+LTU+RUS+ZAF/all?startTime=2000&endTime=2020"

# Deaths per 100,000 population, crude and standardise
if(!exists("suic_sdmx")){
  dataset <- readSDMX(url) 
  suic_sdmx <- as.data.frame(dataset)
}

data(ISO_3166_1)

suicide <- suic_sdmx %>%
  mutate(variable = ifelse(UNIT == "TXCMILTX", "All", ""),
         variable = ifelse(UNIT == "TXCMHOTH", "Males", variable),
         variable = ifelse(UNIT == "TXCMFETF", "Females", variable)) %>%
  rename(year = obsTime, value = obsValue) %>%
  select(year, COU, value, variable) %>%
  left_join(distinct(ISO_3166_1[ , c("Alpha_3", "Alpha_2", "Name")]), by = c("COU" = "Alpha_3")) %>%
  mutate(year = as.numeric(year),
         country = Name) %>%
  as_tibble()

svg("../img/0119-suicides.svg", 11, 9)
suicide %>%
  mutate(
    variable = fct_reorder(variable, -value),
    country = fct_reorder(country, -value)
  ) %>%
  ggplot(aes(x = year, y = value, colour = variable)) +
  facet_wrap(~country) +
  geom_line() +
  labs(colour = "", x = "",
       y = "Deaths per 100,000 population (standardised rates)") +
  ggtitle("Suicide rates around the world (all methods)",
          "USA is moderately high, but not as high as it features when compared on firearm-related deaths")
dev.off()


#===============downloading gun violence archives==============

# to download these files you need to go to http://www.gunviolencearchive.org/query/
# and do a query, then choose download to CSV.  It only downloads 500 at a time.
# "Save" actually means "Execute the query".  Then you need to explicitly download CSV.
# And then you need to pick "download" (even though you're already in the "download CSV"
# environment and it has downloaded it = "download" here means "yes, really download it,
# and save it to my local drive")

# to save others doing this, a subset of the data is on my website

try(unlink("gun-violence", recursive = TRUE))
dir.create("gun-violence")
download.file("http://freerangestats.info/data/gva.zip",              
              destfile = "gun-violence/gva.zip", mode = "wb")
unzip("gun-violence/gva.zip", exdir = "gun-violence")
files <- list.files("gun-violence", full.names = TRUE, pattern = ".csv$")

gva_l <- lapply(files, read_csv)
gva <- do.call("rbind", gva_l) %>%
  mutate(date = as.Date(`Incident Date`, format = "%B %d, %Y"),
         year = year(date),
         month = month(date)) %>%
  # filter out zero deaths incidents, which we can't exclude from the original query
  # (can only filter by number of victims, not number killed)
  filter(`# Killed` > 0) %>%
  # remove duplicates in case I downloaded some data twice or there is a database error:
  distinct()

# this next graph not shown in the blog post, is just to illustrate that the data
# includes some non-2017 points
gva %>%
  ggplot(aes(x = date, y = `# Killed`)) +
  geom_jitter(alpha = 0.1) +
  ggtitle("Fatal firearm incidents in the USA; 500 sampled from selected months",
          "Points are jittered to give a sense of how much concentration there is in '1 death per incident'") +
  labs(x = "", y = "Number killed in each incident",
       caption = "Searches from Gun Violence Archive")
# Only 2014 onwards is complete data
# Only 2017 is at all representative; the res includes various other downloads!

# Histogram
svg("../img/0119-histogram.svg", 8, 3)
gva %>%
  filter(year == 2017) %>%
  ggplot(aes(x = `# Killed`)) +
  geom_histogram(binwidth = 1, aes(y = ..density..), fill = "steelblue", alpha = 0.8) +
  scale_x_continuous(breaks = 1:8) +
  scale_y_continuous("Percentage of events", label = percent) +
  ggtitle("Random sample of firearm-events in 2017",
          "Excludes events with zero people killed")
dev.off()


# Scatterplot
svg("../img/0119-scatterplot.svg", 8, 3)
gva %>%
  filter(year == 2017) %>%
  mutate(n_killed = paste0("'", ifelse(`# Killed` < 5, `# Killed`, "More than 4"), " death' incidents")) %>%
  group_by(n_killed) %>%
  summarise(incidents = n(),
            deaths = sum(`# Killed`)) %>%
  ungroup() %>%
  mutate(incidents = incidents / sum(incidents),
            deaths = deaths / sum(deaths)) %>%
  ggplot(aes(x = incidents, y = deaths, label = n_killed, colour = n_killed))  +
  geom_point(size = 2) +
  geom_text_repel() +
  theme(legend.position = "none") +
  scale_x_continuous("Proportion of incidents", label = percent) +
  scale_y_continuous("Proportion of deaths", label = percent)  +
  ggtitle("The vast majority of firearm deaths come from single-death incidents.",
          "Estimated firearms incidents and deaths in the USA in 2017, aggregated by number of deaths per incident"
    ) +
  # scale_color_viridis(discrete = TRUE, option = "D")
  scale_color_brewer(palette = "Set2")
dev.off()

# Pie chart
blank_theme <- theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"),
    axis.text.x = element_blank()
  )

svg("../img/0119-pie.svg", 8, 3)
gva %>%
  filter(year == 2017) %>%
  mutate(n_killed = paste0("'", ifelse(`# Killed` < 5, `# Killed`, "More than 4"), " death' incidents")) %>%
  group_by(n_killed) %>%
  summarise(incidents = n(),
            deaths = sum(`# Killed`)) %>%
  ungroup() %>%
  mutate(incidents = incidents / sum(incidents),
         deaths = deaths / sum(deaths)) %>%
  ggplot(aes(x = "", weight = deaths, fill = n_killed, stat = "identity")) +
  # make width 0.5 if prefer a donut...
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_brewer("Number of deaths per incident", palette = "Set2",
                     guide = guide_legend(reverse = TRUE)) +
  blank_theme +
  ggtitle("The vast majority of firearm deaths come from single-death incidents.",
          "Estimated firearms incidents and deaths in the USA in 2017, aggregated by number of deaths per incident")
dev.off()          



#=================neg binomial modelling====================
options(mc.cores = 8)

#------------------2017 sample first--------------------
data <- list(
  # x = sample(filter(gva, year == 2017)$`# Killed`, 100),
  x = filter(gva, year == 2017)$`# Killed`,
  lower_limit = 1,
  mu_prior_mean = 1,
  mu_prior_sd = 10,
  phi_prior_mean = 5,
  phi_prior_sd = 20
)

data$n <- length(data$x)

fit1 <- stan("0119-trunc-negbin.stan", data = data)

#----------------------2016 data second-------------------
# hand comparisons of the original data with that on the screen show that
# some kind of unusual sampling is going on with disrpoprotionate numbers of 
# incidents with 4 deats being dropped from the data.  There are 23 such incidents
#, 13 with 5 deaths, 3 with 6 deaths and 1 with 8 deaths and 1 with 50 deaths.
# It's not possible to download reliably as a CSV so we'll just enter the values

other_years <- rbind(
  data_frame(
    killed = c(4, 5, 6,7,8),
    freq = c(25, 8, 1, 1, 1),
    year = 2014
  ), 
  data_frame(
    killed = c(4, 5, 6, 8, 9, 10, 16),
    freq = c(31, 7, 2, 2, 3, 1, 1),
    year = 2015
  ), 
  data_frame(
    killed = c(4,5,6,8,50),
    freq = c(23,13,3,1,1),
    year = 2016
  ),
  data_frame(
    killed = c(4,5,6,8,9,27,59),
    freq = c(33, 4, 2, 1, 1, 1, 1),
    year = 2017
  ),
  data_frame(
    killed = c(4, 5, 17),
    freq = c(11, 3, 1),
    year = 2018
  )
)



data <- list(
  x = rep(other_years$killed, other_years$freq),
  lower_limit = 4,
  mu_prior_mean = summary(fit1)$summary["mu", "mean"],
  mu_prior_sd = summary(fit1)$summary["mu", "sd"],
  phi_prior_mean = summary(fit1)$summary["phi", "mean"],
  phi_prior_sd = summary(fit1)$summary["phi", "sd"]
)

data$n <- length(data$x)

fit2 <- stan("0119-trunc-negbin.stan", data = data)

#---------------
phi <- summary(fit2)$summary["phi", "mean"]
mu <- summary(fit2)$summary["mu", "mean"]

x <- rnbinom(100000, size = phi, mu = mu)
ggplot(data.frame(x), aes(x = x)) +
  geom_histogram(binwidth = 1)


cbind(0:10, round(100 * dnbinom(0:10, size = phi, mu = mu), 1))


