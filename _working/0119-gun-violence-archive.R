
library(tidyverse)
library(scales)
library(readr)
library(lubridate)
library(ggrepel)
library(viridis)

# to download these files you need to go to http://www.gunviolencearchive.org/query/
# and do a query, then choose download to CSV.  It only downloads 500 at a time.
# "Save" actually means "Execute the query".  Then you need to explicitly download CSV.
# And then you need to pick "download" (even though you're already in the "download CSV"
# environment and it has downloaded it = "download" here means "yes, really download it,
# and save it to my local drive")

files <- list.files("../data/gun-violence", full.names = TRUE)

gva_l <- lapply(files, read_csv)
gva <- do.call("rbind", gva_l) %>%
  mutate(date = as.Date(`Incident Date`, format = "%B %d, %Y"),
         year = year(date),
         month = month(date)) %>%
  # filter out zero deaths incidents, which are meant to not be in the original query but there
  # are one or two per year for some reason (probably data is edited from 1 to 0, and a flag
  # doesn't get change)
  filter(`# Killed` > 0) %>%
  # remove duplicates in case I downloaded some data twice or there is a database error:
  distinct()

dim(gva)

gva %>%
  ggplot(aes(x = `# Killed`)) +
  geom_histogram(binwidth = 1) 

gva %>%
  filter(year >= 2014) %>%
  ggplot(aes(x = date, y = `# Killed`)) +
  geom_jitter(alpha = 0.1) +
  ggtitle("Fatal firearm incidents in the USA; 500 sampled from selected months",
          "Points are jittered to give a sense of how much concentration there is in '1 death per incident'") +
  labs(x = "", y = "Number killed in each incident",
       caption = "Searches from Gun Violence Archive")

gva %>%
  group_by(year, month) %>%
  summarise(ave = mean(`# Killed`),
            n = n(),
            days = length(unique(date)))

# Only 2014 onwards is complete data


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
          
