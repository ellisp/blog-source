library(tidyverse)
library(scales)
library(openxlsx)
library(directlabels)
library(forcats)
library(RColorBrewer)
library(stringr)
library(tweenr)

#========download and prepare data=========
download.file("https://github.com/ellisp/ellisp.github.io/raw/master/data/plague-mortality.xlsx",
              destfile = "plauge-mortality.xlsx", mode = "wb")
orig <- read.xlsx("plague-mortality.xlsx", sheet = "data")


d1 <- orig %>%
  rename(place = Place, year = Date, agent = Agent, country = Country, latitude = Latitude) %>%
  gather(month, death_rate, -place, -year, -agent, -country, -latitude) %>%
  # put the months' levels in correct order so they work in charts etc
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
         month_number = as.numeric(month)) %>%
  mutate(place_date = paste(place, year),
         agent = fct_reorder(agent, year)) %>%
  arrange(place, year, month_number) 

# define a caption for multiple use:
the_caption <- str_wrap("Graphic by Peter Ellis, reworking data and figures published by Mark R. Welford and Brian H. Bossak 'Validation of Inverse Seasonal Peak Mortality in Medieval Plagues, Including the Black Death, in Comparison to Modern Yersinia pestis-Variant Diseases'", 
                        100)


#============faceted version of original graphic=====================

  

p1 <- d1 %>%
  ggplot(aes(x = month_number, y = death_rate, colour = place_date)) +
  geom_smooth(aes(group = NULL), se = TRUE, colour = NA, size = 4) +
  geom_line(aes(x = month_number), size = 0.9) +
  scale_y_log10("Number of deaths in a month", limits = c(1, 1000000), label = comma) +
  scale_x_continuous("", breaks = 1:12, labels = levels(d1$month), limits = c(0.5, 12)) +
  facet_wrap(~agent) +
  theme(panel.grid = element_blank()) +
  labs(caption = the_caption) +
  ggtitle("Seasonal deaths by different plague agents",
          "Medieval and early modern Black Death and plague peaked in summer, whereas early 20th Century outbreaks peaked in late winter")


svg("../img/0114-log10.svg", 10, 6)
direct.label(p1, list("top.bumpup", fontfamily = "myfont", cex = 0.8)) # good
dev.off()


# version with free y axes as an alternative to the log transform
p1b <- p1 +
  facet_wrap(~agent, scales = "free_y") +
  scale_y_continuous()

svg("../img/0114-free_y.svg", 10, 6)
direct.label(p1b, list("top.bumpup", fontfamily = "myfont", cex = 0.8))
dev.off()

#==============================y axis as proportion===================
d2 <- d1 %>%
  group_by(country, year, month_number, agent) %>%
  summarise(deaths = sum(death_rate, na.rm = TRUE)) %>%
  mutate(country_date = paste(country, year)) %>%
  group_by(country_date) %>% 
  mutate(prop_deaths = deaths / sum(deaths, na.rm = TRUE)) %>%
  # convert the zeroes back to NA to be consistent with original presentation:
  mutate(prop_deaths = ifelse(prop_deaths == 0, NA, prop_deaths))

# Defining a palette.  This is a little complex because although colour
# is going to be mapped to country_year, I want each country to have its own
# colour regardless of the year.  I'm going to use Set1 of Cynthia Brewer's colours,
# except for the yellow which is invisible against a white background
pal1 <- data_frame(country = unique(d2$country))
pal1$colour <-  brewer.pal(nrow(pal1) + 1, "Set1")[-6]

pal2 <- distinct(d2, country, country_date) %>%
  left_join(pal1, by = "country")
pal3 <- pal2$colour
names(pal3) <- pal2$country_date

# draw graphic with colour mapped to country-year combination, but colours come out
# at country level:
p2 <- d2 %>%
  ggplot(aes(x = month_number, y = prop_deaths, colour = country_date)) +
  geom_smooth(aes(group = NULL), se = TRUE, colour = NA, size = 4) +
  # geom_point() +
  geom_line(aes(x = month_number)) +
  scale_color_manual(values = pal3) +
  scale_y_continuous("Proportion of the year's total deaths in each month\n", 
                     label = percent, limits = c(0, 0.65)) +
  scale_x_continuous("", breaks = 1:12, labels = levels(d1$month), limits = c(0.5, 12)) +
  facet_wrap(~agent) +
  theme(panel.grid = element_blank()) +
  ggtitle("Seasonal deaths by different plague agents",
          "Medieval and early modern Black Death and plague peaked in summer, whereas early 20th Century outbreaks peaked in late winter") +
  labs(caption = the_caption)

svg("../img/0114-proportions.svg", 10, 6)
direct.label(p2, list("top.bumpup", fontfamily = "myfont", cex = 0.8))
dev.off()

#============animation===================

d3 <- d1 %>%
  group_by(place_date, country, latitude) %>%
  mutate(prop_deaths = death_rate / sum(death_rate, na.rm = TRUE),
         prop_deaths = ifelse(is.na(prop_deaths), 0, prop_deaths)) %>%
  ungroup() %>%
  mutate(place_date = fct_reorder(place_date, year),
         place_date_id = as.numeric(place_date))  %>%
  arrange(place_date_id, month_number)

place_dates <- levels(d3$place_date)

# change this to a list of data.frames, one for each state
d3_l <- lapply(place_dates, function(x){
  d3 %>%
    filter(place_date == x) %>%
    select(prop_deaths, month, place_date, agent)
  })

# use tweenr to create a single data frame combining those 20+ frames in the list, with interpolated
# values for smooth animation:
d3_t <- tween_states(d3_l, tweenlength = 5, statelength = 8, ease = "cubic-in-out", nframes = 600) %>%
  # caution - tween_states loses the information ont the ordering of factors
  mutate(month= factor(as.character(month), levels = levels(d1$month)),
         place_date = factor(as.character(place_date, levels = place_dates)))

showtext.opts(dpi = 100)

# make a temporary folder to store some thousands of PNG files as individual frames of the animation
unlink("0114-tmp-small", recursive = TRUE)
dir.create("0114-tmp-small")

for(i in 1:max(d3_t$.frame)){
  png(paste0("0114-tmp-small/", i + 10000, ".png"), 1000, 500, res = 100)  
  the_data <- filter(d3_t, .frame == i)
  the_title <- paste("Deaths from", the_data[1, "agent"], "in", the_data[1, "place_date"])
  
  tmp <- d3 %>% 
    filter(place_date == the_data[1, "place_date"]) %>%
    select(place_date, country, latitude) %>%
    distinct()
  the_xlab <- with(tmp, paste0(place_date, ", ", country, ", ", latitude, " degrees North"))
  
  the_high_point <- the_data %>%
    filter(prop_deaths == max(prop_deaths) ) %>%
    slice(1) %>%
    mutate(prop_deaths = prop_deaths + 0.03) %>%
    mutate(lab = agent)
  
  the_colour <- pal2[pal2$country == tmp$country, ]$colour[1]
  
  print(
    ggplot(the_data,  aes(x = as.numeric(month), y = prop_deaths)) +
    geom_ribbon(aes(ymax = prop_deaths), ymin = 0, fill = the_colour, colour = "grey50", alpha = 0.1) +
    scale_x_continuous(the_xlab, breaks = 1:12, labels = levels(d1$month), limits = c(0.5, 12)) +
    ggtitle(the_title, "Distribution by month of the total deaths that year") +
    theme(panel.grid = element_blank()) +
    geom_text(data = the_high_point, aes(label = lab))  +
    labs(caption = the_caption) +
    scale_y_continuous("Proportion of the year's total deaths in each month\n", 
                       label = percent, limits = c(0, 0.68))
  )
  dev.off()
  
  # counter:
  if (i / 20 == round(i / 20)){cat(i)}
}

# combine all those frames into a single animated gif
pd <- setwd("0114-tmp-small")
system('magick -loop 0 -delay 8 *.png "0114-plague-deaths-small.gif"')
setwd(pd)

file.copy("0114-tmp-small/0114-plague-deaths-small.gif", "../img/0114-plague-deaths-small.gif", overwrite = TRUE)

convert_pngs("0114")
unlink("plague-mortality.xlsx")