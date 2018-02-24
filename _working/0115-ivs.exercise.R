

library(tidyverse)
library(data.table)
library(scales)
library(stringr)
library(forcats)
library(ggseas)
library(directlabels)
library(testthat)

#========Download and import data===============
# download survey data from the MBIE (Ministry of Business, Innovation and Employment) website
download.file("http://www.mbie.govt.nz/info-services/sectors-industries/tourism/tourism-research-data/ivs/documents-image-library/vw_IVS.zip",
              mode = "wb", destfile = "vw_IVS.zip")

unzip("vw_IVS.zip")

# list of CSV files, each one of which is a copy of a view in the MBIE database:
files <- list.files("IVS")

# import each CSV file as a data.table/data.frame object in RAM with the name of the view
for(i in 1:length(files)){
  print(paste("Importing", files[i]))
  tmp <- fread(paste0("IVS/", files[i]))
  # knock of the ".csv" at the end of each file name, for the name of the data frane in R:
  dfname <- str_sub(files[i], end = -5)
  assign(dfname, tmp)
}


#=============Familiarisation============
# First analysis is just to get a feel for the data; not used in the final report
head(vw_IVSSurveyMainHeader)

# total spend by purpose of visit
# Note that "WeightedSpend" just means outlier-treated; you need to
# use PopulationWeight to get the actual survey weights
spend_pov <- vw_IVSSurveyMainHeader %>%
  group_by(Qtr, POV) %>%
  summarise(total_spend = sum(WeightedSpend * PopulationWeight),
            visitors = sum(PopulationWeight),
            sample_size = n())

# graphic  
svg("../img/0115-total-spend.svg", 8, 6)
spend_pov %>%
  # convert the "1997 1" format into a number:
  mutate(qtr = as.numeric(str_sub(Qtr, start = -1)),
         yr = as.numeric(str_sub(Qtr, end = 4)),
         yr_qtr = yr + (qtr - 0.5) / 4) %>%
  # reshape:
  ungroup() %>%
  select(yr_qtr, POV, total_spend, visitors) %>%
  gather(variable, value, -yr_qtr, -POV) %>%
  mutate(POV = fct_reorder(POV, -value)) %>%
  ggplot(aes(x = yr_qtr, y = value, colour = POV)) +
  facet_wrap(~variable, scale = "free_y") +
  geom_line(alpha = 0.3) +
  stat_seas(start = c(1997, 1), size = 1.2) +
  scale_y_continuous("", label = comma) +
  ggtitle("Weighted spend and visitor numbers, visits to New Zealand",
          "Highly seasonal quarterly data") +
  labs(x = "", colour = "Purpose\nof Visit",
       caption = "Source: MBIE International Visitor Survey")
dev.off()

#================Activities=================
head(vw_IVSActivities)
# hmm, an interesting set of "Activities" but we have no way of identifying which are "good" or not
# so we will just count them per person/visit

# number of activities per person
act_pp <- vw_IVSActivities %>%
  group_by(SurveyResponseID) %>%
  summarise(number_activities = n())


#================Queenstown=====================
head(vw_IVSItineraryPlaces)
# 514 distinct places listed:
sort(unique(vw_IVSItineraryPlaces$WhereStayed))

places_visited <- vw_IVSItineraryPlaces %>%
  group_by(SurveyResponseID) %>%
  summarise(number_places_visited = length(unique(WhereStayed)),
            visited_queenstown = ifelse("Queenstown" %in% unique(WhereStayed), 
                                        "Visited Queenstown", "Did not visit Queenstown"))

qt_visitors <- places_visited %>%
  right_join(vw_IVSSurveyMainHeader, by = "SurveyResponseID") %>%
  mutate(qtr = as.numeric(str_sub(Qtr, start = -1)),
         yr = as.numeric(str_sub(Qtr, end = 4)),
         yr_qtr = yr + (qtr - 0.5) / 4,
         visited_queenstown = ifelse(is.na(visited_queenstown), "Did not visit *anywhere*", visited_queenstown)) %>%
  left_join(act_pp, by = "SurveyResponseID") %>%
  group_by(visited_queenstown, yr_qtr)

expect_equal(nrow(qt_visitors), nrow(vw_IVSSurveyMainHeader))

# Two variables look to be about length of stay; but NoDaysInNZ has 120,000+ NA values so we will use the other
# option which has only 196
summary(vw_IVSSurveyMainHeader[ , c("NoDaysInNZ", "LengthOfStay")])

qt_visitors_sum <- qt_visitors %>%
  summarise(total_spend = sum(WeightedSpend * PopulationWeight),
            total_days = sum(LengthOfStay * PopulationWeight, na.rm = TRUE),
            total_activities = sum(number_activities * PopulationWeight, na.rm = TRUE),
            visitors = sum(PopulationWeight),
            spend_per_visitor = total_spend / visitors,
            days_per_visitor = total_days / visitors,
            activities_per_visitor = total_activities / visitors,
            sample_size = n()) %>%
  ungroup() %>%
  mutate(visited_queenstown = fct_reorder(visited_queenstown, total_spend))
  

palette <- c("Visited Queenstown" = "red", "Did not visit Queenstown" = "blue", "Did not visit *anywhere*" = "grey")

# do they spend more per visit?

svg("../img/0115-qt-spend.svg", 8, 4.5)
qt_visitors_sum %>%
  ggplot(aes(x = yr_qtr, y = spend_per_visitor, colour = visited_queenstown)) +
  geom_line(alpha = 0.15) +
  stat_stl(s.window = 7, size = 1.2) +
  theme(legend.position = "right") +
  scale_colour_manual("", values = palette) +
  scale_y_continuous("Spend per visitor\n(seasonally adjusted)", label = dollar) +
  labs(x =  "", caption = "Source: MBIE International Visitor Survey") +
  ggtitle("Total spend while in New Zealand", 
          "Comparing visitors who went to Queenstown with those who did not")
dev.off()

svg("../img/0115-qt-activities.svg", 8, 4.5)
# do they stay more days and do more activities?
qt_visitors_sum %>%
  select(visited_queenstown, yr_qtr, days_per_visitor, activities_per_visitor) %>%
  gather(variable, value, -visited_queenstown, -yr_qtr) %>%
  ggplot(aes(x = yr_qtr, y = value, colour = visited_queenstown)) +
  facet_wrap(~variable, scales = "free_y") +
  geom_line(alpha = 0.15) +
  stat_stl(s.window = 7, size = 1.2) +
  scale_colour_manual("", values = palette) +
  scale_y_continuous("Value\n(seasonally adjusted)", label = comma) +
  labs(x =  "", caption = "Source: MBIE International Visitor Survey") +
  ggtitle("Activities and length of stay while in New Zealand", 
          "Comparing visitors who went to Queenstown with those who did not")
dev.off()
# There is a problem here that if you choose *any* destination, people who visited there
# do more activites and stayed longer in NZ than average.  Why?  Probably because indicating
# *any* itinerary point means they were more engaged (either with NZ, or with the country...)



#===============detective work into how all locations seem to have higher spenders on average=========================

# each distinct place visited at least once by each person:
ip <- vw_IVSItineraryPlaces %>%
  select(SurveyResponseID, WhereStayed) %>%
  distinct()

# total and average spend for each location, 2014 onwards:
all_locs <- vw_IVSSurveyMainHeader %>%
  filter(Year >= 2014) %>% 
  left_join(ip, by = "SurveyResponseID") %>%
  mutate(WhereStayed = ifelse(is.na(WhereStayed), "Did not stay overnight at any location", WhereStayed)) %>%
  group_by(WhereStayed) %>%
  summarise(total_spend = sum(WeightedSpend * PopulationWeight, na.rm = TRUE),
            visitors = sum(PopulationWeight, na.rm = TRUE),
            mean_spend = total_spend / visitors) %>%
  filter(visitors > 0) %>%
  arrange(desc(mean_spend))

# overall average for comparison
overall_mean <- vw_IVSSurveyMainHeader %>%
  filter(Year >= 2014) %>%
  summarise(mean_spend = sum(WeightedSpend * PopulationWeight) / sum(PopulationWeight))

# graphic of just the 150 biggest
svg("../img/0115-all-locations.svg", 8, 10)
all_locs %>%
  filter(rank(-visitors) <= 150) %>%
  mutate(label = ifelse(rank(-visitors) < 20, WhereStayed, "")) %>%
  mutate(WhereStayed = fct_reorder(WhereStayed, mean_spend)) %>% 
  ggplot(aes(x = mean_spend, y = WhereStayed)) +
  geom_vline(xintercept = overall_mean$mean_spend, colour = "steelblue") +
  geom_point(aes(size = visitors)) +
  geom_text(aes(label = label), hjust = 0, nudge_x = 100, size = 2) +
  scale_x_continuous("Mean spend per visit from 2014 to 2017", label = dollar) +
  scale_size_area("Number of visitors\nsince 2014", label = comma) +
  labs(caption = "Source: MBIE International Visitor Survey") +
  ggtitle("Mean spend of visitors to the 150 most visited locations",
          "Only the top 20 locations are labelled") +
  theme(axis.text.y = element_blank(),
        legend.position = "right")
dev.off()

#===============compare spend to number of places people visited=======
png("../img/0115-individuals.png", 8*600, 6*600, res = 600)
vw_IVSSurveyMainHeader %>%
  filter(Year >= 2014) %>%
  left_join(places_visited, by = "SurveyResponseID") %>%
  mutate(number_places_overnighted = ifelse(is.na(number_places_visited), 0, number_places_visited)) %>%
  ggplot(aes(x = number_places_overnighted, y = WeightedSpend, size = PopulationWeight)) +
  geom_point(alpha = 0.1) +
  geom_hline(yintercept = overall_mean$mean_spend, colour = "steelblue") +
  geom_smooth(colour = "orange") +
  annotate("text", x = 70, y = overall_mean$mean_spend - 1000, label = "Overall mean", colour = "steelblue") +
  scale_y_sqrt("Spend in New Zealand\n(square root transformed scale)",
               label = dollar, breaks = c(1000, 10000, 25000, 50000, 100000, 200000)) +
  labs(x = "Number of places overnighted in\n",
       caption = "Source: MBIE International Visitor Survey") +
  theme(legend.position = "none") +
  ggtitle("Individuals' spend in New Zealand compared to places visited",
          "Data from 2014 to 2017")
dev.off()

svg("../img/0115-grouped.svg", 8, 6)
vw_IVSSurveyMainHeader %>%
  filter(Year >= 2014) %>%
  left_join(places_visited, by = "SurveyResponseID") %>%
  mutate(number_places_overnighted = ifelse(is.na(number_places_visited), 0, number_places_visited)) %>%
  group_by(number_places_overnighted) %>%
  summarise(mean_spend = sum(WeightedSpend * PopulationWeight) / sum(PopulationWeight),
            visitors = sum(PopulationWeight)) %>%
  arrange(number_places_overnighted) %>%
  ggplot(aes(x = number_places_overnighted, y = mean_spend, size = visitors)) +
  geom_hline(yintercept = overall_mean$mean_spend, colour = "steelblue") +
  geom_point() +
  annotate("text", x = 70, y = overall_mean$mean_spend - 500, label = "Overall mean", colour = "steelblue") +
  scale_y_continuous("Mean spend in New Zealand",
               label = dollar) +
  labs(x = "Number of places overnighted in\n",
       caption = "Source: MBIE International Visitor Survey") +
  theme(legend.position = "none") +
  ggtitle("Spend in New Zealand compared to places visited",
          "Data from 2014 to 2017")
dev.off()    

convert_pngs("0115")  
