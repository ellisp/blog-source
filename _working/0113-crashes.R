library(tidyverse)
library(mapproj)
library(proj4)
library(ggmap)
library(ggthemes) # for theme_map
library(stringr)
library(forcats)
library(viridis)
library(leaflet)
library(htmltools)
library(broom)
library(openxlsx)
library(forecastHybrid)
library(rstan)

#=================long term picture========================
longterm <- read.xlsx("../data/vkt2.1.xlsx", sheet = "safety (2)", 
                      cols = c(1, 17), rows = 2:69)


svg("../img/0113-longterm.svg", 8, 4)
ggplot(longterm, aes(x = Year, y = Deaths)) +
  geom_line() +
  ggtitle("Road deaths in New Zealand 1950 - 2016",
          "Not adjusted for population or vehicle kilometres travelled") +
  labs(caption = "Source: NZTA data compiled by Sam Warburton")
dev.off()


#============download data and tidy up a bit======================
# The vehicle kilometres travelled data on the NZTA website only goes back 10 years
# so I use Sam Warburton's archive at https://t.co/uthCzgVWvO 

# The individual crash data come from 
# https://www.nzta.govt.nz/safety/safety-resources/road-safety-information-and-tools/disaggregated-crash-data/


# caution, largish file - 27 MB
download.file("https://www.nzta.govt.nz/assets/Safety/docs/disaggregated-crash-data.zip",
              destfile = "disaggregated-crash-data.zip", mode = "wb")
unzip("disaggregated-crash-data.zip")

crash <- read.csv("disaggregated-crash-data.csv", check.names = FALSE)
dim(crash) # 586,189 rows

# I prefer lower case names, and underscores rather than dots:
names(crash) <- gsub(" ", "_", str_trim(tolower(names(crash))), fixed = TRUE)

# Convert the NZTM coordinates to latitude and longitude:
# see https://gis.stackexchange.com/questions/20389/converting-nzmg-or-nztm-to-latitude-longitude-for-use-with-r-map-library
p4str <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"
p <- proj4::project(crash[ , c("easting", "northing")], proj = p4str, inverse = TRUE)
crash$longitude <- p$x
crash$latitude <- p$y


#====================NZ maps========================
my_map_theme <- theme_map(base_family = "myfont") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = rel(1), face = "bold"),
        plot.caption = element_text(colour = "grey50")) 

m <- crash %>%
  as_tibble() %>%
  filter(fatal_count > 0) %>%
  ggplot(aes(x = longitude, y = latitude, size = fatal_count))  +
  borders("nz", colour = NA, fill = "darkgreen", alpha = 0.1) +
  geom_point(alpha = 0.2) +
  coord_map() +
  my_map_theme +
  theme(legend.position = c(0.9, 0.05)) +
  labs(size = "Fatalities",
       caption = "Source: NZTA Disaggregated Crash Data") +
  ggtitle("Fatal crashes in New Zealand, 2000 - 2017")

svg("../img/0113-all-nz.svg", 7, 7)
m
dev.off()

svg("../img/0113-map-over-time.svg", 10, 8)
m + facet_wrap(~crash_year)
dev.off()

svg("../img/0113-map-by-multi.svg", 10, 8)
m + facet_wrap(~multi_veh, ncol = 4)
dev.off()

svg("../img/0113-map-by-holiday.svg", 10, 8)
m + facet_wrap(~holiday)  
dev.off()


#==================Wellington maps===================
wtn <- get_map("Wellington, New Zealand", maptype = "roadmap", zoom = 11)

png("../img/0113-wellington.png", 7 * 300, 8 * 300, res = 300)
ggmap(wtn) +
  geom_point(aes(x = longitude, y = latitude), data = crash, alpha = 0.2) +
  my_map_theme +
  ggtitle("Crashes in Wellington, 2000 to 2017",
          "(Including non-fatal)")
dev.off()


#===================interactive map=================
crash %>%
  filter(fatal_count > 0) %>%
  mutate(fatal_word = ifelse(fatal_count == 1, " fatality", " fatalities"),
         label1 = paste0(fatal_count, fatal_word),
         label2 = tolower(paste0("<center><h4>", crash_year, "</h4><p>", fatal_count, fatal_word, "<br/>", 
                       intsn_midblock,  ", ", flat_hill, ", ", road_curvature, "</p></center>"))) %>%
  leaflet() %>%
  setView(175, -41, zoom = 9) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addCircleMarkers(radius = ~fatal_count * 3, 
                   label = ~label1,
                   popup = ~label2)



#==================time series=============

#------------------vehicle kilometres travelled--------------------------
vkt <- read.xlsx("../data/vkt2.1.xlsx", sheet = "VKT forecasts (2)",
                 cols = 3, rows = 6:62, colNames = FALSE) / 4

vkt_ts <- ts(vkt$X1, start = c(2001, 4), frequency = 4)
vkt_mod <- hybridModel(vkt_ts, models = c("ae"))
vkt_fc <- forecast(vkt_mod, h = 8)

svg("../img/0113-vkt.svg", 8, 4)
autoplot(vkt_fc) +
  ggtitle("Vehicle kilometres travelled per quarter in New Zealand, 2001 to 2017",
          "Forecasts from 2016 second quarter are an average of auto.arima and ets") +
  labs(x = "", y = "Billions of vehicle kilometres travelled",
       fill = "Prediction\nintervals",
       caption = "Source: NZTA data compiled by Sam Warburton;\nForecasts by Peter's Stats Stuff")
dev.off()

#==================combining vkt and crashes============


vkt_df <- data_frame(vkt = c(vkt[ ,1, drop = TRUE], vkt_fc$mean),
                     t = c(time(vkt_ts), time(vkt_fc$mean))) %>%
  mutate(six_month_period = as.character(ifelse(t %% 1 < 0.4, "a", "b")),
         year = as.integer(t %/% 1)) %>%
  group_by(year, six_month_period) %>%
  summarise(vkt = sum(vkt))


crash_six_month <- crash %>%
  as_tibble() %>%
  filter(fatal_count > 0) %>%
  mutate(fin_year_end = as.numeric(str_sub(crash_fin_year, start = -4)),
         six_month_period = ifelse(fin_year_end == crash_year, "a", "b")) %>%
  rename(year = crash_year) %>%
  group_by(year, six_month_period) %>%
  summarise(fatal_count = sum(fatal_count)) %>%
  left_join(vkt_df, by = c("year", "six_month_period")) %>%
  mutate(deaths_per_billion_vkt = fatal_count / vkt) %>%
  filter(year > 2001) %>%
  arrange(year, six_month_period)

dpbv_ts <- ts(crash_six_month$deaths_per_billion_vkt, frequency = 2, start = c(2002, 1))

# high autocorrelation, as we'd expect:
ggtsdisplay(dpbv_ts, 
            main = "Crash deaths per billion vehicle kilometres travelled, six monthly aggregation") 

auto.arima(dpbv_ts) # ARIMA(1,1,0)(0,0,1)[2] with drift

#===============bayesian model=====================
d <- list(
  n      = nrow(crash_six_month),
  deaths = crash_six_month$fatal_count,
  vkt    = crash_six_month$vkt
)


stan_mod <- stan(file = "0113-crashes.stan", data = d, 
                 control = list(adapt_delta = 0.99, max_treedepth = 15))


mu <- t(as.data.frame(extract(stan_mod, "mu")))


mu_gathered <- mu %>%
  as_tibble() %>%
  mutate(period = as.numeric(time(dpbv_ts))) %>%
  gather(run, mu, -period)

mu_summarised <- mu_gathered %>%
  group_by(period) %>%
  summarise(low80 = quantile(mu, 0.1),
            high80 = quantile(mu, 0.9),
            low95 = quantile(mu, 0.025),
            high95 = quantile(mu, 0.975),
            middle = mean(mu))

svg("../img/0113-model-results.svg", 8, 5)
mu_gathered %>%
  ggplot(aes(x = period)) +
  geom_line(alpha = 0.02, aes(group = run, y = mu)) +
  geom_ribbon(data = mu_summarised, aes(ymin = low80, ymax = high80), 
              fill = "steelblue", alpha = 0.5) +
  geom_line(data = mu_summarised, aes(y = middle), colour = "white", size = 3) +
  geom_point(data = crash_six_month, 
             aes(x = year + (six_month_period == "b") * 0.5, 
                 y = deaths_per_billion_vkt), 
             fill = "white", colour = "black", shape = 21, size = 4) +
  labs(x = "", y = "Latent rate of deaths per billion km",
       caption = "Source: data from NZTA and Sam Warburton;
Analysis by Peter's Stats Stuff") +
  ggtitle("State space model of crash deaths in New Zealand",
          "Six monthly observations")
dev.off()

mu_gathered %>%
  filter(period %in% c(2017.0, 2013.5)) %>%
  summarise(mean(mu[period == 2017.0] > mu[period == 2013.5])) # 88% of the time



#===============factors================
svg("../img/0113-factors.svg", 11, 9)
crash %>%
  as_tibble() %>%
  filter(fatal_count > 0) %>%
  mutate(fin_year_end = as.numeric(str_sub(crash_fin_year, start = -4))) %>%
  select(fin_year_end, post_or_pole:other) %>%
  mutate(id = 1:n()) %>%
  gather(cause, value, -fin_year_end, -id) %>%
  group_by(fin_year_end, cause) %>%
  summarise(prop_accidents = sum(value > 0) / length(unique(id))) %>%
  ungroup() %>%
  mutate(cause = gsub("_", " ", cause),
         cause = fct_reorder(cause, -prop_accidents)) %>%
  ggplot(aes(x = fin_year_end, y = prop_accidents)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~cause) +
  scale_y_continuous("Proportion of accidnets with this as a factor\n", label = percent) +
  ggtitle("Factors relating to fatal crashes 2000 - 2017",
          "Ordered from most commonly recorded to least") +
  labs(caption = "Source: NZTA disaggregated crash data",
       x = "Year ending June")
dev.off()



#=========================changing relationship between holidays and numbers==============

crash_counts <- crash %>%
  group_by(crash_fin_year, lg_region_desc, holiday) %>%
  summarise(fatal_count = sum(fatal_count)) %>%
  filter(lg_region_desc != "") %>%
  mutate(holiday = relevel(holiday, "None"))


mod1 <- glm(fatal_count ~ crash_fin_year + lg_region_desc + holiday, 
            family = "poisson", 
            data = crash_counts)

mod2 <- glm(fatal_count ~ crash_fin_year + lg_region_desc * holiday, 
            family = "poisson", data = crash_counts)

# marginal choice between these two models - there's a lot of extra degrees of freedom
# used up in mod2 for marginal improvement in deviance.  AIC suggests use the simpler 
# model; Chi square test says the interaction effect is "significant" and use the complex one.
anova(mod1, mod2, test = "Chi")
AIC(mod1, mod2)

# tidy up coefficients:
cfs <- tidy(mod2)

svg("../img/0113-glm-holidays.svg", 10, 8)
cfs %>%
  filter(grepl(":", term)) %>%
  extract(term, into = c("region", "holiday"), regex = "(.+):(.+)") %>%
  mutate(region = gsub("lg_region_desc", "", region),
         holiday = gsub("holiday", "", holiday)) %>%
  ggplot(aes(x = estimate, y = region, 
             label = ifelse(p.value < 0.05, as.character(region), ""))) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_segment(aes(yend = region, 
                   x = estimate - 2 * std.error, 
                   xend = estimate + 2 * std.error), colour = "grey20") +
  geom_text(size = 3.5, nudge_y = 0.34, colour = "steelblue") +
  ggtitle("Distinctive region - holiday interactions from a statistical model",
          "Higher values indicate the holiday has a higher relative accident rate in that particular region than in Auckland") +
  labs(y = "", caption = "Source: NZTA Disaggregated Crash Data",
       x = "Estimated interaction effect in a generalized linear model with Poisson response:
frequency ~ year + holiday * region") +
  facet_wrap(~holiday)
dev.off()

svg("../img/0113-props-holidays.svg", 10, 8)
crash %>%
  filter(lg_region_desc != "") %>%
  group_by(lg_region_desc, holiday) %>%
  summarise(fatal_count = sum(fatal_count)) %>%
  group_by(lg_region_desc) %>%
  mutate(share = fatal_count / sum(fatal_count)) %>%
  filter(holiday != "None") %>%
  ggplot(aes(x = share, y = lg_region_desc)) +
  facet_wrap(~holiday) +
  geom_point() +
  scale_x_continuous("\nProportion of all fatalities in region that occur in this holiday",
                     label = percent) +
  labs(y = "", caption = "Source: NZTA Disaggregated Crash Data") +
  ggtitle("Proportion of each regions fatalities on various holidays",
          "This simpler calculation is designed as a check on the modelled results in the previous chart")
dev.off()


#==========clean up===========
unlink("disaggregated-crash-data.zip")
unlink("disaggregated-crash-data.csv")
convert_pngs("0113")
