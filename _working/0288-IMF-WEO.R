library(readxl)
library(tidyverse)
library(rsdmx)
library(patchwork)

# newsarticles like this compare the forecasts for growth made in April to those
# made in June:
# https://www.bbc.com/news/articles/czx415erwkwo
# eg US 1.8% v 2.7%; Canada 1.4% v 2.0%; France 0.6% v 0.8%
# Q1 - Are these current or constant prices or PPP; and per capita or not?
# Q2 - how is the Pacific going


#----------------download sdmx version-------------------------
options(timeout=600)

dluz <- function(url, destfile){
  if(!file.exists(destfile)){
    download.file(url, destfile = destfile, mode = "wb")
    unzip(destfile)
  }
}

# to see the outlooks where the whole database is available, all countries, see
# https://www.imf.org/en/Publications/SPROLLS/world-economic-outlook-databases#sort=%40imfdate%20descending

dluz("https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2025/april/WEOAPR2025-SDMXData.ashx",
              destfile = "weo2025-apr.zip")


dluz("https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2024/October/WEOOCT2024-SDMXData.ashx",
              destfile = "weo2024-oct.zip")

#---------------processing---------------

d2025 <- readSDMX("WEOAPR2025/xmlfile_APR2025.xml", isURL = FALSE) |> 
  # this parsing takes a long time:
  as_tibble()

d2024 <- readSDMX("WEOOCT2024/WEO_PUB_OCT2024.xml", isURL = FALSE) |> 
  # this parsing takes a long time:
  as_tibble()

count(d2025, SCALE)    # 1, 1000000 and 1000000000
count(d2025, CONCEPT)  # BCA, BCA_NGDP, BF, BFD, etc
count(d2025, UNIT)     # B to U
count(d2024, UNIT)     # B to U
count(d2025, REF_AREA)     # 001, 110, etc
count(d2025, FREQ)                # everying is A (annual)
count(d2024, FREQ)            # same
count(d2025, LASTACTUALDATE)      # varies, from 2003 and higher

concept <- read_xlsx("WEOAPR2025/WEOPUB_DSD_APR2025.xlsx", sheet = "CONCEPT", skip = 7)[, 1:2] |> 
  rename(CONCEPT = Code,
         concept = Description)

unit <- read_xlsx("WEOAPR2025/WEOPUB_DSD_APR2025.xlsx", sheet = "UNITS", skip = 8, 
                  col_names = c("UNIT", "unit"))

ref_areas <-   read_xlsx("WEOAPR2025/WEOPUB_DSD_APR2025.xlsx", sheet = "REF_AREA", skip = 7)[, 1:2] |> 
  rename(REF_AREA = Code,
         country = Description)

weo2025 <- d2025 |> 
  left_join(concept, by = "CONCEPT") |> 
  left_join(unit, by = "UNIT") |> 
  left_join(ref_areas, by = "REF_AREA") |> 
  mutate(year = as.numeric(TIME_PERIOD),
         value = as.numeric(OBS_VALUE)) |> 
  select(concept:value, everything()) |> 
  mutate(type = if_else(year > as.numeric(LASTACTUALDATE), "Actual", "Forecast"),
         type = fct_relevel(type, "Forecast"),
         edition = "WEO April 2025")

filter(weo2025, is.na(value)) |> count(OBS_VALUE)
# just -- and n/a, so OK


weo2024 <- d2024 |> 
  left_join(concept, by = "CONCEPT") |> 
  left_join(unit, by = "UNIT") |> 
  left_join(ref_areas, by = "REF_AREA") |> 
  mutate(year = as.numeric(TIME_PERIOD),
         value = as.numeric(OBS_VALUE)) |> 
  select(concept:value, everything()) |> 
  mutate(type = if_else(year > as.numeric(LASTACTUALDATE), "Actual", "Forecast"),
         type = fct_relevel(type, "Forecast"),
         edition = "WEO October 2024")

weo_both <- rbind(weo2024, weo2025)



regions <- c(
  "World",
  "Advanced Economies",
  "G7",
  "Other Advanced Economies (Advanced Economies excluding G7 and Euro Area countries)",
  "Euro area",
  "Emerging Market and Developing Economies",
  "Latin America and the Caribbean",
  "Middle East and Central Asia (MECA)",
  "Emerging and Developing Asia",
  "ASEAN-5",
  "Sub-Sahara Africa",
  "Emerging and Developing Europe",
  "European Union"
)

pacific <- c(
  "Solomon Islands",
  "Fiji",
  "Kiribati",
  "Nauru",
  "Vanuatu",
  "Papua New Guinea",
  "Samoa",
  "Tonga",
  "Marshall Islands",
  "Micronesia",
  "Tuvalu",
  "Palau"
)

stopifnot(all(pacific %in% ref_areas$country))
stopifnot(all(regions %in% ref_areas$country))




#----------------charts----------------------------

weo2025 |> 
  filter(CONCEPT == "NGDPRPPPPC") |> 
  ggplot(aes(x = year, y = value, colour = country, linetype = type)) +
  geom_line() +
  theme(legend.position = "none") +
  labs(title = "Gross domestic product per capita, constant prices",
       subtitle = "Purchasing power parity; 2021 international dollar")



weo2025 |> 
  filter(CONCEPT == "NGDPRPPPPC") |> 
  filter(country %in% pacific) |> 
  mutate(country = fct_reorder(country, value, .na_rm = TRUE)) |> 
  ggplot(aes(x = year, y = value, colour = country, linetype = type)) +
  geom_line() +
  facet_wrap(~country) +
  theme(legend.position = "none") +
  labs(title = "Gross domestic product per capita in the Pacific",
       subtitle = "Purchasing power parity, constant prices; 2021 international dollar") +
  scale_y_continuous(label = dollar)
# interesting here that many of the countries did not have the big covid-related
# dip in GDP that Fiji did (or at least, it doesn't show up in their stats)


weo_both |> 
  filter(CONCEPT == "NGDPRPPPPC") |> 
  filter(country %in% pacific) |> 
  mutate(country = fct_reorder(country, value, .na_rm = TRUE)) |> 
  ggplot(aes(x = year, y = value, colour = country, linetype = type)) +
  geom_line() +
  facet_grid(edition~country) +
  theme(legend.position = "none") +
  labs(title = "Gross domestic product per capita in the Pacific",
       subtitle = "Purchasing power parity, constant prices; 2021 international dollars") +
  scale_y_continuous(label = dollar)
# interesting here that many of the countries did not have the big covid-related
# dip in GDP that Fiji did (or at least, it doesn't show up in their stats)


pac_revisions <- weo_both |> 
  filter(CONCEPT == "NGDPRPPPPC") |> 
  filter(country %in% pacific) |> 
  select(country, year, edition, value) |> 
  spread(edition, value) |> 
  mutate(ratio = `WEO April 2025` / `WEO October 2024`) |> 
  mutate(country = fct_reorder(country, ratio, .fun = last, .na_rm = TRUE)) 

pac1 <- pac_revisions |> 
  ggplot(aes(x = year, y = ratio)) +
  facet_wrap(~country) + 
  geom_line(size = 1.2, colour = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Revisions in economic expectations in the Pacific",
       subtitle = "GDP per capita, PPP constant prices, 2021 international dollars",
       x = "",
       y = "Revision atio")

pac2 <- weo_both |> 
  filter(CONCEPT == "LP") |> 
  filter(country %in% pacific) |> 
  select(country, year, edition, value) |> 
  spread(edition, value) |> 
  mutate(ratio = `WEO April 2025` / `WEO October 2024`) |> 
  mutate(country = factor(country, levels = levels(pac_revisions$country))) |> 
  ggplot(aes(x = year, y = ratio)) +
  facet_wrap(~country) + 
  geom_line(size = 1.2, colour = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = " ",
       subtitle = "Population",
       x = "",
       y = "Revision ratio")


pac3 <- weo_both |> 
  filter(CONCEPT == "NGDP" & unit == "National currency") |> 
  filter(country %in% pacific) |> 
  select(country, year, edition, value) |> 
  spread(edition, value) |> 
  mutate(ratio = `WEO April 2025` / `WEO October 2024`) |> 
  mutate(country = factor(country, levels = levels(pac_revisions$country))) |> 
  ggplot(aes(x = year, y = ratio)) +
  facet_wrap(~country) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_line(size = 1.2, colour = "steelblue") +
  labs(title = "  ",
       subtitle = "GDP in local currency, current prices",
       x = "",
       y = "Revision ratio")

pac5 <- weo_both |> 
  filter(CONCEPT == "NGDPD" & unit == "U.S. dollars") |> 
  filter(country %in% pacific) |> 
  select(country, year, edition, value) |> 
  spread(edition, value) |> 
  mutate(ratio = `WEO April 2025` / `WEO October 2024`) |> 
  mutate(country = factor(country, levels = levels(pac_revisions$country))) |> 
  ggplot(aes(x = year, y = ratio)) +
  facet_wrap(~country) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_line(size = 1.2, colour = "steelblue") +
  labs(title = "  ",
       subtitle = "GDP in US dollars, current prices",
       x = "",
       y = "Revision ratio")


pac4 <- weo_both |> 
  filter(CONCEPT == "PPPEX") |> 
  filter(country %in% pacific) |> 
  select(country, year, edition, value) |> 
  spread(edition, value) |> 
  mutate(ratio = `WEO April 2025` / `WEO October 2024`) |> 
  mutate(country = factor(country, levels = levels(pac_revisions$country))) |> 
  ggplot(aes(x = year, y = ratio)) +
  facet_wrap(~country) + 
  geom_line(size = 1.2, colour = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "  ",
       subtitle = "Implied purchasing power parity conversion rate",
       x = "",
       y = "Revision ratio",
       caption = "Ratio of IMF estimates in April 2025 to those October 2024 (higher than 1.0 means the estimate was revised upwards)")

p7 <- pac1 + pac2 + pac3 + pac4 + plot_layout(ncol = 2)
svg_png(p7, "../img/0288-pac-ratios", w = 16, h = 9.5)


weo2025 |> 
  filter(CONCEPT == "NGDP") |> 
  filter(!country %in% regions) |> 
  ggplot(aes(x = year, y = value, colour = country, linetype = type)) +
  geom_line() +
  theme(legend.position = "none") +
  labs(title = "Gross domestic product, current prices",
       subtitle = "local currency")

# export price of manufacturers is the only thing as % change:
weo2025 |> 
  filter(unit == "U.S. dollars; annual percent change") |> 
  count(concept)

# lots of things in USD:
# weo2025 |> 
#   filter(unit == "U.S. dollars") |> 
#   count(concept, CONCEPT) |> 
#   View()



ctxt <- "Gross domestic product per capita, current prices"
utxt <- "U.S. dollars"

weo2025 |> 
  filter(concept == ctxt) |> 
  filter(unit == utxt) |> 
  filter(!country %in% regions) |> 
  ggplot(aes(x = year, y = value, colour = country, linetype = type)) +
  geom_line() +
  theme(legend.position = "none") +
  labs(title = ctxt,
       subtitle = utxt)

#-------------headline growth rates------------

# from trial and error on which series ("CONCEPT") to use, we see the figures
# used for headline is real GDP growth so constant prices, but not per capita
growth_comps <- weo_both |> 
  filter(CONCEPT == "NGDP_R") |> 
  filter(year %in% 2024:2025) |> 
  group_by(country, edition) |> 
  summarise(growth = value[year == 2025] / value[year == 2024] - 1) |> 
  ungroup()

selected <- c(
  "United States",
  "Canada",
  "Japan",
  "United Kingdom",
  "Germany",
  "Italy",
  "France",
  "China",
  "India",
  "Russia",
  "Brazil"
)

bgcol <- "grey90"

p0 <- growth_comps |> 
  filter(country %in% selected) |> 
  mutate(country = factor(country, levels = selected[length(selected):1])) |> 
  mutate(text_col = ifelse(growth < 0, "negative", "positive")) |> 
  ggplot(aes(x = growth, y = country, fill = edition)) +
  geom_col(position = "dodge", width = 0.75, colour = bgcol) +
  geom_text(aes(label = percent(growth, accuracy = 0.1), colour = text_col), 
            position = position_dodge(width = 0.7), hjust = 1, vjust = 0.5,
            fontface = "bold", size = 3) +
  scale_fill_manual(values = c("darkblue", "steelblue"), guide = guide_legend(reverse = TRUE)) +
  scale_colour_manual(values = c("positive" = "white", "negative" = "black")) +
  scale_x_continuous(label = percent) +
  geom_vline(xintercept = 0) +
  guides(colour = "none") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = bgcol, colour = NA),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.8, 0.8)) +
  labs(fill = "",
       x = "",
       y = "",
       title = "Projected real GDP growth in 2025 for selected nations",
       subtitle = "Forecasts for 2025, made in October 2024 and April 2025 in the IMF World Economic Outlook.
The update for some, but not all, countries in January 2025 is not considered here.",
       caption = "Source: IMF World Economic Outlooks. Growth rates based on constant prices, but are not per capita.")



p1 <- growth_comps |> 
  filter(country %in% pacific) |> 
  mutate(country = fct_reorder(country, -growth, .fun = mean)) |> 
  mutate(text_col = ifelse(growth < 0, "negative", "positive")) |> 
  ggplot(aes(x = growth, y = country, fill = edition)) +
  geom_col(position = "dodge", width = 0.75, colour = bgcol) +
  geom_text(aes(label = percent(growth, accuracy = 0.1), colour = text_col), 
            position = position_dodge(width = 0.7), hjust = 1, vjust = 0.4,
            fontface = "bold", size = 3) +
  scale_fill_manual(values = c("darkblue", "steelblue"), guide = guide_legend(reverse = TRUE)) +
  scale_colour_manual(values = c("positive" = "white", "negative" = "black")) +
  scale_x_continuous(label = percent) +
  geom_vline(xintercept = 0) +
  guides(colour = "none") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = bgcol, colour = NA),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.8, 0.8)) +
  labs(fill = "",
       x = "",
       y = "",
       title = "Projected real GDP growth in 2025 for Pacific IMF members",
       subtitle = "Forecasts for 2025, made in October 2024 and April 2025 in the IMF World Economic Outlook.",
       caption = "Source: IMF World Economic Outlooks. Growth rates based on constant prices, but are not per capita.")

svg_png(p0, "../img/00288-selected-growth", w = 8, h = 7)

svg_png(p1, "../img/00288-pict-growth", w = 8, h = 7)


gcw <- growth_comps |> 
  spread(edition, growth) |> 
  mutate(is_pict = ifelse(country %in% pacific, "Pacific", "Other"))
  

p2 <- gcw |> 
  ggplot(aes(x = `WEO October 2024`, y = `WEO April 2025`, colour = is_pict)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey75") +
  geom_point() +
  geom_text_repel(data = filter(gcw,
                                `WEO April 2025` > 0.10 |
                                 `WEO October 2024` < 0 |
                                  `WEO October 2024` > 0.08 | 
                                  is_pict == "Pacific"),
                  aes(label = country),
                  seed = 123) +
  theme(legend.position = "none") +
  coord_equal() +
  scale_x_continuous(label = percent) +
  scale_y_continuous(label = percent) +
  scale_colour_manual(values = c("Pacific" = "blue", "Other" = "grey60")) +
  annotate("text", x = 0.2, y = 0.04, label = "Forecast lower in April 2025", fontface = "italic") +
  annotate("text", x = 0, y = 0.12, label = "Forecast higher in April 2025", fontface = "italic") +
  labs(x = "Forecast as at October 2024",
       y = "Forecast as at April 2025",
       title = "Changes in IMF forecasts of real GDP growth for 2025, by country",
       subtitle = "Pacific island countries highlighted. Diagonal line shows the same forecast in both 2024 and 2025.",
       caption = "Source: IMF World Economic Outlooks. Growth rates based on constant prices, but are not per capita.")


# note tried transform_modulus but it seems not to work?

svg_png(p2, "../img/00288-pict-growth-scatter", w = 8, h = 6)

