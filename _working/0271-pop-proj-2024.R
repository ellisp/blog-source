library(tidyverse)
library(scales)
library(readxl)
library(janitor)
library(ggrepel)

options(timeout=600)
df1 <- "pp24_agegrp.csv"
if(!file.exists(df1)){
  download.file("https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Population1JanuaryByAge5GroupSex_Medium.csv.gz",
                destfile = df1, mode = "wb")
}


df2 <- "pp24_standard.xlsx"
if(!file.exists(df2)){
  unlink(df2)
  download.file("https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/EXCEL_FILES/1_General/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx",
                destfile = df2, mode = "wb")
}


standard <- read_excel(df2, skip = 16) |>
  rename(pop = `Total Population, as of 1 July (thousands)`)

agegrp <- read_csv(df1) |>
  clean_names()

picts <- c(
  "Fiji",
  "New Caledonia",
  "Papua New Guinea",
  "Solomon Islands",
  "Vanuatu",
  "Guam",
  "Kiribati",
  "Marshall Islands",
  "Micronesia (Fed. States of)",
  "Nauru",
  "Northern Mariana Islands",
  "Palau",
  "American Samoa",
  "Cook Islands",
  "French Polynesia",
  "Niue",
  "Samoa",
  "Tokelau",
  "Tonga",
  "Tuvalu",
  "Wallis and Futuna Islands"
)

stopifnot(length(picts) == 21)         
stopifnot(all(picts %in% agegrp$Location))

old_grps <- c("65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+") 
stopifnot(all(old_grps %in% unique(agegrp$age_grp)))

pict_sum <- agegrp |>
  filter(location %in% picts) |>
  mutate(old = age_grp %in% old_grps) |>
  group_by(location, time) |>
  summarise(pop = sum(pop_total),
            prop_old = sum(pop_total[old]) / pop) |>
  ungroup() |>
  filter(time %in% 1950:2050)
  # filter(time %in% seq(from = 1950, to = 2050, by = 5))

p <- pict_sum |>
  ggplot(aes(x = time, y = prop_old, colour = location)) +
  annotate("rect", xmin = 2024, xmax = 2050, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.5) +
  geom_line() +
  geom_point(data = filter(pict_sum, time %in% c(1950, 2024, 2050)), 
             aes(size = pop), alpha = 0.5) +
  geom_text_repel(data = filter(pict_sum, time == 2050), direction = "y",
                  aes(label = location), hjust = 0, nudge_x = 3, 
                  min.segment.length = 5, family = "Calibri") +
  xlim(1950, 2070) +
  guides(colour = "none") +
  scale_y_continuous(label = percent) +
  scale_size_area(label = comma_format(suffix = "m", scale = 1/1000), 
                  breaks = c(0.1, 5, 10) * 1000, max_size = 12) +
  theme(legend.position = c(0.2, 0.8)) +
  labs(x = "", y = "Proportion of population that is 65 or older",
       size= " Population",
       title = "Aging populations in the Pacific",
       caption = "Source: UN World Population Prospects 2024")

svg_png(p, file = "../img/0271-aging-linechart", w = 10, h = 6)
                  
                  

