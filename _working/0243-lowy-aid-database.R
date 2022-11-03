library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(spcstyle)


# download and unzip from the 'download full data' button at https://pacificaidmap.lowyinstitute.org/database
if(!exists("pamd")){
pamd <- read_excel("Lowy-Institute-Pacific-Aid-Map-Database/2022 Lowy Institute Pacific Aid Map - Full.xlsx",
                   sheet = "Database")
}
#------------------familiarisation----------------------

names(pamd)


dim(pamd)

count(pamd, `Lowy Identifier`, sort = TRUE)

pamd |>
  filter(`Lowy Identifier` == "18A215-INM529-PNG") |>
  count(`Project titles`, sort = TRUE)

if(!exists("pamd_l")){
  pamd_l <- pamd |>
    mutate(id = 1:n()) |>
    gather(variable, value, -id) |>
    drop_na()
}

count(pamd_l, value)
count(pamd_l, variable, sort = TRUE) |>
  View()

# 33,641 values of 0 
pamd_l |>
  filter(variable == "end-planned_lang") |>
  count(value)


# all the expenditure is zero
pamd |>
  filter(!is.na(expenditure)) |>
  count(expenditure)

# the columns that are used for the summary pivot table  
count(pamd, `AID TYPE`)
count(pamd, `Committed/ Spent`)
count(pamd, `Donor`)
count(pamd, `FINANCE TYPE`) # note a few discrpencies eg 'Concessional loan' and 'Concessional Loan'; 'Grant-in-aid' and 'Grant-in-Aid'; 'Standard grant' and 'Standard Grant'
count(pamd, `Recipient Country`)
count(pamd, `Final Type of Flow`)

pamd |> count(`Final Transaction Date`)

count(pamd, `sectors`) # colon separated mltiple values

count(pamd, `HIGH LEVEL SECTOR`)


#--------------------------------recreate the basic pivot table---------------------------------
last_year <- 2020

agg_aid<- function(d){
  d  |>
    mutate(Years = lubridate::year(`Final Transaction Date`)) |>
    group_by(`Committed/ Spent`,
             `AID TYPE`,
             `Recipient Country`,
             `Final Type of Flow`,
             `Donor`,
             Years) |>
    summarise(value = sum(`TRANSACTION VALUE USD`, na.rm = TRUE))
}

aggs <- agg_aid(pamd)
  

aggs |>
  filter(`Committed/ Spent` == "Spent") |>
  # 2020 the last year with full data
  filter(Years <= last_year) |>
  filter(Years >= 2009) |>
  group_by(`Final Type of Flow`,
           Years,
           `Recipient Country`) |>
  summarise(value = sum(value)) |>
  ungroup() |>
  mutate(country = fct_reorder(`Recipient Country`, -value, .fun = sum)) |>
  ggplot(aes(x = Years, y = value / 1e6, colour = `Final Type of Flow`)) +
  facet_wrap(~country, scales = "free_y") +
  geom_line() +
#  geom_point() +
  scale_x_continuous(breaks = 2009:last_year) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(label = dollar_format(suffix = "m")) +
  labs(title = "Financial flows to Pacific Island countries",
       subtitle = "Actually spent")


aggs |>
  filter(`Committed/ Spent` == "Committed") |>
  filter(Years <= last_year) |>
  filter(Years >= 2009) |>
  group_by(`Final Type of Flow`,
           Years,
           `Recipient Country`) |>
  summarise(value = sum(value)) |>
  ungroup() |>
  mutate(country = fct_reorder(`Recipient Country`, -value, .fun = sum)) |>
  ggplot(aes(x = Years, y = value / 1e6, colour = `Final Type of Flow`)) +
  facet_wrap(~country, scales = "free_y") +
  geom_line() +
  #  geom_point() +
  scale_x_continuous(breaks = 2009:last_year) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(label = dollar_format(suffix = "m"))


#----------------------find data and stats-related projects-------------------


pamd_stats <- pamd |>
  mutate(pt = tolower(`Project titles`)) |>
  filter(grepl("data", pt) |
           grepl("statistic", pt) |
           grepl("hies", pt) |
           grepl("population dynamics", pt) |
           grepl("demograph", pt) |
           grepl("MICS", `Project titles`) |
           grepl("dhs", pt) |
           grepl("lfs", pt) |
           grepl("survey", pt)) |>
  # knock out some false positives:
  filter(!grepl("unexploded", pt)) |>
  filter(!grepl("geological survey", pt)) |>
  filter(!grepl("geophysical survey", pt)) |>
  filter(!grepl("graphical survey", pt)) |>
  filter(!grepl("graphic survey", pt)) |>
  filter(!grepl("explosive remnants", pt)) |>
  filter(!grepl("clearance", pt)) |>
  filter(!grepl("sediment survey", pt)) |>
  filter(!grepl("topographic data collection", pt)) |>
  filter(!grepl("surveying and mapping", pt)) |>
  mutate(Years = lubridate::year(`Final Transaction Date`))


# check for false positives that we don't think are relating to statistics
stats |>
  filter(`Committed/ Spent` == "Spent") |>
  group_by(Donor, `Project titles`, Years) |>
  summarise(spend = sum(`TRANSACTION VALUE USD`)) |>
  ungroup() |>
  mutate(`Project titles` = fct_reorder(`Project titles`, -spend, .fun = sum)) |>
  spread(Years, spend, fill = 0) |>
  arrange(`Project titles`) |>
  write_csv("stats-projects-multi-year-spend.csv")

stats |>
  filter(`Committed/ Spent` == "Spent") |>
  count(Donor, `Project titles`, pt, sort = TRUE, wt = `TRANSACTION VALUE USD`) |>
  filter(grepl("survey", pt))


stats_agg <- stats |>
  agg_aid() |>
  filter(`Committed/ Spent` == "Spent") |>
  # 2020 the last year with full data
  filter(Years <= last_year) |>
  filter(Years >= 2009) |>
  group_by(Years,
           `Donor`) |>
  summarise(value = sum(value)) |>
  ungroup() |>
  mutate(donor = fct_reorder(`Donor`, -value, .fun = sum)) |>
  mutate(donor = fct_lump(donor, n = 7, w = value)) |>
  group_by(Years,
           donor) |>
  summarise(value = sum(value)) |>
  ungroup()

#CairoWin()

 stats_agg |>
   mutate(donor = fct_reorder(donor, value, .fun = sum)) |>
  ggplot(aes(x = Years, y = value / 1e6, fill = donor)) +
  theme(text = element_text(family = "Calibri"),
        legend.position = "right") +
  geom_col() +
  scale_x_continuous(breaks = 2009:last_year) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(label = dollar_format(suffix = "m", accuracy = 0.1)) +
  scale_fill_manual(values = spc_cols(8:1)) +
  expand_limits(y = 0) +
  labs(title = "Financial flows to Pacific Island countries relating to statistics",
       subtitle = "Projects with 'data', 'statistics', 'demography', 'population dynamics' or a survey in their titles, excluding geological, topgraphical and unexploded ordinance surveys",
       y = "US Dollars spent", 
       x = "",
       caption = "Source: SPC analysis with data from the Lowy Institute Pacific Aid Map",
       fill = "")
 