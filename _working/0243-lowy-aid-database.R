library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(spcstyle)


# download and unzip from the 'download full data' button at https://pacificaidmap.lowyinstitute.org/database
if(!exists("pamd")){
pamd <- read_excel("Lowy-Institute-Pacific-Aid-Map-Database/2022 Lowy Institute Pacific Aid Map - Full.xlsx",
                   sheet = "Database") |>
  mutate(id = 1:n())
}
#------------------familiarisation----------------------

names(pamd)


dim(pamd)

count(pamd, `Lowy Identifier`, sort = TRUE)

pamd |>
  filter(`Lowy Identifier` == "18A215-INM529-PNG") |>
  count(`Project titles`, sort = TRUE)

if(!exists("pamd_l")){
  pamd_l <- pamd  |>
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

sectors_long <- pamd |>
  mutate(sectors = gsub(";\\s*;", "", sectors)) |>
  mutate(sectors = gsub("^\\s*;", "", sectors)) |>
  # some projects eg 279860 have more than 26 sectors
  separate(sectors, into = c(LETTERS, letters), sep = ";") |>
  select(id, A:Z) |>
  gather(sequence, sector, -id) |>
  filter(!is.na(sector)) |>
  filter(sector != "0") |>
  mutate(sector = str_squish(sector)) |>
  distinct()

sectors_long |>
  count(sector, sort = TRUE)


pamd_stats <- pamd |>
  mutate(pt = tolower(`Project titles`)) |>
  mutate(prob_data = grepl("data", pt) |
           grepl("statistic", pt) |
           grepl("hies", pt) |
           grepl("population dynamics", pt) |
           grepl("demograph", pt) |
           grepl("MICS", `Project titles`) |
           grepl("census", pt) |
           grepl("dhs", pt) |
           grepl("lfs", pt) |
           grepl("survey", pt) |
           grepl("statistical capacity building", sectors, ignore.case = TRUE) |
           grepl("data development and capacity building", sectors, ignore.case = TRUE) |
           grepl("data production, accessibility and use", sectors, ignore.case = TRUE)) |>
  # knock out some false positives:
  mutate(prob_data = if_else(
    grepl("unexploded", pt) |
      grepl("geological survey", pt) |
      grepl("geophysical survey", pt) |
      grepl("graphical survey", pt) |
      grepl("graphic survey", pt) |
      grepl("explosive remnants", pt) |
      grepl("clearance", pt) |
      grepl("sediment survey", pt) |
      grepl("topographic data collection", pt) |
      grepl("strongim gavman", pt) |
      grepl("fiji recovery and resilience", pt) |
      grepl("fiji social protection covid", pt) |
      grepl("vanuatu climate resilient", pt) |
      grepl("strengthen budget execution", pt) |
      grepl("tc winston impact evaluation", pt) |
      grepl("meteorological satellite", pt) |
      grepl("border security", pt) |
      grepl("surveying and mapping", pt),
    FALSE, prob_data)) |>
  mutate(Years = lubridate::year(`Final Transaction Date`))

# look at the sector breakdown
# pamd_stats |>
#   select(id, prob_data, `TRANSACTION VALUE USD`) |>
#   left_join(sectors_long, by = "id") |>
#   group_by(sector, prob_data) |>
#   summarise(value = sum(`TRANSACTION VALUE USD`)) |>
#   spread(prob_data, value, fill = 0) |>
#   arrange(desc(`TRUE`)) |>
#   View()


pamd_stats |>
  select(id, `Project titles`, prob_data, `TRANSACTION VALUE USD`) |>
  inner_join(sectors_long, by = "id") |>
  group_by(`Project titles`, sector, prob_data) |>
  summarise(value = sum(`TRANSACTION VALUE USD`, na.rm = TRUE)) |>
  group_by(`Project titles`, prob_data) |>
  mutate(value = value / n()) |>
  group_by(sector, prob_data) |>
  summarise(value = sum(value)) |>
  ungroup() |>
  filter(value >= 0) |>
  mutate(data_value = value * prob_data) |>
  mutate(sector_lumped = fct_lump(sector, n = 8, w = data_value)) |>
  mutate(sector_lumped = fct_reorder(sector_lumped, data_value, .fun = sum)) |>
  group_by(sector_lumped, prob_data) |>
  summarise(value = sum(value)) |>
  ungroup() |>
  mutate(sector_lumped = fct_relevel(sector_lumped, "Other")) |>
  ggplot(aes(x = prob_data, y = value, fill = sector_lumped)) +
  geom_col(position = "fill", colour = "white") +
  #scale_fill_manual(values = spc_cols(1:11)) +
  labs(title = "Top sectors for aid to the Pacific, 2009-2020",
       subtitle = "Data-focused aid compared to other types of aid") +
  scale_y_continuous()

stats <- filter(pamd_stats, prob_data)

stats |>
  left_join(sectors_long, by = "id") |>
  filter(sector == "Adaptation") |>
  distinct(`Project titles`)

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


stats |>
  filter(`Committed/ Spent` == "Spent") |>
  count(Donor, `Project titles`, pt, sort = TRUE, wt = `TRANSACTION VALUE USD`) |>
  filter(grepl("census", pt))

stats |>
  filter(grepl("spc", `Project titles`, ignore.case = TRUE)) |>
  agg_aid()

stats_agg <- stats |>
  agg_aid() |>
  filter(`Committed/ Spent` == "Spent") |>
  filter(Years <= last_year) |>
  filter(Years >= 2009) |>
  group_by(Years,
           `Donor`) |>
  summarise(value = sum(value)) |>
  ungroup() |>
  mutate(donor = fct_reorder(`Donor`, -value, .fun = sum)) |>
  mutate(donor = fct_lump(donor, n = 8, w = value)) |>
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
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(label = dollar_format(suffix = "m", accuracy = 0.1)) +
  scale_fill_manual(values = spc_cols(9:1)) +
  expand_limits(y = 0) +
  labs(title = "Financial flows to Pacific Island countries relating to statistics",
       subtitle = "Projects in a data sector or with 'census', 'data', 'statistics', 'demography', 'population dynamics' or a survey in their titles, excluding geological, topographical and unexploded ordinance surveys.
Excludes core funding for SPC and for PFTAC, which would require extra data sources to identify the 'statistics' component of.",
       y = "US Dollars spent", 
       x = "",
       caption = "Source: SPC analysis with data from the Lowy Institute Pacific Aid Map",
       fill = "")
 
# 2013 a big year for Australian support for statistics, with US$2.5m on SPC statistics, US$1.1m for the second year
# of the Solomon Islands HIES, and US$1.7m for a project in PNG with health survey all of which went to nearly nothing 
# in 2014; and ABS support of US$1m that went down to US$570k the next year

# these charts exclude PFTAC because we can't separate out the statistics adviser from the others:
filter(pamd, grepl("pftac", `Project titles`, ignore.case = TRUE)) |>
  distinct(`Project titles`)




pamd_stats |>
  filter(grepl("spc", `Project titles`, ignore.case = TRUE)) |>
  distinct(`Project titles`, prob_data) |> 
  View()



#statistics support for SPC
stats |>
  filter(grepl("spc", `Project titles`, ignore.case = TRUE)) |>
  agg_aid() |>
  filter(`Committed/ Spent` == "Spent") |>
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
  ungroup() |>
  mutate(donor = fct_reorder(donor, value, .fun = sum)) |>
  ggplot(aes(x = Years, y = value / 1e6, fill = donor)) +
  theme(text = element_text(family = "Calibri"),
        legend.position = "right") +
  geom_col() +
  scale_x_continuous(breaks = 2009:last_year) +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(label = dollar_format(suffix = "m", accuracy = 0.1)) +
  scale_fill_manual(values = spc_cols(8:1)) +
  expand_limits(y = 0) +
  labs(title = "Program and project financial flows to relating to statistics delivered through SPC",
       subtitle = "Excludes SPC Core funding, approximately US$1m of which goes to statistics",
       y = "US Dollars spent", 
       x = "",
       caption = "Source: SPC analysis with data from the Lowy Institute Pacific Aid Map",
       fill = "")


