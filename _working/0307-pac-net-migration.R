# draws charts of migration and natural change impact on growth
# The source for this is UN population projections. Some details
# on net migration aren't in the PDH.stat so we get them from
# the UN website.
#
# Peter Ellis November 2025

library(tidyverse)
library(readxl)
library(spcstyle)
library(scales)

#-------------Download and get ready the data---------------

df2 <- "pp24_standard.xlsx"
if(!file.exists(df2)){
  unlink(df2)
  download.file("https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/1_General/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx",
                destfile = df2, mode = "wb")
}

un_indicators <- read_excel(df2, skip = 16) 

picts <- c(
  "Fiji", "New Caledonia", "Papua New Guinea", "Solomon Islands",                                             
  "Guam", "Kiribati", "Marshall Islands", "Micronesia (Fed. States of)", "Nauru",
  "Vanuatu", "Northern Mariana Islands","Palau", "American Samoa", "Cook Islands",
  "French Polynesia", "Niue", "Samoa", "Tokelau", "Tonga", "Tuvalu", "Wallis and Futuna Islands" 
)

stopifnot(all(picts %in% un_indicators$`Region, subregion, country or area *`))

pict_indicators <- un_indicators |> 
  rename(country = `Region, subregion, country or area *`) |> 
  filter(country %in% picts) |> 
  mutate(nnm = as.numeric(`Net Number of Migrants (thousands)`),
         nmr = as.numeric(`Net Migration Rate (per 1,000 population)`) ,
         nc = as.numeric(`Natural Change, Births minus Deaths (thousands)`),
         pc = as.numeric(`Population Change (thousands)`),
         pop1july = as.numeric(`Total Population, as of 1 July (thousands)`)) |> 
  mutate(country = gsub("States of", "States", country), 
         country = fct_reorder(country, abs(nmr)),
         migration_direction = ifelse(nmr <0, -1, 1))

#------------basic faceted plot of all PICTs--------------------

p0 <- pict_indicators |> 
  ggplot(aes(x = Year, y = nmr)) +
  facet_wrap(~country, ncol = 7, scales = "fixed") + 
  geom_line(colour = "grey50", linetype = 1, linewidth = 0.4) +
  geom_ribbon(aes(ymin = 0, ymax = nmr, fill = migration_direction), alpha = 0.5) +
  scale_y_continuous(label = percent_format(scale = 1)) +
  scale_fill_gradient2(low = "red", high = "blue") +
  labs(title = "Net migration impact on Pacific island countries and territories",
       subtitle = "Countries shown in sequence of least proportionately impacted to most",
       x = "",
       y = "Net migration as a proportion of population in residence")  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 7, colour = "grey50"),
        panel.grid.minor = element_blank())


svg_png(p0, "../img/0307-all-picts-net-migration", w = 10, h = 5)

#------------Just six selected PICTs----------------

selected <- c("Niue", "Marshall Islands", "Samoa", "Kiribati", "Northern Mariana Islands", "Papua New Guinea")
stopifnot(all(selected %in% pict_indicators$country))


p1 <- pict_indicators |> 
  filter(country %in% selected) |> 
  select(country, Year, `Net migration gain/loss` = nnm, `Natural increase` = nc, `Total increase` = pc) |> 
  gather(variable, value, -Year, -country) |> 
  mutate(variable = fct_relevel(variable, "Net migration gain/loss", after = Inf)) |> 
  ggplot(aes(x = Year, y = value, colour = variable, linetype = variable)) +
  facet_wrap(~country, ncol = 3, scales = "free_y") + 
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_line() +
  scale_colour_manual(values = spc_cols(c(3, 2, 4))) +
  scale_linetype_manual(values = c(2,1,3)) +
  labs(y = "Annual change in population (thousands)",
       x = "",
       colour = "", linetype = "")

svg_png(p1, "../img/0307-six-picts-natural-immmigration-line", w = 10, h = 5)


