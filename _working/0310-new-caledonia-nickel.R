
library(tidyverse)
library(readxl)
library(fredr)

# Nickel sector info from ISEE available at
# https://www.isee.nc/economie-entreprises/entreprises-secteurs-d-activites/secteur-du-nickel

download.file("https://www.isee.nc/component/phocadownload/category/147-consultez-les-donnees-historiques?download=676:les-exportations-de-nickel",
              destfile = "nc-nickel-exports.xls", mode = "wb")

#--------------------minerai---------------------

ore <- read_excel("nc-nickel-exports.xls", sheet = "Minerai mensuel", range = "A29:HG32",
                      col_names = c("destination", 
                                     as.character(seq.Date(from = "2008-01-15", to = "2025-10-20", by = "month")))) |> 
  gather(date, value, -destination) |> 
  mutate(value = as.numeric(value),
         date = as.Date(date),
         datem = format(date, "%b %Y"),
         datem = fct_reorder(datem, date),
         type = "Ore")

labs <- levels(ore$datem)
labs[!grepl("Oct", labs)] <- ""

p1 <- ore |> 
  ggplot(aes(x = datem, y = value, fill = destination)) +
          geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.x = element_blank(),
       panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels = labs) +
  scale_y_continuous(label = comma) +
  labs(x = "", y = "XPF (m)", fill = "Destination:",
       title = "Nickel ore exports from New Caledonia, 2008-2025")

svg_png(p1, "../img/0310-ore", w = 10, h = 5)

#--------------------metallurgie------------------

metal <- read_excel("nc-nickel-exports.xls", sheet = "Métallurgie mensuel", range = "A134:HG138",
                      col_names = c("destination", 
                                     as.character(seq.Date(from = "2008-01-15", to = "2025-10-20", by = "month")))) |> 
  gather(date, value, -destination) |> 
  mutate(value = as.numeric(value),
         date = as.Date(date),
         datem = format(date, "%b %Y"),
         datem = fct_reorder(datem, date),
         type = "Processed metal")

p2 <- metal |> 
  ggplot(aes(x = datem, y = value, fill = destination)) +
          geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.x = element_blank(),
       panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels = labs) +
  scale_y_continuous(label = comma) +
  labs(x = "", y = "XPF (m)", fill = "Destination:",
       title = "Nickel metal exports from New Caledonia, 2008-2025")

svg_png(p2, "../img/0310-metals", w = 10, h = 5)


#----------------------combined------------------

both <- rbind(metal, ore) |> 
  group_by(datem, type) |> 
  summarise(value = sum(value, na.rm = TRUE))

p3 <- both |> 
  ggplot(aes(x = datem, y = value, fill = type)) +
          geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.x = element_blank(),
       panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels = labs) +
  scale_y_continuous(label = comma) +
  labs(x = "", y = "XPF (m)", fill = "Type of export:",
       title = "Nickel exports from New Caledonia 2008-2025")

svg_png(p3, "../img/0310-both", w = 10, h = 5)


nickel_ts <- both |> 
  group_by(datem) |> 
  summarise(value = sum(value)) |> 
  pull(value) |> 
  ts(frequency = 12, start = c(2008, 10))


p4 <- function(){
  par(bty = "l")
  plot(stl(nickel_ts, s.window = 7), col = "steelblue",
      main = "Nickel exports (ore + metals, in millions of XPF) from New Caledonia")
}

svg_png(p4, "../img/0310-decomposed", w = 10, h = 6)

#------------------nickel prices---
fredr_set_key(Sys.getenv("FRED_API_KEY"))

nickel_prices <- fredr(
  series_id = "PNICKUSDM",
  observation_start = as.Date("2008-01-01"),
  observation_end   = Sys.Date()   # or a chosen end date
)

p5 <- nickel_prices |> 
  ggplot(aes(x = date, y = value)) +
  geom_line(colour = "brown") +
  scale_y_continuous(label = dollar) +
  labs(x = "", y = "US dollars (m)",
        title = "World nickel prices",
        subtitle = "(Not adjusted for inflation)")

svg_png(p5, "../img/0310-nickel-prices", w = 10, h = 5)
