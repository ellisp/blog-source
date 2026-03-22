

library(owidapi)
library(tidyverse)

country_codes <- 
  c(
    "ASM", "COK", "FSM", "FJI", "PYF", "GUM", "KIR", "MHL", "NRU", "NCL",
    "NIU", "MNP", "PLW", "PNG", "PCN", "WSM", "SLB", "TKL", "TON", "TUV",
    "VUT", "WLF", "AUS", "NZL"
  )
stopifnot(length(country_codes) == 24)

palette <- c(
  coal = "brown",
  gas = "magenta",
  oil = "red",
  solar = "yellow",
  wind = "steelblue",
  hydro = "darkblue",
  bioenergy = "lightgreen",
  'other renewables' = "darkgreen"
)





#-------------------electricity mix-----------------
elec_mix <- owid_get(
  chart_id = "share-elec-by-source",
  entities  = country_codes
)

p1 <- elec_mix |> 
   rename(country = entity_name) |> 
   select(-entity_id) |> 
   gather(variable, value, -country, -year) |>
   filter(value != 0) |> 
   filter(year > 2001) |> 
   mutate(variable = gsub("_share_of_electricity__pct", "", variable, fixed = TRUE),
          variable =gsub("_", " ", variable),
          variable =gsub(" excluding bioenergy", "", variable),
          variable = fct_drop(variable)) |> 
   mutate(variable = fct_relevel(variable, c("bioenergy", "hydro", "other renewables"), after = Inf)) |> 
   mutate(country = fct_relevel(country, c("Australia", "New Zealand"), after = Inf)) |> 
   ggplot(aes(x = year, y = value, fill = variable)) +
   facet_wrap(~country, ncol = 5) +
   scale_fill_manual(values = palette) +
   geom_col() +
   labs(y = "Percentage of electricity",
        fill = "Source:",
        title = "Share of electricity by source in Oceania",
        x = "",
        caption = "Source: Ember (2026); Energy Institute - Statistical Review of World Energy (2025). Data processed by Our World In Data. Chart by freerangestats.info.") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))

svg_png(p1, "../img/0320-pict-electricity-mix", w = 10, h = 6) 


#--------------------all energy mix-------------------
# chart id here is wrong. But do I really need this?
energy_mix <- owid_get(
  chart_id = "share-energy-by-source",
  entities  = country_codes
)

