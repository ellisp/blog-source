


library(tidyverse)
library(rsdmx)
library(scales)
library(janitor)
library(ISOcodes)
library(glue)
require(spcstyle)



d <- readSDMX("https://stats-sdmx-disseminate.pacificdata.org/rest/data/SPC,DF_POP_PROJ,3.0/A.AS+CK+FJ+PF+GU+KI+MH+FM+NR+NC+NU+MP+PW+PG+PN+WS+SB+TK+TO+TV+VU+WF+_T+MEL+MIC+POL+_TXPNG+MELXPNG.MIDYEARPOPEST._T._T?startPeriod=1950&endPeriod=2050&dimensionAtObservation=AllDimensions") |> 
  as_tibble() |> 
  clean_names() |> 
  mutate(time_period = as.numeric(time_period))

head(ISOcodes::ISO_3166_1)

mel <- c("Melanesia", "Papua New Guinea", "Fiji", "Solomon Islands", "Vanuatu", "New Caledonia")
pol <- c("Polynesia", "Tonga", "Samoa", "Cook Islands", "Tuvalu", "American Samoa", "Pitcairn", "Wallis and Futuna", "French Polynesia", "Niue", "Tokelau")

pict_names <- tribble(~Alpha_2, ~Name,
                       "_T", "All PICTs",
                       "MEL", "Melanesia",
                        "_TXPNG", "Total excluding PNG",
                       "POL", "Polynesia",
                       "MIC", "Micronesia")  |> 
  bind_rows(select(ISO_3166_1, Alpha_2, Name)) |> 
  rename(geo_pict = Alpha_2,
          pict = Name) |> 
  mutate(region = case_when(
    pict %in% mel ~ "Melanesia",
    pict %in% pol ~ "Polynesia",
    grepl("^_T", geo_pict) ~ "Total",
    TRUE ~ "Micronesia"
  ))


d2 <- d |> 
  mutate(era = ifelse(time_period <= 2025, "Past", "Future")) |> 
  inner_join(pict_names, by = "geo_pict") |> 
  mutate(pict = fct_reorder(pict, obs_value, .fun = last)) 

d2 |> 
  ggplot(aes(x = time_period, y = obs_value, colour = era)) +
  facet_wrap(~pict, scales = "free_y", ncol = 7) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(label = comma) +
  labs(x = "", y = "")

library(extrafont)

the_font <- "Calibri" 
d3 <- d2 |> 
  group_by(pict, region) |> 
  summarise(pop2025 = obs_value[time_period == 2025],
            pop2020 = obs_value[time_period == 2020]) |> 
  mutate(cagr = (pop2025 / pop2020) ^ (1/5) - 1) 

p2 <- d3 |> 
  ggplot(aes(x = pop2025, y = cagr, colour = region)) +
  annotate("rect", xmin = 30, xmax = Inf, ymin = 0, ymax = -Inf, alpha = 0.2) +
  geom_point(size = 2.5) +
  theme_minimal(base_family = the_font) +
  geom_text_repel(aes(label = pict), seed = 123, family = the_font) +
  scale_x_log10(label = comma, breaks = c(sort(unique(d3$pop2025))[1:4], 3e4, 1e5, 3e5, 1e6, 3e6, 1e7)) +
  scale_y_continuous(label = percent) +
  scale_colour_manual(values = spc_cols()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank()) +
  labs(x = "Population in 2025",
       y = "Average annual population growth rate 2020 to 2025",
       colour = "",
      title = "Current population and recent growth in the Pacific",
      caption = "Source: UN World Population Prospects, from the Pacific Data Hub")

svg_png(p2, "../img/0305-scatter", w = 10, h = 6)
