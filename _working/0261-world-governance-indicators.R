library(tidyverse)
library(readxl)
library(randomcoloR)
library(snakecase)

fn <- "wgidataset.xlsx"
download.file("https://www.worldbank.org/content/dam/sites/govindicators/doc/wgidataset.xlsx",
              destfile = fn, mode = "wb")



topics <- readxl::excel_sheets(fn)[-1]
sheets <- list()

for (i in 1:length(topics)){
  the_topic <- topics[i]
  
  d <- read_excel(fn, sheet = the_topic, skip = 13, col_names = FALSE)
  years <- as.numeric(d[1, ])
  types <- as.character(d[2,])
  names(d) <- paste(types, years, sep=":")
  names(d)[1:2] <- c("country", "iso3")
  sheets[[i]] <- d |>
    filter(!`Estimate:1996` %in% c("1996", "Estimate")) |>
    gather(variable, value, -country, -iso3) |>
    separate(variable, into = c("type", "year"), sep = ":") |>
    mutate(year = as.numeric(year),
           value = as.numeric(value),
           indicator = the_topic)
}

wgi <- bind_rows(sheets)

wgi |>
  filter(type == "Estimate") |>
  ggplot(aes(x = year, y = value, colour = country)) +
  facet_wrap(~indicator) +
  geom_line() +
  theme(legend.position = "none")


picts <- c("FJI", "PNG","VUT", "SLB",
           "FSM", "KIR", "MHL", "PLW", "NRU", 
           "NIU", "WSM", "TON", "TUV", "ASM",  "COK")

stopifnot(all(picts %in% wgi$iso3))
wgi <- sheets |>
  bind_rows() |>
  mutate(is_pict = ifelse(iso3 %in% picts, "PICT", "Other"),
         ipn = as.numeric(as.factor(is_pict))) |>
  mutate(country = fct_reorder(country, ipn)) |>
  mutate(indicator = to_sentence_case(indicator),
         indicator = str_replace_all(indicator, "of ", " of "),
         indicator = str_replace_all(indicator, "and ", " of "))



pal <- c("PICT" = "blue", "Other" = "grey80")

wgi |>
  filter(type == "Estimate") |>
  ggplot(aes(x = year, y = value, colour = is_pict, group = country)) +
  facet_wrap(~indicator) +
  geom_line() +
  theme(legend.position = "none") +
  scale_colour_manual(values = pal)

set.seed(123)
pal2 <- c(distinctColorPalette(length(picts)), "grey30")
pict_names <- wgi |>
  filter(is_pict == "PICT" & type == "Estimate") |>
  mutate(country = fct_reorder(country, value, .na_rm = TRUE)) |>
  distinct(country) |>
  arrange(desc(country)) |>
  pull(country) |>
  as.character()

names(pal2) <- c(pict_names, "Other")

d_plot <- wgi |>
  filter(type == "Estimate") |>
  mutate(country2 = ifelse(is_pict == "PICT", as.character(country), "Other")) |>
  mutate(country2 = factor(country2, levels = names(pal2))) 

p3 <- d_plot |>
  ggplot(aes(x = year, y = value, colour = country2, group = country)) +
  facet_wrap(~indicator) +
  geom_line() +
  geom_line(data = filter(d_plot, is_pict == "PICT"), size = 1.1) +
  geom_text_repel(data = filter(d_plot, year == max(year) & is_pict == "PICT"), 
            aes(label = iso3), hjust = 0, size = 2.5, direction = "y", nudge_x = 0.5,
            seed = 123, min.segment.length = 5) +
  theme(legend.position = "right",
        panel.background = element_rect(fill = "black", colour = NA),
        panel.grid = element_blank(),
        panel.spacing = unit(5, "mm"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values = pal2) +
  expand_limits(x = 2024) +
  labs(colour = "",
       x= "",
       y= "",
       title = "Governance indicators in the Pacific",
       subtitle = "Grey lines indicate countries other than those that are Pacific Islands",
       caption = "Source: Daniel Kaufmann and Aart Kraay (2023). Worldwide Governance Indicators, 2023 Update (www.govindicators.org), Accessed on 06/03/2024.

")

svg_png(p3, "../img/0261-mvi-picts", w = 12, h = 7)

