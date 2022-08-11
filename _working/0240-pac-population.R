


library(tidyverse)
library(rsdmx)
library(scales)
library(janitor)
library(ISOcodes)
library(glue)

if(!exists("proj_raw")){
  # This is quite slow - several minutes - but the slow part is apparently parsing
  # the XML in the as_tibble
  proj_raw <- readSDMX(providerId = "PDH", 
                       resource = "data", 
                       flowRef = "DF_POP_PROJ")  |>
    as_tibble() |>
    clean_names()
}

# see https://blog.datawrapper.de/gendercolor/
pal <- c("#D4855A", "#C5CB81")
names(pal) <- c("Female", "Male")

proj_col <- "steelblue"
ff <- "Calibri"

y1 <- 2020
y2 <- 2050



pops <- proj_raw |>
  filter(sex != "_T" & age != "_T") |>
  filter(!geo_pict %in% c("_T", "_TXPNG", "MELXPNG", "MEL", "POL", "MIC")) |>
  filter(indicator == "MIDYEARPOPEST") |>
  mutate(age = gsub("^Y", "", age)) |>
  separate(age, into = c("from", "to"), sep = "T", remove = FALSE) |>
  mutate(age = gsub("T", "-", age),
         age = gsub("-999", "+", age, fixed = TRUE),
         sex = case_when(
           sex == "M" ~ "Male",
           sex == "F" ~ "Female"
         )) |>
  mutate(age = factor(age)) |>
  left_join(ISO_3166_1, by = c("geo_pict" = "Alpha_2")) |>
  rename(pict = Name)

prop_old <- pops |>
  filter(obs_time == y1) |>
  group_by(pict) |>
  summarise(total = sum(obs_value),
            prop_70_plus = sum(obs_value[age == "70+"]) / total) |>
  ungroup()



growth <- pops |>
  filter(obs_time %in% c(y1, y2)) |>
  group_by(pict, obs_time) |>
  summarise(pop = sum(obs_value)) |>
  group_by(pict) |>
  summarise(cagr = (pop[obs_time == y2] / pop[obs_time == y1]) ^ (1 / (y2 - y1)) - 1) |>
  ungroup()


pops$prop_70_plus <- NULL
pops$total <- NULL
pops$cagr <- NULL

pops <- pops |>
  left_join(prop_old, by = "pict") |>
  left_join(growth, by = "pict") |>
  mutate(pict_label = fct_reorder(
    glue("{pict}\n{comma(signif(total, 2), scale = 1/1000, accuracy = 1, suffix = 'k')}, {percent(cagr, accuracy = 0.1)}"),
    prop_70_plus)) |>
  arrange(pict, age)

p1 <- ggplot(pops, aes(y = age, fill = sex)) +
  geom_col(data = filter(pops, sex == "Male" & obs_time == y1), 
           aes(x = obs_value)) +
  geom_col(data = filter(pops, sex == "Female" & obs_time == y1), 
           aes(x = -obs_value)) +
  geom_path(data = filter(pops, sex == "Male" & obs_time == y2),
            aes(x = obs_value, y = as.numeric(age)),
            colour = proj_col) +
  geom_path(data = filter(pops, sex == "Female" & obs_time == y2),
            aes(x = -obs_value, y = as.numeric(age)),
            colour = proj_col) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(label = comma) +
  theme_void(base_family = ff) +
  theme(axis.text.y = element_text(hjust = 1, size = 6),
        axis.title.x = element_text(),
        legend.position = "top",
        plot.caption = element_text(hjust = 0.5, colour = "grey20"),
        panel.background = element_rect(fill = "grey95", colour = NA),
        plot.margin = unit(c(3,3,3,3), "mm")) +
  facet_wrap(~pict_label, scales = "free_x", ncol = 7) +
  labs(title = glue("Population estimates and projections in {y1} and {y2}"),
       subtitle = "Pacific Island Country and Territory members of the Pacific Community, sorted by proportion of elderly population.\n",
       x = "Number of people",
       fill = "",
       caption = glue("Blue lines are the projection to {y2}. Numbers in the titles are estimated population in {y1} and projected compound annual growth rate to {y2}."))

png("../img/0240-pop-pyramids-wide.png", width = 7000, height = 4000, res = 600, type = "cairo-png")
print(p1)
dev.off()

png("../img/0240-pop-pyramids-tall.png", width = 5000, height = 8000, res = 600, type = "cairo-png")
print(p1 + facet_wrap(~pict_label, scales = "free_x", ncol = 3))
dev.off()

