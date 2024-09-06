library(tidyverse)
library(readxl)
library(glue)


download.file("https://www.abs.gov.au/statistics/people/people-and-communities/general-social-survey-summary-results-australia/2020/GSS_Table5.xlsx",
              destfile = "gss_table5.xlsx", mode = "wb")

d <- read_excel("gss_table5.xlsx", skip = 6, sheet = "Table 5.1_Estimate") |>
  rename(variable = ...1) |>
  filter(!is.na(variable)) |>
  mutate(category = ifelse(is.na(Heterosexual), variable, NA)) |>
  tidyr::fill(category, .direction = "down") |>
  filter(!is.na(Heterosexual)) |>
  mutate(sequence = 1:n()) |>
  gather(sexuality, value, -variable, -category, -sequence) |>
  mutate(value = as.numeric(value)) |>
  group_by(sexuality, category) |>
  mutate(prop = value / sum(value)) |>
  mutate(variable = fct_reorder(variable, sequence),
         var_wrap = fct_reorder(str_wrap(variable, 20), sequence)) |>
  ungroup()

d
View(d)

# some categories have long answers and are difficult to present on a chart
difficult_cats <- c("Community involvement", "Cultural tolerance and discrimination",
                    "Family and community support", "Crime and safety", "Stressors")

d |>
  filter(!category %in% difficult_cats) |>
  filter(sexuality != "Total persons") |>
  filter(variable != "Persons aged 15 years and over") |>
  ggplot(aes(x = var_wrap, y = prop, fill = sexuality)) +
  geom_col(position = "dodge") +
  facet_wrap(~str_wrap(category, 35), scales = "free") +
  labs(x = "", fill = "") +
  scale_y_continuous(label = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.8, 0.1))


d2 <- d |>
  select(category, variable, sexuality, prop) |>
  spread(sexuality, prop) |>
  mutate(ratio = `Gay, Lesbian or Bisexual` / Heterosexual) |>
  mutate(label = glue("{category} is '{variable}'")) |>
  select(label, ratio)

# The ten things with higher proportion of LBTIQ+ in it
d2 |> arrange(desc(ratio))  |> slice(1:10)
  
# the ten things with lower proportion
d2 |> arrange(ratio) |> slice(1:10)
