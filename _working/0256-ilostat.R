


# documentation at https://ilostat.github.io/Rilostat/
# available on CRAN:
# install.packages("Rilostat")

library(Rilostat)
library(tidyverse)
library(ISOcodes)

the_caption <- "Source: Data from ILOSTAT, analysis by freerangestats.info"

# "The ilostat R package ('Rilostat') was designed to give data users the
# ability to access the ILOSTAT database, search for data, rearrange the
# information as needed, download it in the desired format, and make various
# data visualizations, all in a programmatic and replicable manner, with the
# possibility of quickly re-running the queries as required"


toc <- get_ilostat_toc()

toc |>
  filter(grepl("unemployment", indicator.label)) |>
  count(indicator, indicator.label, sort = TRUE) |>
  View()


get_data_from_label <- function(the_label, freq = "A"){
  the_id <- toc |>
    filter(indicator.label == the_label &
             freq == "A") |>
    pull(id)
  print(the_id)
  
  d <- get_ilostat(id = the_id, time_format = "num")
  
  return(d)
}


lab1 <- "Prime-age unemployment rate by sex, household type and rural / urban areas (%)"
unemp <-  get_data_from_label(lab1)
# "GED_XLU1_SEX_HHT_GEO_RT_A"
# I think GED refers to the Gender Equality and Non-Discrimination Indicators databas
# see https://ilostat.ilo.org/resources/concepts-and-definitions/description-gender-equality-and-non-discrimination-indicators/

# various household types. Where do we find which ones are C, O, K, E L?
unique(unemp$classif1)

# various rural / urban
unique(unemp$classif2)

set.seed(124)
my_countries <- sample(unique(unemp$ref_area), 4)


d0 <- unemp |>
  left_join(ISO_3166_1, by = c("ref_area" = "Alpha_3")) |>
  filter(sex != "SEX_T") |>
  filter(!is.na(obs_value)) |>
  filter(ref_area %in% my_countries) |>
  rename(country = Name) 

# rural and urban, but no household type info:
d1 <- d0 |>
  filter(classif2 != "GEO_COV_NAT") |>
  filter(classif1 == "HHT_AGGREGATE_TOTAL") 
  

d1 |>
  filter(classif1 == "HHT_AGGREGATE_TOTAL") |>
  ggplot(aes(x = time, y = obs_value, linetype = classif2, shape = classif2, colour = sex)) +
  facet_wrap( ~ country) +
  geom_point() +
  geom_line()

d1 |>
  mutate(country =fct_reorder(country, obs_value)) |>
  ggplot(aes(x = time, y = obs_value, linetype = classif2, shape = classif2, colour = sex)) +
  facet_grid(classif2 ~ country) +
  geom_point() +
  geom_line() +
  labs(title = the_label)



hht <- sdmx_ilostat(dsd = "CL_HHT")

d2 <- d0 |>
  filter(classif2 == "GEO_COV_NAT") |>
  filter(classif1 != "HHT_AGGREGATE_TOTAL")  |>
  left_join(hht, by = c("classif1" = "code")) |>
  rename(hht_label = label)

d2 |>
  mutate(country =fct_reorder(country, obs_value)) |>
  mutate(hht_label = str_wrap(hht_label, 15),
         # use the ordering info that came with the classification:
         hht_label = fct_reorder(hht_label, ORDER)) |>
  ggplot(aes(x = time, y = obs_value, 
             colour = sex)) +
  facet_grid(hht_label ~ country) +
  geom_point() +
  geom_line() +
  labs(title = lab1)

# we can get the code lists for the classifications following 
# a straightforward naming convention. this wasn't obvious to
# me fro the helpfile but wasn't too hard to guess/work out:
sdmx_ilostat(dsd = "CL_HHT")
sdmx_ilostat(dsd = "CL_SEX")
filter(sdmx_ilostat(dsd = "CL_AGE"), IS_TOTAL == "Y")
sdmx_ilostat(dsd = "CL_GEO_COV")


#----------------scatter plot of gender stuff------------

toc |>
  filter(grepl("participation", indicator.label)) |>
  count(indicator, indicator.label, sort = TRUE) |>
  View()






lfpr <- get_data_from_label("Labour force participation rate by sex and age (%)")

ratios1 <- lfpr |>
  filter(classif1 == "AGE_AGGREGATE_TOTAL" &
            sex !="SEX_T") |>
  group_by(ref_area, sex) |>
  arrange(desc(time)) |>
  slice(1) |>
  group_by(ref_area) |>
  summarise(participation_ratio = obs_value[sex == "SEX_M"] / 
              obs_value[sex == "SEX_F"])
  

ratios2 <- unemp |>
  filter(classif2 == "GEO_COV_NAT") |>
  filter(classif1 == "HHT_AGGREGATE_TOTAL") |>
  filter(sex !="SEX_T") |>
  group_by(ref_area, sex) |>
  arrange(desc(time)) |>
  slice(1) |>
  group_by(ref_area) |>
  summarise(unemployment_ratio = obs_value[sex == "SEX_F"] / 
              obs_value[sex == "SEX_M"])

ratios_both <- ratios1 |>
  inner_join(ratios2, by = "ref_area") |>
  left_join(ISO_3166_1, by = c("ref_area" = "Alpha_3")) |>
  rename(country = Name) 

p4 <- ratios_both |>
  ggplot(aes(x = participation_ratio, y = unemployment_ratio)) +
  geom_vline(xintercept = 1, colour = "brown") +
  geom_hline(yintercept = 1, colour = "brown") +
  geom_smooth(method = "lm", colour = "white") +
  geom_point() +
  geom_text_repel(data = filter(ratios_both, participation_ratio > 3 |
                                  unemployment_ratio > 3 |
                                  unemployment_ratio < 0.5),
                  aes(label = country), colour = "steelblue", size = 2.9,
                  seed = 125) +
  scale_x_log10() +
  scale_y_log10() +
  annotate("label", x = 0.9, y = 8, label = "More\nfemale\nunemployment",
           hjust = 0, family = "Roboto", fontface = "italic", 
           label.size = 0, alpha = 0.5) +
  annotate("label", x = 6.5, y = 1.08, label = "Less\nfemale\nparticipation",
           hjust = 0, vjust = 0, family = "Roboto", 
           label.size = 0, alpha = 0.5, fontface = "italic") +
  labs(x = "Male labour force participation as a ratio of female participation\nHigher numbers mean less female participation.",
        y = "Female unemployment as a ratio of male unemployment\nHigher numbers mean more female unemployment",
       title = "Gender comparison of unemployment and labour force participation",
       subtitle = "Latest values for each country",
       caption = the_caption)

svg_png(p4, "../img/0256-scatter", w = 8, h = 6.5)
