
library(tidyverse)
library(readxl)
library(ggrepel)
library(spcstyle)

download.file("https://www.un.org/ohrlls/sites/www.un.org.ohrlls/files/files/mvi_results.xlsx", 
              destfile  ="mvi_results.xlsx", mode = "wb")

mvi <- read_excel("mvi_results.xlsx", sheet = "MVI scores", skip = 1) |>
  filter(!is.na(Country))

sv <- read_excel("mvi_results.xlsx", sheet = "Structural vulnerability") |>
  select(-Country) |>
  gather(variable, value, -ISO) |>
  mutate(type = "Structural vulnerability")
  
lsr <- read_excel("mvi_results.xlsx", sheet = "Lack of structural resilience") |>
  select(-Country) |>
  gather(variable, value, -ISO) |>
  mutate(type = "Lack of sructural resilience")



picts <- c(
  "Fiji",
  "Micronesia (Federated States of)",
  "Kiribati",
  "Marshall Islands",
  "Nauru",
  "Palau",
  "Papua New Guinea",
  "Samoa",
  "Solomon Islands",
  "Tonga",
  "Tuvalu",
  "Vanuatu"
)

# check all present:
stopifnot(all(picts %in% unique(mvi$Country)))

# 10 SPC PICTs thare excluded are the 3 NZ Realm territories, 3 French territories, 3 US and 1 UK

rawvars <- c(
  "Merchandise and services export concentration",                      
  "Instability of export revenue",                                      
  "Food and fuel import dependency",                                    
  "Victims of natural hazards",                                         
  "Damages related to natural hazard",                                  
  "Rainfall shocks",                                                    
  "Temperature shocks",                                                 
  "Drylands",                                                           
  "Low elevated coastal zones",                                         
  "Victims of epidemics",                                               
  "Regional Conflict-related death (excluding own country's data)",     
  "Regional Homicide (excluding own country's data)",                   
  "Refugees from abroad",                                               
  "Low connectivity",                                                   
  "Low population size",                                                
  "Low gross fixed capital formation",                                  
  "Production concentration index",                                     
   "Renewable internal freshwater resources",                            
  "Lack of crop land",                                                  
  "Low tree cover",                                                     
  "Dependency ratio",                                                   
  "Population density",                                                 
  "Low number of people using at least basic sanitation services",      
  "Under-5 mortality",                                                  
  "Low years of schooling",                                             
  "Low proportion of seats held by women in national parliaments"  
)

components <- sv |>
  rbind(lsr) |>
  left_join(select(mvi, Country, ISO), by ="ISO") |>
  mutate(is_pict = if_else(Country %in% picts, "Pacific Island", "Other")) |>
  mutate(raw_var = variable %in% rawvars) |>
  # take out the actual indexes
  filter(!grepl("Index", variable)) |>
  mutate(value = as.numeric(value)) |>
  mutate(country2 = ifelse(is_pict == "Pacific Island", Country, "Median non-Pacific"))
  
mvi <- mvi |>
  mutate(is_pict = if_else(Country %in% picts, "Pacific Island", "Other")) |>
  rename(svi = `Structural vulnerability index`,
         lsri = `Lack of Structural Resilience Index`)

#------------------overview of 2 dimensions-----------

mc <- "grey50"
p1 <- mvi |>
  ggplot(aes(x = svi,
             y = lsri,
             colour = is_pict)) +
  geom_vline(xintercept = median(mvi$svi), colour = mc) +
  geom_hline(yintercept = median(mvi$lsri, na.rm = TRUE), colour = mc) +
  annotate("text", 
           x = c(20, median(mvi$svi) - 3),  
           y = c(median(mvi$lsri) +1, 35), 
           label = "Median", colour = mc) +
  geom_point() +
  geom_text_repel(data = filter(mvi, is_pict == "Pacific Island" | svi > 70 | lsri > 72), 
                  aes(label = Country), size = 3) +
  coord_equal() +
  labs(x = "Structural vulnerability",
       y = "Lack of of social resilience",
       title = "The two indexes that make up the Multidimensional Vulnerability Index") +
  scale_colour_manual(values = c("grey70", spc_cols(2))) +
  theme(legend.position = "none",
        panel.grid = element_blank())

svg_png(p1, "../img/0255-two-indexes", w = 8, h = 7)


p2 <- components |>
  filter(type == "Structural vulnerability") |>
  filter(raw_var) |>
  group_by(country2, variable, type, is_pict) |>
  summarise(value = median(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(country2 = fct_reorder(country2, value),
         variable = str_wrap(variable, 30),
         country2 = fct_relevel(country2, "Median non-Pacific")) |>
  ggplot(aes(x = value, y = country2, fill = is_pict)) +
  facet_wrap(~variable) +
  geom_col() +
  scale_fill_manual(values = c("grey70", spc_cols(2))) +
  theme(legend.position = "none") +
  labs(title = "Original variables making up the Structural Vulnerability Index",
       subtitle = "One of the two indexes making up the Multidimensional Vulnerability Index",
       x = "", y = "")

p3 <- components |>
  filter(type != "Structural vulnerability") |>
  filter(raw_var) |>
  group_by(country2, variable, type, is_pict) |>
  summarise(value = median(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(country2 = fct_reorder(country2, value),
         variable = str_wrap(variable, 30),
         country2 = fct_relevel(country2, "Median non-Pacific")) |>
  ggplot(aes(x = value, y = country2, fill = is_pict)) +
  facet_wrap(~variable) +
  geom_col() +
  scale_fill_manual(values = c("grey70", spc_cols(2))) +
  theme(legend.position = "none")  +
  labs(title = "Original variables making up the Lack of Structural Resilience Index",
       subtitle = "One of the two indexes making up the Multidimensional Vulnerability Index",
       x = "", y = "")


p4 <- components |>
  filter(type == "Structural vulnerability") |>
  filter(!raw_var) |>
  group_by(country2, variable, type, is_pict) |>
  summarise(value = median(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(country2 = fct_reorder(country2, value),
         variable = str_wrap(variable, 30),
         country2 = fct_relevel(country2, "Median non-Pacific")) |>
  ggplot(aes(x = value, y = country2, fill = is_pict)) +
  facet_wrap(~variable) +
  geom_col() +
  scale_fill_manual(values = c("grey70", spc_cols(2))) +
  theme(legend.position = "none") +
  labs(title = "Constructed concepts making up the Structural Vulnerability Index",
       subtitle = "One of the two indexes making up the Multidimensional Vulnerability Index",
       x = "", y = "")

p5 <- components |>
  filter(type != "Structural vulnerability") |>
  filter(!raw_var) |>
  group_by(country2, variable, type, is_pict) |>
  summarise(value = median(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(country2 = fct_reorder(country2, value),
         variable = str_wrap(variable, 30),
         country2 = fct_relevel(country2, "Median non-Pacific")) |>
  ggplot(aes(x = value, y = country2, fill = is_pict)) +
  facet_wrap(~variable) +
  geom_col() +
  scale_fill_manual(values = c("grey70", spc_cols(2))) +
  theme(legend.position = "none")  +
  labs(title = "Constructed concepts making up the Lack of Structural Resilience Index",
       subtitle = "One of the two indexes making up the Multidimensional Vulnerability Index",
       x = "", y = "")

p1
p2
p4
p3
p5
