
library(tidyverse)
library(readxl)
library(ggrepel)
library(tidygraph)
library(ggraph)
library(RColorBrewer)
library(glue)

if(!file.exists("mvi_results.xlsx")){
  download.file("https://www.un.org/ohrlls/sites/www.un.org.ohrlls/files/files/mvi_results.xlsx", 
                destfile  ="mvi_results.xlsx", mode = "wb")
}

mvi <- read_excel("mvi_results.xlsx", sheet = "MVI scores", skip = 1) |>
  filter(!is.na(Country)) |>
  # fix an encoding problem with the space in South Sudan that doesn't print properly:
  mutate(Country = ifelse(grepl("South.+Sudan", Country), "South Sudan", Country))

sv <- read_excel("mvi_results.xlsx", sheet = "Structural vulnerability") |>
  select(-Country) |>
  gather(variable, value, -ISO) |>
  mutate(type = "Structural vulnerability")
  
lsr <- read_excel("mvi_results.xlsx", sheet = "Lack of structural resilience") |>
  select(-Country) |>
  gather(variable, value, -ISO) |>
  mutate(type = "Lack of structural resilience")



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

mvi_variables <- tribble(~raw_var, ~cat1, ~cat2, ~cat3,
  "Merchandise and services export concentration", "Exposure to fluctuations in international trade and financial flows", "Economic vulnerability", "Structural Vulnerability Index",
  "Instability of export revenue", "Exposure to fluctuations export earnings", "Economic vulnerability", "Structural Vulnerability Index",
  "Food and fuel import dependency","Exposure to fluctuations in strategic import prices", "Economic vulnerability", "Structural Vulnerability Index",
  
  "Victims of natural hazards", "Exposure to natural hazards", "Environmental vulnerability", "Structural Vulnerability Index",
  "Damages related to natural hazard", "Exposure to natural hazards", "Environmental vulnerability", "Structural Vulnerability Index",
  "Rainfall shocks", "Exposure to extreme weather events", "Environmental vulnerability", "Structural Vulnerability Index",
  "Temperature shocks", "Exposure to extreme weather events", "Environmental vulnerability", "Structural Vulnerability Index",
  "Drylands", "Exposure to ecosystem pressure", "Environmental vulnerability", "Structural Vulnerability Index",
  "Low elevated coastal zones", "Exposure to ecosystem pressure", "Environmental vulnerability", "Structural Vulnerability Index",
  
  "Victims of epidemics", "Exposure to global health shocks", "Social vulnerability", "Structural Vulnerability Index",
  "Regional Conflict-related death (excluding own country's data)", "Spillover effects of regional violence", "Social vulnerability", "Structural Vulnerability Index",
  "Regional Homicide (excluding own country's data)", "Spillover effects of regional violence", "Social vulnerability", "Structural Vulnerability Index",     
  "Refugees from abroad", "Exposure to entrance of international forced displacement of people", "Social vulnerability", "Structural Vulnerability Index",     
  
  "Low connectivity", "Low capacity to integrate with international markets", "Lack of Economic Resilience", "Lack of Structural Resilience Index",
  "Low population size", "Lack of economies of scale", "Lack of Economic Resilience", "Lack of Structural Resilience Index",
  "Low gross fixed capital formation", "Low domestic economic capacity", "Lack of Economic Resilience", "Lack of Structural Resilience Index",
  "Production concentration index", "Low domestic economic capacity", "Lack of Economic Resilience", "Lack of Structural Resilience Index",
  
  "Renewable internal freshwater resources", "Inadequacy of water supply", "Lack of Environmental Resilience", "Lack of Structural Resilience Index",
  "Lack of crop land", "Lack of resilience of the agricultural system", "Lack of Environmental Resilience", "Lack of Structural Resilience Index",
  "Low tree cover", "Lack of resilience to heat shocks", "Lack of Environmental Resilience", "Lack of Structural Resilience Index",
  
  "Dependency ratio", "Demographic pressure", "Lack of Social Resilience", "Lack of Structural Resilience Index",
  "Population density", "Demographic pressure", "Lack of Social Resilience", "Lack of Structural Resilience Index",
  "Low number of people using at least basic sanitation services",  "Ineffective social service provision", "Lack of Social Resilience", "Lack of Structural Resilience Index",
  "Under-5 mortality", "Ineffective social service provision", "Lack of Social Resilience", "Lack of Structural Resilience Index",
  "Low years of schooling", "Ineffective social service provision", "Lack of Social Resilience", "Lack of Structural Resilience Index",
  "Low proportion of seats held by women in national parliaments", "Lack of gender equity", "Lack of Social Resilience", "Lack of Structural Resilience Index",
)

conceptvars <- c(unique(mvi_variables$cat2), unique(mvi_variables$cat1))

rawvars <- mvi_variables$raw_var

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

#------------------overview of 1 and of 2 dimensional versions of the index-----------

mc <- "grey82"                # colour for median annotations
pc <- c("grey70", "blue3") # colours for points and bars
the_caption = "Data from https://www.un.org/ohrlls/mvi, analysis by freerangestats.info"

p0 <- mvi |>
  filter(is_pict == "Pacific Island") |>
  bind_rows(tibble(Country = "Other countries", `MVI - Score` = 0)) |>
  mutate(Country = fct_reorder(Country, `MVI - Score`)) |>
  ggplot(aes(x = `MVI - Score`, y = Country)) +
  geom_segment(xend = 0, aes(yend = Country), colour = pc[2]) +
  geom_point(aes(colour = Country), size = 3) +
  geom_text(data = filter(mvi, is_pict != "Pacific Island"), colour = mc, y = 1, label = "|") +
  geom_vline(xintercept = median(mvi$`MVI - Score`), colour = mc, linewidth = 2, alpha = 0.5) +
  annotate("text", x = 51.5, y = 13.41, label = "World median", colour = mc, hjust = 1) +
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank()) +
  scale_colour_manual(values = rep(c("transparent", pc[2]), times = c(1, 100))) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(mvi$`MVI - Score`))) +
  labs( y = "",
        title = "Multidimensional Vulnerability Index",
        subtitle = "Pacific Island countries compared to all developing countries",
        caption = the_caption)

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
       y = "Lack of of structural resilience",
       title = "The two indexes that make up the Multidimensional Vulnerability Index",
       subtitle = "Highlighting Pacific Island countries, and selected other countries with high levels of vulnerability",
       caption = the_caption) +
  scale_colour_manual(values = pc) +
  theme(legend.position = "none",
        panel.grid = element_blank(), 
        panel.border = element_blank())

#------------------plots of the concepts and original variables--------

# a lot of shared code coming up so put a lot of stuff into one function:
group_plot <- function(comp_trans){
  p <- comp_trans |>
    group_by(country2, variable, type, is_pict) |>
    summarise(value = median(value, na.rm = TRUE)) |>
    ungroup() |>
    arrange(variable) |>
    mutate(country2 = fct_reorder(country2, value),
           variable = str_wrap(variable, 30),
           variable = factor(variable, levels = unique(variable)),
           country2 = fct_relevel(country2, "Median non-Pacific")) |>
    ggplot(aes(x = value, y = country2, fill = is_pict)) +
    facet_wrap(~variable) +
    geom_col() +
    scale_fill_manual(values = pc) +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(), 
          panel.border = element_blank(),
          strip.text = element_text(size = rel(0.8), face = "plain", colour = "grey20"),
          axis.ticks = element_blank(),
          strip.background = element_rect(fill = "white")) +
    labs(subtitle = "One of the two indexes making up the Multidimensional Vulnerability Index",
         x = "", y = "", caption = the_caption)
  return(p)
}


p2 <- components |>
  filter(type == "Structural vulnerability") |>
  filter(raw_var) |>
  mutate(variable = factor(variable, levels = rawvars)) |>
  group_plot() +
  labs(title = "Original variables making up the Structural Vulnerability Index")

p3 <- components |>
  filter(type != "Structural vulnerability") |>
  filter(raw_var) |>
  mutate(variable = factor(variable, levels = rawvars)) |>
  group_plot() +
  labs(title = "Original variables making up the Lack of Structural Resilience Index")


p4 <- components |>
  filter(type == "Structural vulnerability") |>
  filter(!raw_var) |>
  mutate(variable = factor(variable, levels = conceptvars)) |>
  group_plot() +
  labs(title = "Constructed concepts making up the Structural Vulnerability Index")

p5 <- components |>
  filter(type != "Structural vulnerability") |>
  filter(!raw_var) |>
  mutate(variable = factor(variable, levels = conceptvars)) |>
  group_plot() +
  labs(title = "Constructed concepts making up the Lack of Structural Resilience Index")

svg_png(p0, "../img/0255-overall-index", w = 8, h = 5)
svg_png(p1, "../img/0255-two-indexes", w = 8, h = 7)
svg_png(p4, "../img/0255-sv-concepts", w = 11, h = 8)
svg_png(p5, "../img/0255-lsr-concepts", w = 10, h = 8)
svg_png(p2, "../img/0255-sv-variables", w = 10, h = 9)
svg_png(p3, "../img/0255-lsr-variables", w = 10, h = 9)

#-----------------------network graph to show how it connects-------------------
mvi_variables
ff <- "Roboto"

mvi_nodes <- bind_rows(
  tibble(name = mvi_variables$raw_var, level = "Original indicator"),
  tibble(name = mvi_variables$cat1, level = "First level concept"),
  tibble(name = mvi_variables$cat2, level = "Second level concept"),
  tibble(name = mvi_variables$cat3, level = "Index"),
  tibble(name= "Multidimensional Vulnerability Index", level = "Final index")
  ) |>
  distinct() |>
  mutate(name_wrap = str_wrap(name, 35),
         level = factor(level, levels = unique(level)))

mvi_edges <- bind_rows(
  tibble(from = mvi_variables$raw_var, to = mvi_variables$cat1),
  tibble(from = mvi_variables$cat1, to = mvi_variables$cat2),
  tibble(from = mvi_variables$cat2, to = mvi_variables$cat3),
  tibble(from = unique(mvi_variables$cat3), to = "Multidimensional Vulnerability Index")
) |>
  distinct() |>
  mutate(from = factor(from, levels = mvi_nodes$name),
         to = factor(to, levels = mvi_nodes$name),
         from = as.numeric(from),
         to = as.numeric(to))

set.seed(122)
mvi_graph <- tbl_graph(nodes = mvi_nodes, edges = mvi_edges)

g1 <- ggraph(mvi_graph, layout = "fr") +
  geom_edge_link(colour = "grey50") +
  geom_node_point(colour = "black") +
  geom_node_label(aes(label = name_wrap, colour = level), fill = "black",
                  label.size = 0, size = 3.5, alpha = 0.5) +
  scale_colour_brewer(palette = "RdYlBu") +
  theme(panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        panel.border = element_blank(),
        text = element_text(family = "Roboto", colour = "grey85"),
        legend.position = c(0.55, 0.65),
        legend.background = element_rect(fill = "grey10"),
        legend.text = element_text(colour = "grey85"),
        legend.key = element_rect(fill = "black")) +
  guides(colour = guide_legend(override.aes = list(alpha=1))) +
  labs(title = "Construction of the Multidimensional Vulnerability Index",
       colour = "",
       caption = the_caption)

svg_png(g1, "../img/0255-construction", w = 23, h = 13)

#-----------------network graph showing one country's results at a time------------------

combined <- mvi |>
  select(Country, 
         `Multidimensional Vulnerability Index` = `MVI - Score`, 
         `Structural Vulnerability Index` = svi, 
         `Lack of Structural Resilience Index` = lsri) |>
  gather(variable, value, -Country) |>
  rbind(select(components, Country, variable, value))

for(this_country in picts){
  this_country_mvi <- combined |>
    filter(Country == this_country) |>
    select(variable, value)
  
  mvi_nodes2 <- mvi_nodes |>
    left_join(this_country_mvi, by = c("name" = "variable")) |>
    mutate(name_with_val = glue("{name_wrap}: {round(value)}"))
  
  
  set.seed(122)
  mvi_graph2 <- tbl_graph(nodes = mvi_nodes2, edges = mvi_edges)
  
  g2 <- ggraph(mvi_graph2, layout = "fr") +
    geom_edge_link(colour = "grey50") +
    geom_node_point(aes(colour = value), size = 8) +
    geom_node_label(aes(label = name_with_val), fill = "black", nudge_y = -0.3,
                    label.size = 0, size = 3.5, alpha = 0.2, colour = "white") +
    scale_colour_viridis_c(option = "C", direction = 1, limits = c(0, 100)) +
    theme(panel.background = element_rect(fill = "black", colour = "black"),
          plot.background = element_rect(fill = "black", colour = "black"),
          panel.border = element_blank(),
          text = element_text(family = "Roboto", colour = "grey85"),
          legend.position = c(0.55, 0.65),
          legend.background = element_rect(fill = "grey10"),
          legend.text = element_text(colour = "grey85"),
          legend.key = element_rect(fill = "black")) +
    guides(colour = guide_legend(override.aes = list(alpha=1))) +
    labs(title = glue("What drives the score on the Multidimensional Vulnerability Index - {this_country}"),
         colour = "Higher scores means\nmore vulnerable",
         caption = the_caption)
  
  svg_png(g2, glue("../img/0255-mvi-{this_country}"), w = 23, h = 13)
}
