#===============Setup==================
library(tidyverse)
library(rsdmx)
library(janitor)
library(glue)
library(ggbiplot)
library(WDI)
library(ggrepel)

#' Collapse a character vector
#' 
#' @param ... passed through to \code{paste()}
#' @details
#' this is just a convenience wrapper around paste()
#' 
pastec <- function(...){
  paste(..., collapse = ', ')
}

#===============Part 1 - life expectancy compared to health spend============
#---------------life expectancy--

# had to explore a bit to find the indicators to use
# w <- WDIsearch("health")
# View(w)

h <- WDI(indicator = c(health_spend = "SH.XPD.CHEX.PC.CD", 
                       life_exp = "SP.DYN.LE00.IN"),
         start = 2020) 
h  |>
  filter(!(iso2c %in% c("ZW", "ZM", "ZA", "XK") | 
             !(grepl("^[XZ]", iso2c) |
             iso2c %in% c("EU", "V1", "V2", "V3", "V4", "OE")))) |>
  distinct(country, iso2c)


h2 <- h  |>
  # reove regions and groupings (which have ISO2 code beginning with Z or X
  # but leave in 4 countries with real codes beginning Z or X)
  # and also knock out some specific things like EU, OECD average, etc
  filter((iso2c %in% c("ZW", "ZM", "ZA", "XK") | 
            !(grepl("^[XZ]", iso2c) |
                iso2c %in% c("EU", "V1", "V2", "V3", "V4", "OE")))) |>
  drop_na() |>
  group_by(country) |>
  arrange(desc(year)) |>
  # originally i picked the latest year for each country, but in so many
  # cases it was 2021 I decided to just force it to be 2021
  filter(year == 2021) |>
  slice(1)  |>
  ungroup()

# going ot use this model to identify 'outlier' countries worth labelling
mod <- lm(life_exp ~ log(health_spend), data = h2)

# add the residuals of hte model and label any countries not from 2021 (this
# label change was more necessary when I had multiple years in the data, but
# I've left it in for future reference)
h3 <- h2 |>
  mutate(res = as.numeric(residuals(mod)),
         country_yr = ifelse(year == 2021, 
                             country, glue("{country}, {year}")))

# Draw scatter plot
p1 <- h3 |> 
  ggplot(aes(x = health_spend, y = life_exp)) +
  geom_smooth(method = "lm", colour = "lightgreen", se = FALSE) +
  geom_point() +
  # label some interesting countries:
  geom_text_repel(data = 
                    filter(h3,
                           health_spend > 8500 | 
                             life_exp > 84 | 
                             life_exp < 55 |
                             res < -10 |
                             res > 7),
                  aes(label = country_yr,
                      # annoying ggrepel couldn't put Norway in a good position automatically
                      # so I am going to move it ad hoc:
                      x = ifelse(country == "Norway", health_spend * 1.189, health_spend)),
                  colour = "steelblue", 
                  seed = 125,
                  size = 2.8) +
  scale_x_log10(label = dollar) +
  labs(x = "Health expenditure per capita (US dollars, logarithmic scale)",
       y = "Life expectancy",
       title = "Health expenditure is associated with higher life expectancy",
       subtitle = "USA is a high income outlier, but many poorer countries also have poor life expectancy given health spend per person.",
       caption = "Source: data for 2021 from the World Development Indicators; analysis by freerangestats.info")

svg_png(p1, "../img/0285-life-exp-scatter", w = 9, h = 6.5)


#===============Part 2 - comparing various cause of death numbers=========

#--------------metadata for causes of death------------------
md <- readSDMX("https://sdmx.oecd.org/public/rest/dataflow/OECD.ELS.HD/DSD_HEALTH_STAT@DF_COM/1.0?references=all")

# English names for each of the code lists
sapply(md@codelists@codelists, \(x)x@Name$en)
# noting number 7 is Cause of death and 2 is area ie country

#' convenience function for extracting codelists from sdmx metadata
#' 
#' This is not very robust but does the job for today
extract_codes <- function(metadata, id, description_name = "description"){
  codes <- md@codelists@codelists[[id]]@Code
  lookup <- tibble(code = sapply(codes, \(x)x@id), 
         description = sapply(codes, \(x)unlist(x@name)))
  names(lookup)[2] <- description_name
  return(lookup)
}


cod_codes <- extract_codes(md, 7, "cause_of_death") 
area_codes <- extract_codes(md, 2, "country") |>
  # country is still a list within a list so need to extract further
  mutate(country = as.character(sapply(country, \(x){x['en']}))
)

# the codeelists Code should have parentCode to show the hierarchical nature
# of the cause of death codes, but it doesn't seem to be there
sapply(md@codelists@codelists[[7]]@Code, \(x)x@parentCode)

# you are meant to be able to get parent codes from this but they all look to be
# NA (see above( so I couldn't see how to do this. So instead I've made a list
# by hand of all those at the top level of hte classification
high_level_cod <- c(
  "Certain infectious and parasitic diseases",
  "Neoplasms",
  "Diseases of the blood and blood-forming organs",
  "Endocrine, nutritional and metabolic diseases",
  "Mental and behavioural disorders",
  "Diseases of the nervous system" ,
  "Diseases of the respiratory system" ,
  "Diseases of the circulatory system" ,
  "Diseases of the digestive system",
  "Diseases of the skin and subcutaneous tissue",
  "Diseases of the musculoskeletal system and connective tissue" ,
  "Diseases of the genitourinary system"  ,
  "Certain conditions originating in the perinatal period",
  "Congenital malformations, deformations and chromosomal abnormalities",
  "Symptoms, signs, ill-defined causes" ,
  "External causes of mortality",
  "Codes for special purposes: COVID-19" 
)

# The actual death rates (standardised by age)
death_rates <- readSDMX("https://sdmx.oecd.org/public/rest/data/OECD.ELS.HD,DSD_HEALTH_STAT@DF_COM,1.0/.A..DT_10P5HB.._T...STANDARD....?startPeriod=2015&dimensionAtObservation=AllDimensions") |>
  as_tibble() |>
  clean_names() |>
  left_join(cod_codes, by = c("death_cause" = "code")) |>
  left_join(area_codes, by = c("ref_area" = "code"))

# A problem, its different sets of countries for each year
count(death_rates, time_period)

death_rates |>
  count(country, time_period) |>
  count(country, sort = TRUE, name = "number_years") |>
  count(number_years, name = "number_countries")

death_rates |>
  count(country, time_period) |>
  count(time_period, sort = TRUE, name = "number_countries") 


#' Draw a bar chart for one country showing its death rates in comparison to an average
bar_one_country <- function(the_country, 
                            comparison_countries = NULL,
                            subtitle = NULL,
                            st_width = 120,
                            years = 2017:2019,
                            bar_fill = c("country_average", "this_country")){
  
  bar_fill <- match.arg(bar_fill)
  
  death_rates2 <- death_rates |>
    filter(time_period %in% years)
  
  if(!the_country %in% death_rates2$country){
    stop(glue("Could not find {the_country} in {paste(years, collapse = ', ')}"))
  }
  
  
  if(is.null(comparison_countries)){
    
  } else {
    check <- comparison_countries %in% death_rates2$country
    if(!all(check)){
      warning(glue("Could not find {pastec(comparison_countries[!check])}"))
    }
    
    death_rates2 <- death_rates2 |>
      filter(country %in% c(the_country, comparison_countries))
    
    if(is.null(subtitle)){
      subtitle <- glue("Comparison: {pastec(comparison_countries[check])}")
    }
  }
  

  
  # some countries won't have data for all years
  bad_countries <- death_rates2 |>
    count(country, time_period)|>
    count(country) |>
    filter(n != length(years)) |>
    pull(country)
  
  if(length(bad_countries) > 0){
    warning(glue("Dropping {pastec(bad_countries)} as missing some years"))
    if(the_country %in% bad_countries){
      stop("This is fatal for drawing this graph...")
    }
    death_rates2 <- death_rates2 |>
      filter(!country %in% bad_countries)
    
    comparison_countries <- comparison_countries[!comparison_countries %in% bad_countries]
  }
  
  one_wide <- death_rates2 |>
    filter(cause_of_death %in% high_level_cod) |>
    mutate(cause_of_death = case_when(
      cause_of_death == "Diseases of the nervous system" ~ "Diseases of the nervous system (includes Alzheimer's and Parkinson's)",
      cause_of_death == "Mental and behavioural disorders" ~ "Mental and behavioural disorders (includes Dementia)",
      cause_of_death == "External causes of mortality" ~ "External causes of mortality (includes accidents, assaults and self-harm)",
      cause_of_death == "Neoplasms" ~ "Neoplasms (eg cancer)",
      TRUE ~ cause_of_death
    )) |>
    group_by(cause_of_death, time_period) |>
    mutate('country_average' = mean(obs_value)) |> 
    ungroup() |>
    filter(country == the_country) |>
    select(cause_of_death, time_period, one_country = obs_value, 'country_average') |>
    mutate(time_period = as.numeric(time_period)) |>
    mutate(cause_of_death = fct_reorder(str_wrap(cause_of_death, 30), one_country)) |>
    group_by(cause_of_death) |>
    mutate(comp_diff = mean(one_country) - mean(country_average),
           country_average = mean(country_average)) |>
    ungroup()
  
  p <- one_wide |>
    distinct(cause_of_death, comp_diff, country_average) |>
    mutate(cause_of_death = fct_reorder(str_wrap(cause_of_death, 40), comp_diff)) |>
    ggplot(aes(x = comp_diff, y = cause_of_death)) +
    scale_fill_viridis_c() +
    labs(x = glue("Age standardised deaths per 100,000 in {the_country} compared to unweighted average of {length(unique(death_rates2$country))} countries"),
         y = "",
         caption = "Source: OECD. Analysis by freerangestats.info",
         title = glue("Where do extra deaths in {the_country} come from in {pastec(years)}?"),
         subtitle = str_wrap(subtitle, width = st_width))
  
  if(bar_fill == "this_country"){
    p <- p +
      geom_col(aes(fill = comp_diff)) +
      labs(fill = glue("Rate in {the_country}"))
    
  } else {
    p <- p +
      geom_col(aes(fill = country_average)) +
      labs(fill = "All-country average")
    
  }
  
  return(p)
}


p2 <- bar_one_country("United States", bar_fill = "country_average",
                subtitle = "Comparison to all countries with available cause of death rates.")

p3 <- bar_one_country("United States", years = 2021, 
                      comparison_countries = unique(death_rates$country))

svg_png(p2, "../img/0285-us-1719", w = 11, h = 8)
svg_png(p3, "../img/0285-us-21", w = 11, h = 8)

Cairo::CairoPDF("../img/0285-all-countries-2017-2019.pdf", 
                width = 11, height = 8, title = "Cause of deaths 2017-2019")
for(tc in sort(unique(death_rates$country))){
  try(print(bar_one_country(tc, bar_fill = "country_average")))
}
dev.off()

Cairo::CairoPDF("../img/0285-all-countries-2021.pdf", 
                width = 11, height = 8, title = "Cause of deaths 2021")
for(tc in sort(unique(death_rates$country))){
  try(print(bar_one_country(tc, years = 2021, bar_fill = "country_average")))
}
dev.off()


# note that assault, suicide etc impact on young people
# so they disproportionately impact life expectancy
# see https://www.aamcresearchinstitute.org/our-work/data-snapshot/narrowing-gap

# to look into - those resipiratory and other diseases
# will probably be worth making a proper classification


#----------comparisons with a smaller set of countries--------------

better_countries <- h2 |>
  filter(life_exp > h2[h2$country == "United States", ]$life_exp) |>
  mutate(country = ifelse(grepl("Korea, Rep.", country, fixed = TRUE), "Korea", country)) |>
  pull(country)

p4 <- bar_one_country("United States", 
                comparison_countries = better_countries, 
                years = 2017:2019)

p5 <- bar_one_country("United States", 
                comparison_countries = better_countries, 
                years = 2021)

p6 <- bar_one_country("Australia", 
                comparison_countries = better_countries, 
                years = 2022)

p7 <- bar_one_country("Estonia", 
                comparison_countries = better_countries, 
                years = 2022)

svg_png(p4, "../img/0285-us-1719-lim", w = 11, h = 8)
svg_png(p5, "../img/0285-us-21-lim", w = 11, h = 8)
svg_png(p6, "../img/0285-aus-22-lim", w = 11, h = 8)
svg_png(p7, "../img/0285-est-22-lim", w = 11, h = 8)


#-----------biplot--------

cod_biplot <- function(years, cod_col = "steelblue", min_loading = 0.25, ...){
  all_countries <- death_rates |>
    filter(cause_of_death %in% high_level_cod &
             time_period %in% years) |>
    group_by(country, cause_of_death) |>
    summarise(value = sum(obs_value)) |>
    spread(cause_of_death, value, fill = 0)
  
  all_countries_m <- as.matrix(all_countries[ ,-1])
  #all_countries_m <- t(scale(t(all_countries_m)))
  
  row.names(all_countries_m) <- pull(all_countries, 1)
  colnames(all_countries_m) <- gsub("Diseases of the ", "", colnames(all_countries_m))
  colnames(all_countries_m) <- gsub("Certain ", "", colnames(all_countries_m))
  #colnames(all_countries_m) <- gsub(" system", "", colnames(all_countries_m))
  
  pc <- princomp(all_countries_m)
  select_cols <- loadings(pc)[, 1:2] |>
    as_tibble() |>
    mutate(cn = row.names(loadings(pc)[,1:2])) |>
    mutate(cn = ifelse(abs(Comp.1) > min_loading | abs(Comp.2) > min_loading, cn , "")) |>
    pull(cn)
  
  colnames(all_countries_m) <- select_cols
  pc <- princomp(all_countries_m)
  
  
  p <- ggbiplot(pc, labels = row.names(all_countries_m),
           varname.color = cod_col, ...) +
    labs(subtitle = glue("Cause of death by country: {pastec(years)}"))
  
  return(p)
}

p8 <- cod_biplot(2021)
p9 <- cod_biplot(2020, min_loading = 0.1)
p10 <- cod_biplot(2017:2019, min_loading = 0.3)

svg_png(p8, "../img/0285-biplot-21", w = 9, h = 6)
svg_png(p9, "../img/0285-biplot-20", w = 9, h = 6)
svg_png(p10, "../img/0285-biplot-1719", w = 9, h = 6)

#---------not used------------
# time series chart
us_narrow <- us_wide |>
  gather(country, value, -cause_of_death, -time_period, -us_diff)

us_narrow |>
  ggplot(aes(x = time_period)) +
  geom_line(aes(y = value, colour = country)) +
  geom_ribbon(data = us_wide, aes(ymin = USA, ymax = `OECD average`, fill = us_diff)) +
  facet_wrap(~cause_of_death, scales = "fixed") +
  scale_colour_manual(values = c("USA" = "red", 
                                 "OECD average" = "grey")) +
  scale_fill_viridis_c()



# bonus charts
# following would be an error, at least as at 12/12/2024:
try(bar_one_country("United States", years = 2022))

# couple of trial charts won't use
# drops New Zealand:
bar_one_country("United States", c("Australia", "New Zealand", "Germany", "Finland"))

# first year NZ appears is 2016:
bar_one_country("United States", c("Australia", "New Zealand", "Germany", "Finland"), years = 2016)

bar_one_country("United States", 
                comparison_countries = better_countries, 
                st_width = 140,
                years = 2020)

