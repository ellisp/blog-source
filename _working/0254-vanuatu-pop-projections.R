


library(tidyverse)
library(glue)
library(scales)

dir.create("data-pop-proj-2022")

#------------------download and import data for all countries from existing projections----------------
list.files("data-pop-proj-2022")

files <- c("WPP2022_Fertility_by_Age1.zip",
           "WPP2022_DeathsBySingleAgeSex_Medium_1950-2021.zip",
           "WPP2022_DeathsBySingleAgeSex_Medium_2022-2100.zip",
           "WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.zip",
           "WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.zip",
           "WPP2022_Demographic_Indicators_Medium.zip"
           )

# let downloads take up to 10 minutes rather than 1 minute max, as files are
# large (largest is fertility for single age, 78MB)
options(timeout=600)

if(!file.exists("data-pop-proj-2022/WPP2022_Demographic_Indicators_Medium.csv")){
  # download the zip files:
  for(i in 1:length(files)){
    download.file(glue("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/{files[i]}"),
                   destfile = glue("data-pop-proj-2022/{files[i]}"), mode = "wb")
  }
  
  
  # unzip them
  for(i in 1:length(files)){
    unzip(glue("data-pop-proj-2022/{files[i]}"), exdir = "data-pop-proj-2022")
  }
}

fert_all <- read_csv("data-pop-proj-2022/WPP2022_Fertility_by_Age1.csv")
mort_past <- read_csv("data-pop-proj-2022/WPP2022_DeathsBySingleAgeSex_Medium_1950-2021.csv")
mort_future <- read_csv("data-pop-proj-2022/WPP2022_DeathsBySingleAgeSex_Medium_2022-2100.csv")
pop_past <- read_csv("data-pop-proj-2022/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.csv")
pop_future <- read_csv("data-pop-proj-2022/WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.csv")
indicators <-   read_csv("data-pop-proj-2022/WPP2022_Demographic_Indicators_Medium.csv")

mort_all <- rbind(mort_past, mort_future)
pop_all <- rbind(pop_past, pop_future)

# clean up
rm(mort_past, mort_future, pop_past, pop_future)

#----------------------projections for one country-------------------

# adapting the implementation of the method for cohort component projection at 
# https://farid-flici.github.io/tuto.html


#' @param start_pop_m is a vector of population of males at single year age
#'   periods beginning of year starting age 0
#' @param start_pop_f as start_pop_m but for females
#' @param start_year first year ie the year for which the actual population
#'   number refers to
#' @param end_year end year for the population projection
#' @param fertility a matrix with rows for female agegroups 15 to 49 and a
#'   column for each year, values are births per woman (not per thousand women)
#'   that year and age. Must have 35 rows and number of columns equal to
#'   end_year minus start_year plus one.
#' @param mort_m a matrix with rows for age 0 to at least 50 and columns for
#'   each year, for males. Must have number of columns equal to end_year minus
#'   start_year plus one.
#' @param mort_f as mort_m but for females. Must have same number of rows and
#'   columns as mort_m.
#' @param sex_ratio_birth number of boys born for every girl born, vector with
#'   length of years to be projected.
pop_proj <- function(start_pop_m, 
                            start_pop_f, 
                            start_year, 
                            end_year, 
                            fertility, 
                            mort_m,
                            mort_f,
                            sex_ratio_birth = rep(1.045, end_year - start_year + 1),
                            net_migration = rep(0, end_year - start_year + 1)){
  
  stopifnot(nrow(fertility) == 35)
  
  ncols <- end_year - start_year + 1
  
  stopifnot(ncol(fertility) == ncols)
  stopifnot(ncol(mort_m) == ncols)
  stopifnot(ncol(mort_f) == ncols)
  stopifnot(length(start_pop_m) > 50)
  stopifnot(length(start_pop_m) < 120)
  stopifnot(length(start_pop_f) > 50)
  stopifnot(length(start_pop_f) < 120)
  stopifnot(length(net_migration) == ncols)
  stopifnot(length(sex_ratio_birth) == ncols)
  
  stopifnot(nrow(mort_m) == nrow(mort_f))
  # fill in with high probability of death for ages beyond where we have mortality data
  if(nrow(mort_m) < 121){
    extra_deaths_m <- matrix(max(mort_m), nrow = 121 - nrow(mort_m), ncol = ncol(mort_m))
    extra_deaths_f <- matrix(max(mort_f), nrow = 121 - nrow(mort_f), ncol = ncol(mort_f))
    
    mort_m <- rbind(mort_m, extra_deaths_m)
    mort_f <- rbind(mort_f, extra_deaths_f)
  }
  
  # Create matrices for male and female population for the full projections
  # each row of the matrix is an age group, each column is a year
  PopM <- matrix(0, nrow=121, ncol = ncols)
  PopF <- matrix(0, nrow=121, ncol = ncols)
  deaths <- rep(NA, ncols)
  
  rownames(PopF) <- rownames(PopM) <- c(0:120)                # ages
  colnames(PopF) <- colnames(PopM) <- c(start_year:end_year)  # years
  
  # first year gets populated with the actual population numbers that we have:
  PopM[1:length(start_pop_m), 1] <- start_pop_m
  PopF[1:length(start_pop_f), 1] <- start_pop_f
  
  # subsequent years get modified by people aging one year, deaths, births, and net migration
  for (i in 2 : ncols) {
    
    # Age one and above:
    PopM[2:121, i] <- PopM[1:120, i-1] * (1 - mort_m[1:120, i-1])
    PopF[2:121, i] <- PopF[1:120, i-1] * (1 - mort_f[1:120, i-1])
    
    deaths[i-1] <- sum(PopM[, i-1]) - sum(PopM[, i]) + sum(PopF[, i-1]) - sum(PopF[, i])
    
    # Age zero. Fertility rate by the number of women in the middle of the previous year.
    # Note that PopF rows 16:50 equates to women age 15:49
    reproducing_women <- (PopF[16:50, i-1] + PopF[16:50, i]) / 2
    
    prop_boys <- sex_ratio_birth[i] / (1 + sex_ratio_birth[i])          
    prop_girls <- 1 - prop_boys
    
    PopM[1, i] <-  reproducing_women %*% fertility[, i-1] * prop_boys
    
    PopF[1, i] <- reproducing_women %*% fertility[, i-1] * prop_girls 
    
    # migration. This assumes that net migrants have the same age and sex profile as
    # the overall population, which seems unlikely, but not obvious where to
    # get the age profile of net migrants from the published data.
    PopM[, i] <- PopM[, i] * (net_migration[i] + 1)
    PopF[, i] <- PopF[, i] * (net_migration[i] + 1)
  }
  
  # Return a list of the two matrices
  return(list(
    PopM = PopM,
    PopF = PopF,
    deaths = deaths
  ))
}



repeat_un_proj <- function(the_country, the_years = 2020:2100){
  
  if(!the_country %in% unique(indicators$Location)){
    stop("Country not found")
  }
  
  this_fert <- fert_all |>
    filter(Location == the_country & Variant == "Medium") |>
    filter(Time %in% the_years) |>
    # Age-Specific Fertility rate
    select(Time, AgeGrp, ASFR) |>
    mutate(Time = as.character(Time),
           # turn into proportions, not rates per 1000:
           ASFR = ASFR / 1000) |>
    pivot_wider(id_cols = AgeGrp, names_from = Time, values_from = ASFR) |>
    arrange(AgeGrp) |>
    select(-AgeGrp) |>
    as.matrix()
  
  # should be 35 rows ie fertilities for ages 15 to 49
  stopifnot(nrow(this_fert) == 35)
  # should be 81 columns, 1 column for each year from 2020 to 2100
  stopifnot(ncol(this_fert) == 81)
  
  
  # Mortality is in numbers not a ratio so we need to join to the population data to turn it into a ratio
  pop_years <- pop_all |>
    filter(Location == the_country & Variant == "Medium" & Time %in% the_years) |>
    select(Time, PopMale, PopFemale, AgeGrp)

  this_mort <- mort_all |>
    filter(Location == the_country & Variant == "Medium") |>
    filter(Time %in% the_years) |>
    left_join(pop_years, by = c("Time", "AgeGrp")) |>
    # next step important because we will be sorting by AgeGrp
    mutate(AgeGrp = case_when(
      AgeGrp == "100+" ~ 100,
      TRUE ~ suppressWarnings(as.numeric(as.character(AgeGrp)))
    ))
  
    
    
  this_mort_m <- this_mort |>
    # sometimes more deaths than people (eg 1 death, 0 people) so cap the death ratio at 1
    mutate(DeathMale = pmin(1, DeathMale / PopMale)) |>
    select(Time, AgeGrp, DeathMale) |>
    mutate(Time = as.character(Time)) |>
    pivot_wider(id_cols = AgeGrp, names_from = Time, values_from = DeathMale) |>
    arrange(AgeGrp) |>
    select(-AgeGrp) |>
    as.matrix()
  
  this_mort_f <- this_mort |>
    mutate(DeathFemale = pmin(1, DeathFemale / PopFemale)) |>
    select(Time, AgeGrp, DeathFemale) |>
    mutate(Time = as.character(Time)) |>
    pivot_wider(id_cols = AgeGrp, names_from = Time, values_from = DeathFemale) |>
    arrange(AgeGrp) |>
    select(-AgeGrp) |>
    as.matrix()
  
  # check the years are correct, didn't get mangled or reordered
  stopifnot(all(colnames(this_mort_f) == the_years))
  stopifnot(all(colnames(this_mort_m) == the_years))
  
  this_pop <- pop_all |>
    filter(Location == the_country & Variant == "Medium") |>
    filter(Time == min(the_years)) |>
    # next step important because we will be sorting by AgeGrp
    mutate(AgeGrp = case_when(
      AgeGrp == "100+" ~ 100,
      TRUE ~ suppressWarnings(as.numeric(as.character(AgeGrp)))
    )) |>
    arrange(AgeGrp) 
  
  # convert to units, not thousands of people:
  this_pop_m <- this_pop$PopMale * 1000
  this_pop_f <- this_pop$PopFemale * 1000
  
  # reality check
  # Population in millions; should be about 0.3 if the_country is Vanuatu, about 1400 if India:
  (sum(this_pop_m) + sum(this_pop_f) ) / 1e6
  
  # net migration
  this_cnmr <- indicators |>
    filter(Location == the_country & Time %in% the_years) |>
    arrange(Time) |>
    pull(CNMR) / 1000
  
  # sex ratio at birth
  this_srb <- indicators |>
    filter(Location == the_country & Time %in% the_years) |>
    arrange(Time) |>
    pull(SRB) / 100
  
  this_proj <- pop_proj(
                  start_pop_m = this_pop_m, 
                  start_pop_f = this_pop_f, 
                  start_year = min(the_years), 
                  end_year = max(the_years), 
                  fertility = this_fert, 
                  mort_m = this_mort_m,
                  mort_f = this_mort_f,
                  net_migration = this_cnmr,
                  sex_ratio_birth = this_srb
    
  )
  return(list(un_proj = this_proj,
         un_pop_m = this_pop_m,
         un_pop_f = this_pop_f,
         un_fert = this_fert,
         un_mort_m = this_mort_m,
         un_mort_f = this_mort_f,
         un_cnmr = this_cnmr,
         un_srb = this_srb))
}


#------------comparisons------------

the_country <- "Vanuatu"
my_proj <- repeat_un_proj(the_country)$un_proj



# total population
comp_data <- indicators |>
  filter(Location == the_country & Variant == "Medium" & Time %in% 2020:2100) |>
  select(Time, `UN original` = TPopulation1Jan) |>
  mutate(`Reproduction` = as.numeric(apply(my_proj$PopM, 2, sum) + apply(my_proj$PopF, 2, sum)) / 1000) 

# First year should be an exact match:
stopifnot(comp_data[1, ]$UN == comp_data[1, ]$Reproduction)


p1 <- comp_data |>
  gather(variable, value, -Time) |>
  ggplot(aes(x = Time, y = value, colour = variable)) +
  geom_line() +
  scale_y_continuous(label = comma) +
  labs(title = the_country,
       subtitle = "Attempt to re-create the UN population projections from population in 2020, fertility and mortality rates",
       y = "Population", x = "", colour = "")

svg_png(p1, "../img/0254-vanuatu-pop")
# svg_png(p1, "../img/0254-Australia-pop")

# births
p2 <- indicators |>
  filter(Location == the_country & Variant == "Medium" & Time %in% 2020:2100) |>
  select(Time, `UN original` = Births) |>
  mutate(Reproduction = as.numeric(my_proj$PopM[1,] + my_proj$PopF[1, ]) / 1000) |>
  gather(variable, value , -Time)  |>
  ggplot(aes(x = Time, y = value, colour = variable)) +
  geom_line() +
  labs(title = the_country,
       subtitle = "Attempt to re-create the UN population projections from population in 2020, fertility and mortality rates",
       y = "Population", x = "", colour = "")

svg_png(p2, "../img/0254-vanuatu-births")


# deaths
p3 <- indicators |>
  filter(Location == the_country & Variant == "Medium" & Time %in% 2020:2100) |>
  select(Time, `UN original` = Deaths) |>
  mutate(Reproduction = as.numeric(my_proj$deaths) / 1000) |>
  gather(variable, value , -Time)  |>
  ggplot(aes(x = Time, y = value, colour = variable)) +
  geom_line() +
  labs(title = the_country,
       subtitle = "Attempt to re-create the UN population projections from population in 2020, fertility and mortality rates",
       y = "Population", x = "", colour = "")

svg_png(p3, "../img/0254-vanuatu-deaths")


# some very small differences in Vanuatu which probably come down to something about 1 Jan v 1 July for one or more
# of the rates I'm calculating

# Australia is quite significantly out, too few births. Maybe because the migration
# I am assuming to be same profile as population, whereas UN may have a model
# of more reproductive age people migrating in

#-----------------------thinking about migration----------------

# ok for Vanuatu so there's an assumption of net zero migration it seems, for the forecast period
# but that's not the case for other countries:
p4 <- indicators |>
  filter(Location == the_country) |>
  ggplot(aes(x = Time, y = CNMR)) +
  geom_line() +
  geom_vline(xintercept = 2022, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(title = the_country,
       subtitle = "Net migration per thousand people")

svg_png(p4, "../img/0254-vanuatu-mig")
# svg_png(p4, "../img/0254-australia-mig")

#-----------------changing one or two factors while keeping the rest the same--------------

# Say we had the 2020 Vanuatu census so we knew the real population then, and wanted
# to redo the population projections with everything staying as they were before


# file:///C:/Users/Peter/Downloads/Vanuatu_2020_Census_Analytical_report_Vol_2.pdf
revision_country <- "Vanuatu"
van_orig <- repeat_un_proj(revision_country)

revised_pop_m <- van_orig$un_pop_m * 151597 / sum(van_orig$un_pop_m)
revised_pop_f <- van_orig$un_pop_f * 148422 / sum(van_orig$un_pop_f)

un_cbr_2020 <- (van_orig$un_proj$PopM[1, 2]  + van_orig$un_proj$PopF[1, 2]) / 
  sum(van_orig$un_proj$PopM[, 2], van_orig$un_proj$PopF[, 2]) * 1000

adj_ratio <- 28.2 / un_cbr_2020

revised_fert <- van_orig$un_fert * adj_ratio


revised_proj <- pop_proj(
  start_pop_m = revised_pop_m,
  start_pop_f = revised_pop_f,
  start_year = 2020,
  end_year = 2100,
  fertility = revised_fert,
  mort_m = van_orig$un_mort_m,
  mort_f = van_orig$un_mort_f,
  sex_ratio_birth = van_orig$un_srb,
  net_migration = van_orig$un_cnmr)


projected_pop <- apply(revised_proj$PopM, 2, sum) + apply(revised_proj$PopF, 2, sum)

# total population
comp_data <- indicators |>
  filter(Location == revision_country & Variant == "Medium" & Time %in% 2020:2100) |>
  select(Time, `2022 UN projections` = TPopulation1Jan) |>
  mutate(`Revised with 2020 census` = as.numeric(projected_pop / 1000) )


p5 <- comp_data |>
  filter(Time <= 2050) |>
  gather(variable, value, -Time) |>
  ggplot(aes(x = Time, y = value, colour = variable)) +
  geom_line() +
  labs(title = revision_country,
       subtitle = "Attempt to re-create the UN population projections from population in 2020, fertility and mortality rates",
       y = "Population")

svg_png(p5, "../img/0254-vanuatu-revised")

d <- tibble(value = c( revised_proj$PopM[, 31], revised_proj$PopF[, 31],   
                  van_orig$un_proj$PopM[, 31],  van_orig$un_proj$PopF[, 31]),
       sex = rep(c("Male", "Female", "Male", "Female"), each = 121),
       model = rep(c("Revised projections with 2020 census made in 2024", "UN projections made in 2022"), each = 242),
       age = rep(0:120, 4)) |>
  mutate(agef = fct_reorder(as.character(age), age)) |>
  filter(age < 100)

p6 <- d |>
  filter(sex == "Female") |>
  ggplot(aes(x = value, y = agef)) +
  facet_wrap(~model) +
  geom_col(fill = "brown") +
  geom_col(data = filter(d, sex == "Male"), aes(x = -value), fill = "orange") +
  geom_vline(xintercept = 0, colour = "white") +
  labs(x = "Number of people", y = "",
       title = "Comparison of UN original and revised population projections",
       subtitle = "Population age distribution in 2050") +
  scale_x_continuous(breaks = c(-4000, 0, 4000), labels = c(4000, 0, 4000))

svg_png(p6, "../img/0254-vanuatu-pyramid", h = 8)       
