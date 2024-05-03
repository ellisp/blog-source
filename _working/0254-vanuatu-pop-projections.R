


library(tidyverse)
library(glue)

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
    PopF = PopF
  ))
}



un_proj <- function(the_country){
  
  if(!the_country %in% unique(indicators$Location)){
    stop("Country not found")
  }
  
  this_fert <- fert_all |>
    filter(Location == the_country & Variant == "Medium") |>
    filter(Time %in% 2020:2100) |>
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
  
  this_mort <- mort_all |>
    filter(Location == the_country & Variant == "Medium") |>
    filter(Time %in% 2020:2100) |>
    # next step important because we will be sorting by AgeGrp
    mutate(AgeGrp = case_when(
      AgeGrp == "100+" ~ 100,
      TRUE ~ suppressWarnings(as.numeric(as.character(AgeGrp)))
    ))
    
  this_mort_m <- this_mort |>
    select(Time, AgeGrp, DeathMale) |>
    mutate(Time = as.character(Time)) |>
    # deaths per unit, not per thousand:
    mutate(DeathMale = DeathMale / 1000) |>
    pivot_wider(id_cols = AgeGrp, names_from = Time, values_from = DeathMale) |>
    arrange(AgeGrp) |>
    select(-AgeGrp) |>
    as.matrix()
  
  this_mort_f <- this_mort |>
    select(Time, AgeGrp, DeathFemale) |>
    mutate(Time = as.character(Time)) |>
    # deaths per unit, not per thousand:
    mutate(DeathFemale = DeathFemale / 1000) |>
    pivot_wider(id_cols = AgeGrp, names_from = Time, values_from = DeathFemale) |>
    arrange(AgeGrp) |>
    select(-AgeGrp) |>
    as.matrix()
  
  # check the years are correct, didn't get mangled or reordered
  stopifnot(all(colnames(this_mort_f) == 2020:2100))
  stopifnot(all(colnames(this_mort_m) == 2020:2100))
  
  this_pop <- pop_all |>
    filter(Location == the_country & Variant == "Medium") |>
    filter(Time == 2020) |>
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
    filter(Location == the_country & Time %in% 2020:2100) |>
    arrange(Time) |>
    pull(CNMR) / 1000
  
  # sex ratio at birth
  this_srb <- indicators |>
    filter(Location == the_country & Time %in% 2020:2100) |>
    arrange(Time) |>
    pull(SRB) / 100
  
  this_proj <- pop_proj(
                  start_pop_m = this_pop_m, 
                  start_pop_f = this_pop_f, 
                  start_year = 2020, 
                  end_year = 2100, 
                  fertility = this_fert, 
                  mort_m = this_mort_m,
                  mort_f = this_mort_f,
                  net_migration = this_cnmr,
                  sex_ratio_birth = this_srb
    
  )
  return(this_proj)
}

the_country <- "India"
my_proj <- un_proj(the_country)

projected_pop <- apply(my_proj$PopM, 2, sum) + apply(my_proj$PopF, 2, sum)

comp_data <- indicators |>
  filter(Location == the_country & Variant == "Medium" & Time %in% 2020:2100) |>
  select(Time, UN = TPopulation1Jan) |>
  mutate(New = as.numeric(projected_pop / 1000) )

# First year should be an exact match:
stopifnot(comp_data[1, ]$UN == comp_data[1, ]$New)

comp_data |>
  gather(variable, value, -Time) |>
  ggplot(aes(x = Time, y = value, colour = variable)) +
  geom_line() +
  labs(title = the_country,
       subtitle = "Attempt to re-create the UN population projections from population in 2020, fertility and mortality rates")

# My projection too high: Samoa, Vanuatu, Fiji, Tonga, Australia, Brazil, New Zealand, Egypt, Georgia, France, Germany,
# Israel, Peru, Mexico, Canada (except at very end), Palau
# My projection too low: India, China, United States of America

#-----------------------thinking about migration----------------

# ok for Vanuatu so there's an assumption of net zero migration it seems, for the forecast period
# but that's not the case for other countries:
indicators |>
  filter(Location == the_country) |>
  ggplot(aes(x = Time, y = CNMR)) +
  geom_line() +
  geom_vline(xintercept = 2022, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(title = the_country,
       subtitle = "Net migration per thousand people")
