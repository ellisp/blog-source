


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

# let downloads take 10 minutes rather than 1 minute max, as files are large
# (largest is fertility for single age, 78MB)
options(timeout=600)

# download the zip files:
for(i in 1:length(files)){
  download.file(glue("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/{files[i]}"),
                 destfile = glue("data-pop-proj-2022/{files[i]}"), mode = "wb")
}


# unzip them
for(i in 1:length(files)){
  unzip(glue("data-pop-proj-2022/{files[i]}"), exdir = "data-pop-proj-2022")
}

fert_all <- read_csv("data-pop-proj-2022/WPP2022_Fertility_by_Age1.csv")
mort_past <- read_csv("data-pop-proj-2022/WPP2022_DeathsBySingleAgeSex_Medium_1950-2021.csv")
mort_future <- read_csv("data-pop-proj-2022/WPP2022_DeathsBySingleAgeSex_Medium_2022-2100.csv")
pop_past <- read_csv("data-pop-proj-2022/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.csv")
pop_future <- read_csv("data-pop-proj-2022/WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.csv")
indicators <-   read_csv("data-pop-proj-2022/WPP2022_Demographic_Indicators_Medium.csv")

mort_all <- rbind(mort_past, mort_future)
pop_all <- rbind(pop_past, pop_future)

#----------------------projections for Vanuatu-------------------

# adapting the implementation of the method for cohort component projection at 
# https://farid-flici.github.io/tuto.html


#' @param start_pop_m is a vector of ages at beginning of year starting age 0
#' @param fertility is a matrix with rows for females aged 15 to 49 and a column for each year
pop_proj_no_mig <- function(start_pop_m, 
                            start_pop_f, 
                            start_year, 
                            end_year, 
                            fertility, 
                            mort_m,
                            mort_f,
                            sex_ratio_birth = 1.045){
  
  stopifnot(nrow(fertility) == 35)
  
  
  prop_boys <- sex_ratio_birth / (1 + sex_ratio_birth)          
  prop_girls <- 1 - prop_boys
  ncols <- end_year - start_year + 1
  
  stopifnot(ncol(fertility) == ncols)
  stopifnot(ncol(mort_m) == ncols)
  stopifnot(ncol(mort_f) == ncols)
  stopifnot(length(start_pop_m) > 50)
  stopifnot(length(start_pop_m) < 120)
  stopifnot(length(start_pop_f) > 50)
  stopifnot(length(start_pop_f) < 120)
  
  
  stopifnot(nrow(mort_m) == nrow(mort_f))
  # fill in with ones for years
  if(nrow(mort_m) < 121){
    extra_deaths <- matrix(1, nrow = 121 - nrow(mort_m), ncol = ncol(mort_m))
    mort_m <- rbind(mort_m, extra_deaths)
    mort_f <- rbind(mort_f, extra_deaths)
  }
  
  # Create matrices for male and female population for the full projections
  # each row of the matrix is an age group, each column is a year
  PopM <- matrix(0, nrow=121, ncol = ncols)
  PopF <- matrix(0, nrow=121, ncol = ncols)
  
  rownames(PopF) <- rownames(PopM) <- c(0:120)
  colnames(PopF) <- colnames(PopM) <- c(start_year:end_year)
  
  # first year gets populated with the actual population numbers that we have:
  PopM[1:length(start_pop_m), 1] <- start_pop_m
  PopF[1:length(start_pop_f), 1] <- start_pop_f
  
  # subsequent years get modified by births and deaths and people aging one year
  for (i in 2 : ncols) {
    for (j in 2: 121) {
      PopM[j, i] <- PopM[j-1, i-1] * (1-mort_m[j-1, i-1])
      PopF[j, i] <- PopF[j-1, i-1] * (1-mort_f[j-1, i-1])
    }
    PopM[1, i] <- as.matrix(t(PopF[16:50, i-1] + PopF[16:50, i]) / 2) %*% 
      as.matrix(fertility[, i-1]) * prop_boys
    
    PopF[1, i] <- as.matrix(t(PopF[16:50, i-1] + PopF[16:50, i]) / 2) %*% 
      as.matrix(fertility[, i-1]) * prop_girls 
  }
  
  # Return a list of the two matrices
  return(list(
    PopM = PopM,
    PopF = PopF
  ))
}

van_fert <- fert_all |>
  filter(Location == "Vanuatu" & Variant == "Medium") |>
  filter(Time %in% 2020:2100) |>
  select(Time, AgeGrp, ASFR) |>
  mutate(Time = as.character(Time),
         # turn into proportions, not rates per 1000:
         ASFR = ASFR / 1000) |>
  pivot_wider(id_cols = AgeGrp, names_from = Time, values_from = ASFR) |>
  arrange(AgeGrp) |>
  select(-AgeGrp) |>
  as.matrix()

# should be 35 ie fertilities for ages 15 to 49
stopifnot(nrow(van_fert) == 35)
stopifnot(ncolw(van_fert) == 81)

van_mort <- mort_all |>
  filter(Location == "Vanuatu" & Variant == "Medium") |>
  filter(Time %in% 2020:2100) |>
  # next step important because we will be sorting by AgeGrp
  mutate(AgeGrp = case_when(
    AgeGrp == "100+" ~ 100,
    TRUE ~ as.numeric(as.character(AgeGrp))
  ))
  
van_mort_m <- van_mort |>
  select(Time, AgeGrp, DeathMale) |>
  mutate(Time = as.character(Time)) |>
  pivot_wider(id_cols = AgeGrp, names_from = Time, values_from = DeathMale) |>
  arrange(AgeGrp) |>
  select(-AgeGrp) |>
  as.matrix()

van_mort_f <- van_mort |>
  select(Time, AgeGrp, DeathFemale) |>
  mutate(Time = as.character(Time)) |>
  pivot_wider(id_cols = AgeGrp, names_from = Time, values_from = DeathFemale) |>
  arrange(AgeGrp) |>
  select(-AgeGrp) |>
  as.matrix()


van_pop <- pop_all |>
  filter(Location == "Vanuatu" & Variant == "Medium") |>
  filter(Time == 2020) |>
  # next step important because we will be sorting by AgeGrp
  mutate(AgeGrp = case_when(
    AgeGrp == "100+" ~ 100,
    TRUE ~ as.numeric(as.character(AgeGrp))
  )) |>
  arrange(AgeGrp) 

# convert to units, not thousands of people:
van_pop_m <- van_pop$PopMale * 1000
van_pop_f <- van_pop$PopFemale * 1000

# reality check
length(van_pop_m)
sum(van_pop_m) + sum(van_pop_f) # should be about 300k


van_proj <- pop_proj_no_mig(
                start_pop_m = van_pop_m, 
                start_pop_f = van_pop_f, 
                start_year = 2020, 
                end_year = 2100, 
                fertility = van_fert, 
                mort_m = van_mort_m,
                mort_f = van_mort_f
  
)

van_proj$PopM


#-----------------------adding in migration----------------

indicators |>
  filter(Location == "Vanuatu") |>
  ggplot(aes(x = Time, y = CNMR)) +
  geom_line()
