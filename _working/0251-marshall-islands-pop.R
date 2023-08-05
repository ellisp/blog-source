

# Marshall Islands 2011 Census report is at
# https://microdata.pacificdata.org/index.php/catalog/22/related-materials

# 2021 basic tables are at
# https://sdd.spc.int/news/2023/05/30/marshall-islands-2021-census-report-basic-tables

# 1999 report is at
# https://pacificdata.org/data/dataset/spc_mhl_1999_phc_v01_m/resource/b4c94ed9-4e7a-44f9-a522-dc4fbaf3d91e

library(tidyverse)
library(rsdmx)
library(janitor)
library(demography)
library(readxl)
library(mgcv)
library(patchwork)

#---------population numbers-------------

mip <- tibble(
  year = c(1920, 1925, 1930, 1935, 1958, 1967, 1973, 1980, 1988, 1999, 2011, 2021),
  pop = c(9800, 9644, 10412, 10446, 13928, 18925, 24135, 30873, 43380, 50840, 53158, 42000)
)

mip |>
  ggplot(aes(x = year, y = pop)) +
  geom_line() +
  geom_point()

#------------population pyramid----------------
pyramid_raw <- readSDMX("https://stats-sdmx-disseminate.pacificdata.org/rest/data/SPC,DF_POP_PROJ,3.0/A.MH.MIDYEARPOPEST.F+M.Y00T04+Y05T09+Y10T14+Y15T19+Y40T44+Y45T49+Y60T64+Y65T69+Y70T999+Y55T59+Y50T54+Y35T39+Y30T34+Y25T29+Y20T24?startPeriod=2011&endPeriod=2011&dimensionAtObservation=AllDimensions") |>
  as_tibble()

pyramid <- pyramid_raw |>
  clean_names() |>
  separate(age, into = c("age_from", "age_to"), sep = "T") |>
  mutate(age_from = as.numeric(gsub("Y", "", age_from)),
         age_to = as.numeric(age_to),
         age = paste(age_from, age_to, sep = "-"),
         # change 70-999 to 70+:
         age = gsub("-999", "+", age) ,
         age = fct_reorder(age, age_from),
         age_mid = ifelse(age_to < 70, (age_to + age_from) / 2, 85)) |>
  select(sex, age_from, age_to, age, age_mid, obs_value) |>
  mutate(sex = case_when(
    sex == "F" ~ "Female",
    sex == "M" ~ "Male"
  )) |>
  rename(people = obs_value)

pyramid |>
  spread(sex, people) |>
  ggplot(aes(y = age)) +
  geom_col(aes(x = -Male), fill = "blue") +
  geom_col(aes(x = Female), fill = "purple") +
  geom_vline(xintercept = 0, colour = "white")
  scale_x_continuous(breaks = c(-4000, -2000, 2000, 4000),
                     labels = c("4,000", "2,000", "2,000", "4,000"))
  

  
#--------------fertility rates----------------  
  
  # Table 8.2 of the 2011 report
fert <- tibble(
  age_mid = c(17, 22, 27, 32, 37, 42, 47),
  asfr = c(0.0854, 0.2201, 0.2174, 0.1537, 0.0947, 0.0295, 0.0096),
  aasfr = c(0.0822, 0.2118, 0.2092, 0.1479, 0.0912, 0.0284, 0.0092),
  sex = "Female"
)  
sum(fert$asfr)  

#-----------mortality rates--------------
# 2011 analysis used the West tables from the regional model life tables
# and they use q(2) ie the probability of dying by exact age 2 to choose
# which particular life table from the set of West tables.
# Note when they did this for real they did it for boys and girls
# separately but they only show q(2) for total. 0.0239 is the value
# for q(2) in table 8.5. Which translates to values of a,b,c and t 
# of 1.3062, 5.5677, 0.2962 and 2.36
#
# Note the estimate of q(1) is 0.0346


# download.file("https://www.un.org/en/development/desa/population/publications/pdf/mortality/EMLT/MLT_UN2011_130_1y_complete.xlsx",
#               destfile = "model-life-tables.xlsx", mode = "wb")

mlt_raw <- read_excel("model-life-tables.xlsx", sheet = "Sheet1") |>
  clean_names()

mlt_west <- mlt_raw |>
  filter(family == "West")


# the following is a *much* cruder way of getting a lifetable
# than you ar emeant to follow. See "manual X" for details
# https://unstats.un.org/unsd/demographic/standmeth/handbooks/Manual_X-en.pdf
# But it gets us something that is (very) roughly the right shape
mlt <- mlt_west |>
  filter((sex == "Female" & e0 == 73) |
           (sex == "Male" & e0 == 71))

mlt |>
  ggplot(aes(x = age, y = mx1, colour = sex)) +
  geom_line() +
  labs(title = "Mortality rates implied by 'West' model life table", 
       subtitle = "Life expectancy of 73 for females and 71 for males.")

# note that in a life table, 
# - age specific death rate is _nm_x
# - proportion dying in age interval is _nq_x


#--------model the individual age years-----------
d0 <- rbind(
  select(pyramid, age_mid, people, sex),
  expand.grid(age_mid = 100:110, people = 0, sex = c("Female", "Male"))
)
m1 <- gam(people ~ s(age_mid), data = filter(d0, sex == "Female" & age_mid < 70))
m2 <- gam(people ~ s(age_mid), data = filter(d0, sex == "Male" & age_mid < 70))

d1 <- tibble(age_mid = 0:100, sex = "Female") 
d2 <- tibble(age_mid = 0:100, sex = "Male") 

pyramid2 <- rbind(
  mutate(d1, proj_people = pmax(0, predict(m1, newdata = d1)) / 5),
  mutate(d2, proj_people = pmax(0, predict(m1, newdata = d2)) / 5)) |>
  left_join(pyramid, by = c("age_mid" = "age_from", "sex")) |>
  rename(age_y = age_mid) |>
  fill(age, age_mid.y, .direction = "down") |>
  group_by(age) |>
  mutate(proj_people2 = proj_people * sum(people, na.rm = TRUE) / sum(proj_people)) |>
  select(sex, 
         age = age_y,
         age_mid = age_mid.y,
         people = proj_people2)
  
pyramid2 |>
  spread(sex, people) |>
  ggplot(aes(y = as.factor(age))) +
  geom_col(aes(x = -Male), fill = "blue") +
  geom_col(aes(x = Female), fill = "purple") +
  geom_vline(xintercept = 0, colour = "white") +
  scale_x_continuous(breaks = c(-800, -400, 400, 800),
                   labels = c("800", "400", "400", "800"))

fert |>
  right_join(pyramid2, by = c("sex", "age_mid"))
 
dem_rmi <- pyramid2 |>
  left_join(mlt, by = c("age", "sex")) |>
  left_join(fert, by = c("age_mid", "sex"))

# this is closer to it, still too low (should be 3.3 and 4.0)
dem_rmi |>
  mutate(deaths = people * mx1) |>
  group_by(sex) |>
  summarise(`Crude death rate` = sum(deaths) / sum(people) * 1000)


one_year <- function(d){
  infants <- filter(d, age == 0)  |>
    select(sex, age, qx1)
  
  d2 <- d |>
    arrange(sex, age) |>
    mutate(births = people * aasfr,
           deaths = people * qx1,
           age = age + 1,
           people = people - deaths, 
           qx1 = lead(qx1),
           aasfr = lead(aasfr)) |>
    select(sex, age, people, qx1, aasfr, births)
  
  born <- sum(d2$births, na.rm = TRUE)
  
  d3 <- tibble(sex = c("Female", "Male"),
               age = 0,
               people = c(0.475 * born, 0.525 * born)) |>
    left_join(infants, by = c("sex", "age")) |>
    mutate(aasfr = NA)
  
  d4 <- d2 |>
    select(-births) |>
    rbind(d3) |>
    # knock off the old people
    filter(!is.na(people))
  
  return(d4)
}

new_data <- dem_rmi
pops <- tibble(year = 2012:2021, pop = NA)
for(i in 1:10){
  new_data <- one_year(new_data)
  pops[i, ]$pop <- sum(new_data$people)
}

p3 <- mip |>
  mutate(source = "Census") |>
  rbind(mutate(pops, source = "Projection")) |>
  ggplot(aes(x = year, y = pop, colour = source)) +
  geom_line() +
  geom_point() +
  labs(x = "",
       y = "",
       colour = "",
       title = "Population of Marshall Islands",
       subtitle = "Projection based on no migration and life table as at 2011") +
  scale_y_continuous(label = comma) +
  theme(legend.position = "none")


#-------alternative based on crude birth and death rates--------

crude <- tibble(
  year = c(1988, 1999, 2011),
  cdr = c(8.9, 4.9, 3.7), # Table 8.7 2011 report
  cbr = c(NA, 41.8, 32.1)  # Table 8.3 2011 report
)
conflicts_prefer(dplyr::lag)

crude_l <- crude |>
  gather(variable, value, -year) |>
  group_by(variable) |>
  arrange(variable, year) |>
  mutate(source = "Census",
         growth = (value / lag(value)) ^ (1/(year - lag(year))) - 1)

new_crude <- expand.grid(variable = c("cdr", "cbr"), 
                         year = 2012:2021,
                         source = "Projection")

crude_l2 <- crude_l |>
  bind_rows(new_crude) |>
  arrange(variable, year) |>
  group_by(variable) |>
  mutate(value = if_else(
    is.na(value),
    value[year == 2011] * (1 + growth[year == 2011]) ^ (year - 2011),
    value))

crude_l2

p4 <- crude_l2 |>
  ggplot(aes(x = year, y = value)) +
  geom_line(colour = "grey") +
  geom_point(aes(colour = source), size = 2) +
  facet_wrap(~variable, scales = "free_y") +
  expand_limits(y = 0, x = 2021)

crude_l3 <- crude_l2 |>
  select(year, variable, value) |>
  spread(variable, value) |>
  full_join(rbind(mip, tibble(year = 2021, pop = NA)),
            by = "year") |>
  mutate(source = ifelse(is.na(pop), 
                         "Projection",
                         "Census")) |>
  arrange(year, desc(source))

for(i in 2:nrow(crude_l3)){
  if(is.na(crude_l3[i, ]$pop)){
    rlag <- crude_l3[i-1, ]
    crude_l3[i, ]$pop <- rlag$pop + 
      (rlag$cbr - rlag$cdr) / 1000 * rlag$pop
  }
}

p5 <- crude_l3 |>
  ggplot(aes(x = year, y = pop, colour = source)) +
  geom_line() +
  #geom_point() +
  labs(x = "",
       y = "",
       colour = "",
       title = "",
       subtitle = "Projection if no migration and crude birth and death rates grew at 1999-2011 rate") +
  scale_y_continuous(label = comma)
  

p3 +p5 +plot_layout(ncol = 1)



