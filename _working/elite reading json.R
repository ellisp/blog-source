

library(R.utils)
library(tidyverse)
library(jsonlite)
library(glue)

# fn <- glue("galaxy_populated_{Sys.Date()}.json.gz")
fn <- "galaxy_population.json.gz"

if(!file.exists(fn)){
  download.file("https://downloads.spansh.co.uk/galaxy_populated.json.gz",
                destfile = fn, mode = "wb")
  
  gunzip(fn, remove = FALSE)
}
d_raw <- list()
systems <- tibble()
factions <- tibble()
to_skip <- 1 # we always skip the first line which is an opening square bracket
chunk_size <- 100
finished <- FALSE
while(!finished){
  print(to_skip)
  tmp <- read_lines("galaxy_populated.json", skip = to_skip, n_max = chunk_size)

  # might be able to speed it up here by deleting commodity prices etc.
    
  if(tmp[length(tmp)] != "]"){
    # remove trailing comma
    tmp[length(tmp)] <- str_sub(tmp[length(tmp)], end = -2) 
    # add square brackets at beginning and end
    write_lines(c("[", tmp, "]"), "tmp.json")
  } else {
    write_lines(c("[", tmp), "tmp.json")
    finished <- TRUE
  }
  d_raw <- fromJSON("tmp.json")
  
  # a lot of the info we don't need - all the market prices, all the compositions
  # of every planet
  
  space_pads <- sapply(d_raw$stations, function(d){
    case_when(
      max(d$landingPads$large, na.rm = TRUE) > 0 ~ 3,
      max(d$landingPads$medium, na.rm = TRUE) > 0 ~ 2,
      max(d$landingPads$small, na.rm = TRUE) > 0 ~ 1) |>
      max()
  })
    
  
  new_systems <- with(d_raw,
        tibble(
          id64 = as.character(id64),
          name = as.character(name),
          x = coords$x,
          y = coords$y,
          z = coords$z,
          population = as.character(population),
          controlling_faction_name = controllingFaction$name,
          controlling_faction_government = controllingFaction$government,
          controlling_faction_allegiance = allegiance, 
          powers = as.character(powers),
          powerState = as.character(powerState),
          primaryEconomy = as.character(primaryEconomy),
          secondaryEconomy = as.character(secondaryEconomy),
          security = as.character(security),
          date = as.character(date),
          max_space_landing_pad = case_when(
            space_pads == 1 ~ "Small",
            space_pads == 2 ~ "Medium",
            space_pads == 3 ~ "Large"
          )
        )
      )
  
  if(sum(is.na(new_systems$controlling_faction_government)) > 0){
    stop()
    # this appears to be a bug eg for New Yambo, has no controlling faction
  }
  
  faction_numbers <- lapply(d_raw$factions, nrow)
  faction_numbers_no_0 <- sapply(faction_numbers, function(x){ifelse(is.null(x), 0, x)})
  
  new_factions <- bind_rows(d_raw$factions) |>
    mutate(id64 = rep(new_systems$id64, faction_numbers_no_0))
  
  systems <- bind_rows(systems, new_systems)
  factions <- bind_rows(factions, new_factions)
  
  to_skip <- to_skip + chunk_size
}

# save(factions, file = "factions.rda")
# save(systems, file = "systems.rda")
# load("factions.rda")
# load("systems.rda")

#-------------further processing------------

# and create a powers table for Power BI
# and a government table for Power BI

# have a way of marking in the factions data frame
# which faction is ontrolling

count(systems, powerState)
controls <- systems |>
  filter(powerState == "Controlled") |>
  select(control_id = id64,
         control_name = name,
         control_power = powers,
         control_x = x,
         control_y = y,
         control_z = z)
  
system_controls <- systems |>
  filter(powerState == "Exploited") |>
  left_join(controls, by = c("powers" = "control_power"), 
            relationship = "many-to-many") |>
  mutate(distance = sqrt((x - control_x) ^2 + (y - control_y) ^ 2 + (z - control_z) ^2)) |>
  group_by(name) |>
  arrange(distance) |>
  slice(1) |>
  ungroup() |>
  select(id64, control_id:control_z, distance)

systems_with_controls <- systems |>
  left_join(system_controls, by = "id64") |>
  mutate(powers = gsub('\", \"', '; ', powers, fixed = TRUE),
         powers = gsub('c(\"', '', powers, fixed = TRUE),
         powers = gsub('\")', '', powers, fixed = TRUE),
         powers = ifelse(powers == 'NULL', NA, powers)) 

powers_df <- systems_with_controls |>
  count(powers) |>
  filter(!is.na(powers)) |>
  filter(!grepl("; ", powers))

tmp <- systems |>
  select(id64, controlling_faction_name) |>
  mutate(is_controlling = TRUE)

factions_with_ranking <-  factions |>
  group_by(id64) |>
  arrange(desc(influence)) |>
  mutate(inf_ranking = 1:n()) |>
  rename(faction_name = name) |>
  left_join(tmp, by = c("id64", "faction_name" = "controlling_faction_name")) |>
  mutate(is_controlling = replace_na(is_controlling, FALSE)) |>
  mutate(alt_faction_name = if_else(is_controlling, glue("**{faction_name}"),
                                    faction_name))



elite_power_ethos <- read_csv("elite_power_ethos.csv")
elite_power_strengths <- read_csv("elite_power_strengths.csv") |>
  mutate(strength = factor(strength, 
                           levels = c("Unknown", "Weak", "Neutral", "Strong"))) 
elite_power_fun <- elite_power_ethos |>
  left_join(elite_power_strengths, by = "ethos") |>
  select(power, purpose, government, strength)


exploited_systems <- systems_with_controls |>
  filter(powerState == "Exploited") |>
  left_join(filter(elite_power_fun, purpose == "Fortification"),
            by = c("powers" = "power", 
                   "controlling_faction_government" = "government")) |>
  mutate(strength = replace_na(strength, "Unknown"))



ref_sys <- filter(systems, name == "Rhea") |>
  select(x, y, z)

s2 <- systems |>
  select(id64, x, y, z) |>
  mutate(dist_from_ref = sqrt((x - ref_sys$x) ^ 2 +
           (y - ref_sys$y) ^ 2 + (z - ref_sys$z) ^ 2)) |>
  filter(dist_from_ref < 220) |>
  mutate(link = 1)
dim(s2)

# note 3 systems that are > 200ly from rhea - Ither, HIP 104256 
# and Bormburrun
# s2 |>
#   filter(powers == "Felicia Winters") |>
#   filter(dist_from_ref > 200) |>
#   select(name, dist_from_ref)
  
# distance between systems, but only for those that are within 15 light years,
# and are < 230 ly from Rhea.
# This takes a few minutes to calculate. Needs more RAM.
system_distances <- s2 |>
  select(id642 = id64, x2 = x, y2 = y, z2 = z, link) |>
  full_join(select(s2, id64, x, y, z, link), by = "link",
            relationship = "many-to-many") |>
  filter(id64 < id642) |>
  filter(abs(x2 - x) <= 15) |>
  filter(abs(y2 - y) <= 15) |>
  filter(abs(z2 - z) <= 15) |>
  mutate(distance = sqrt((x - x2) ^2 + (y - y2) ^ 2 + (z - z2) ^2)) |>
  filter(distance <= 15)
# should change id64 to an integer. big integer?  

save(system_distances, file = "system_distances.rda")

system_distances_backwards <-  system_distances
names(system_distances_backwards) <- names(system_distances)[c(6:9, 5, 1:4, 10)]
system_distances_backwards <- select(system_distances_backwards, 6:9, 5, 1:4, 10)
system_distances_dupes <- bind_rows(system_distances, system_distances_backwards)  
  

#---------------examples--------------------  
# example use - FUN for a particular system
exploited_systems |>
  filter(control_name == "Lulua") |>
  select(name, controlling_faction_name, controlling_faction_government,
         strength, max_space_landing_pad, distance) #|>  View()

# example use - Strawberry style sheet for exploited systems around a
# given control
exploited_systems |>
  filter(control_name == "Lulua") |>
  select(id64, system_name = name) |>
  left_join(factions_with_ranking, by = "id64") |>
  filter(inf_ranking <= 3) |>
  pivot_wider(id_cols = c(system_name),
              names_from = inf_ranking,
              values_from = c(alt_faction_name, government, influence),
              names_vary = "slowest")  |>  View()

# example use - all FUN for winters forting
exploited_systems |>
  filter(powers == "Felicia Winters") |>
  group_by(control_name, strength) |>
  summarise(freq = n()) |>
  spread(strength, freq, fill = 0) |>
  mutate(triggers = case_when(
    Strong > Neutral & Strong > Weak ~ "Half",
    Weak > Neutral ~ "Penalty",
    TRUE ~ "Neutral"
  )) |>
  mutate(triggers = factor(triggers, levels = c(
    "Penalty", "Neutral", "Half"
  ))) |>
  arrange(desc(triggers)) # |>   View()
# what happens when Weak and Strong are tied
# check this against some known situations


# example use - FUN for a system we might want to expand to
systems |>
  filter(name == "Malgariji") |>
  select(id64) |>
  inner_join(system_distances_dupes, by = "id64") |>
  filter(distance <= 15) |>
  select(id64 = id642) |>
  left_join(select(systems, id64, system_name = name)) |>
  left_join(factions_with_ranking, by = "id64") |>
  filter(inf_ranking <= 3) |>
  pivot_wider(id_cols = c(system_name),
              names_from = inf_ranking,
              values_from = c(alt_faction_name, government, influence),
              names_vary = "slowest")  |>  
  arrange(government_1) |> View()


# example use - all currently unexploited and uncontrolled systems 
# that might have half triggers for Winters



# example use - pivot factions wider for a strawberry

#-----------problems-----------------
# problem with several hundred systems that have no controlling faction
exploited_systems |>
  filter(is.na(controlling_faction_name)) |> pull(name)
  count(powers)
  
# An example is Warkawa but   Inara lists it as having
  # Black Widow as controlling faction, so what is problem?

  
  
  #--------------for Power BI-----------------  
write_csv(systems_with_controls, "systems.csv")
write_csv(exploited_systems, "exploidted_systems.csv")
write_csv(factions, "factions.csv")
write_csv(powers_df, "powers_df.csv")

# unique(systems$powers)
# count(systems, powers, sort = FALSE)
# systems
# factions


