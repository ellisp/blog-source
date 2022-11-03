library(tidyverse)
library(haven)
library(glue)
library(scales)
library(broom)
library()


get_col_labels <- function(d){
  tibble(
    short = colnames(d),
    long = sapply(d, function(x){attributes(x)$label}))
}


read_mics <- function(subfolder){
  col_labels <- tibble()
  
  output <- list()

  all_files <- list.files(subfolder, pattern = "\\.sav$")
  
  for(i in 1:length(all_files)){
    df_name <- gsub("\\.sav", "", all_files[i])
    tmp <- read_spss(glue("{subfolder}/{all_files[i]}"))
    output[[df_name]] <- tmp
    col_labels <- rbind(col_labels, mutate(
      get_col_labels(tmp),
      dataframe = df_name)
    )
    output[["col_labels"]] <- col_labels
  }
  return(output)
}

fiji <- read_mics("data/Fiji MICS6 SPSS Datasets")
togo <- read_mics("data/Togo MICS6 SPSS Datasets")
samoa <- read_mics("data/MICS6 Samoa SPSS Datasets")
nepal <- read_mics("data/Nepal MICS6 SPSS Datasets")

# which variable is the household weights?
View(fiji$col_labels)
filter(fiji$col_labels, grepl("weight", long))


common_names <- names(fiji$hh) |>
  intersect(names(samoa$hh)) |>
  intersect(names(nepal$hh)) |>
  intersect(names(togo$hh))

common_names <- common_names[!
  common_names %in% c("stratum", "HH47", "HH48", "WQ1", "HHAGE")]
common_names <- common_names[!grepl("HH5", common_names)]
common_names <- common_names[!grepl("HH26", common_names)]
common_names <- common_names[!grepl("WQ5", common_names)]
common_names <- common_names[!grepl("WQ10", common_names)]
common_names <- common_names[!grepl("WQ23", common_names)]
common_names <- common_names[!grepl("WQ24", common_names)]
common_names <- common_names[!grepl("WQ25", common_names)]

labels_to_character <- function(d){
  afc <- function(x){
    y <- x |>
      as_factor() |>
      as.character() |>
      stringr::str_replace("^OUI$", "YES") |>
      stringr::str_replace("^NON$", "NO") |>
      stringr::str_replace("^Dans leur jardin / Parcelle$", "IN OWN YARD / PLOT") |>
      stringr::str_replace("^Dans leur logement$", "IN OWN DWELLING") |>
      stringr::str_replace("^Ailleurs$", "ELSEWHERE") |>
      stringr::str_replace("^Féminin$", "Female") |>
      stringr::str_replace("^Masculin$", "Male") |>
      stringr::str_replace("^Sans Assurance$", "Without insurance") |>
      stringr::str_replace("^Avec Assurance$", "With insurance") |>
      stringr::str_replace("^A une difficult. fonctionnelle", "Has functional difficulty") |>
      stringr::str_replace("^Pas de difficult. fonctionnelle", "Has no functional difficulty") |>
      stringr::str_replace("^SEPARE$", "SEPARATED") |>
      stringr::str_replace("^VEUVE$", "WIDOWED")
      
    
    y[y == "NO RESPONSE"] <- NA
    y[y == "DK"] <- NA
    return(y)
    }
  mutate(d, across(where(is.labelled), afc))
}
  

all_hh <- bind_rows(
  mutate(labels_to_character(select(fiji$hh, all_of(common_names))), country = "Fiji"),
  mutate(labels_to_character(select(samoa$hh, all_of(common_names))), country = "Samoa"),
  mutate(labels_to_character(select(nepal$hh, all_of(common_names))), country = "Nepal"),
  mutate(labels_to_character(select(togo$hh, all_of(common_names))), country = "Togo")
)

dim(all_hh)

count(all_hh, HC13)
# before knocking things out of common_names, this got lots of 
# warnings about conflicting value labels

# eg HH7 and HH7

count(labels_to_character(fiji$hh), HH7)

count(labels_to_character(fiji$hh), HH5M)
count(labels_to_character(fiji$hh), HH5D)

count(fiji$hh, HH7)
count(samoa$hh, HH7)
count(nepal$hh, HH7)
count(togo$hh, HH7)

all_hh |>
  group_by(country) |>
  summarise(mean_weight = mean(hhweight),
            median_weight = median(hhweight),
            max_weight = max(hhweight))
  
ggplot(all_hh, aes(x = hhweight, colour = country)) +
  geom_density() +
  geom_rug()

all_hh |>
  group_by(country, HC13) |>
  summarise(n = sum(hhweight)) |>
  drop_na() |>
  group_by(country) |>
  summarise(perc_internet = n[HC13 == "YES"] / sum(n))

all_hh |>
  group_by(country, HC19) |>
  summarise(n = sum(hhweight)) |>
  drop_na() |>
  group_by(country) |>
  summarise(perc_bankaccount = n[HC19 == "YES"] / sum(n))


all_hh |>
  group_by(country, HC19, HHSEX) |>
  summarise(n = sum(hhweight)) |>
  drop_na() |>
  group_by(country, HHSEX) |>
  summarise(perc_bankaccount = n[HC19 == "YES"] / sum(n))

all_hh |>
  group_by(country, HC13, HHSEX) |>
  summarise(n = sum(hhweight)) |>
  drop_na() |>
  group_by(country, HHSEX) |>
  summarise(perc_internet = n[HC13 == "YES"] / sum(n))

count(all_hh, WS3, wt = hhweight)

# HC13 - does household have internet at home
# windex10 wealth decile

all_hh

count(all_hh, windex10, wt = hhweight)
count(all_hh, windex10)

all_hh |>
  group_by(country, HC13, windex10) |>
  summarise(n = sum(hhweight)) |>
  drop_na() |>
  group_by(country, windex10) |>
  summarise(perc_internet = n[HC13 == "YES"] / sum(n)) |>
  ungroup() |>
  complete(country, windex10, fill = list(perc_internet = 0)) |>
  mutate(windex10 = fct_relevel(windex10, "10th decile", after = Inf),
         country = fct_reorder(country, perc_internet)) |>
  ggplot(aes(x = windex10, y = country, fill = perc_internet)) +
  geom_tile() +
  scale_fill_viridis_c(label = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Access to the Internet",
       caption = "Source: Multi Indicator Cluster Surveys",
       x= "Household wealth",
       y = "",
       fill = "",
       subtitle = str_wrap("Higher wealth households have access 
                           to the internet in all countries, but 
                           at lower levels of wealth the 
                           differences between countries are stronger.", 80))


#--------------women--------------

common_names_wm <- names(fiji$wm) |>
  intersect(names(samoa$wm)) |>
  intersect(names(nepal$wm)) |>
  intersect(names(togo$wm))

common_names_wm <- common_names_wm[!
                               common_names_wm %in% c("stratum", "HH47", "HH48", "WQ1", "HHAGE")]

drop_patterns <- function(x, pattern){
  x <- x[!grepl(pattern, x)]
  return(x)
}

bad_patterns <- c("WM7M", "WM10M", "WM6D", "WB4", "CM")
for(p in bad_patterns){
  common_names_wm <- drop_patterns(common_names_wm, p)
}

all_wm <- bind_rows(
  mutate(labels_to_character(select(fiji$wm, all_of(common_names_wm))), country = "Fiji"),
  mutate(labels_to_character(select(samoa$wm, all_of(common_names_wm))), country = "Samoa"),
  mutate(labels_to_character(select(nepal$wm, all_of(common_names_wm))), country = "Nepal"),
  mutate(labels_to_character(select(togo$wm, all_of(common_names_wm))), country = "Togo")
)




# which variables available
fiji$col_labels |>
  filter(dataframe == "wm") |>
  filter(short %in% common_names_wm) |>
  View()

all_wm2 <- all_wm |>
  filter(!is.na(HA1)) |> 
  mutate(heard_of_aids = as.logical(HA1 == "YES"),
         WAGE = fct_relevel(WAGE, "30-34"),
         MA1a = replace_na(MA1, "NO"),
         WB14a = replace_na(WB14, "not tested"),
         wealth = scale(wscore),
         cant_read = as.numeric(grepl("CANNOT READ", WB14a) |
                                  grepl("NE PEYT PAS LIRE", WB14a)),
         with_a_man = as.numeric(grepl("^OUI", MA1a) | grepl("^YES", MA1a)),
         with_insurance = as.numeric(insurance == "With insurance"),
         with_disability = as.numeric(replace_na(disability, "") == "Has functional difficulty"))

model <- glm(heard_of_aids ~ with_a_man + wealth + WAGE + 
               with_insurance + with_disability + cant_read +
               country, 
            family = binomial, data = all_wm2)

anova(model, test = "Chi")

count(all_wm2, WAGE)
count(all_wm2, with_disability)
count(all_wm2, with_insurance)
count(all_wm2, cant_read)

summary(model)

results <- cbind(tidy(model), confint(model)) |>
  mutate(term_group = case_when(
    grepl("WAGE", term) ~ "Age (compared to 30-34)",
    grepl("country", term) ~ "Country (compared to Fiji)",
    grepl("wealth", term) ~ "Wealth (per 1 standard deviation)",
    TRUE ~ "Other variables")) |>
  mutate(term = gsub("WAGE", "", term),
         term = gsub("country", "", term),
         term = gsub("_", " ", term)) |>
  mutate(term_group = fct_reorder(term_group, estimate)) |>
  rename(lower = `2.5 %`,
         upper = `97.5 %`)

results |>
  filter(term != "(Intercept)") |>
  ggplot(aes(x = estimate, y = term, colour = term_group)) +
  geom_vline(xintercept = 0) +
  geom_point() +
  geom_segment(aes(x = lower, xend = upper, yend = term)) +
  theme(legend.position = "right") +
  guides(colour = guide_legend(reverse = TRUE))

count(all_wm, HA1, country)
count(all_wm, MA6, country)


# Households
hh

# Household members
hl

# Women
wm


# fertility / birth history
bh

# Children under 5
ch

# Children 5-17
fs

# Men
mn
