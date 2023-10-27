# https://unsdg.un.org/latest/blog/smallest-footprint-biggest-trouble-inside-push-measure-vulnerability-small-island

source("0255-mvi-pacific.R")


sids <- c(
  "Antigua and Barbuda",
  "Haiti",
  "Saint Kitts and Nevis",
  "Bahamas",
  "Jamaica",
  "Saint Lucia",
  "Barbados",
  "Kiribati",
  "Saint Vincent and the Grenadines",
  "Belize",
  "Maldives",
  "Seychelles",
  "Cabo Verde",
  "Marshall Islands",
  "Solomon Islands",
  "Comoros",
  "Micronesia (Federated States of)",
  "Suriname",
  "Cook Islands",
  "Mauritius",
  "Timor-Leste",
  "Cuba",
  "Nauru",
  "Tonga",
  "Dominica",
  "Niue",
  "Trinidad and Tobago",
  "Dominican Republic",
  "Palau",
  "Tuvalu",
  "Fiji",
  "Papua New Guinea",
  "Vanuatu",
  "Grenada",
  "Samoa",
  "Guinea-Bissau",
  "Sao Tome and Principe",
  "Guyana",
  "Singapore"
  )

sids[!sids %in% mvi$Country] # unfortunatley Cooks and Niue not in the MVI


mvi |>
  mutate(sid = Country %in% sids) |>
  group_by(sid) |>
  summarise(across(where(is.numeric), mean))

mvi |>
  mutate(sid = Country %in% sids) |>
  group_by(sid) |>
  summarise(across(where(is.numeric), median))


#-----------Sachs version---------
download.file("https://irp.cdn-website.com/be6d1d56/files/uploaded/WP_MVI_Sachs%20Massa%20Marinescu%20Lafortune_FINAL_cVeeBVmKSKyYYS6OyiiH.pdf",
              destfile = "sachs.pdf", mode = "wb")

library(pdftools)

sachs_pdf <- pdf_text("sachs.pdf")
tab1 <- sachs_pdf[[35]]

pdf_to_tab <- function(page, find = NULL, replace = ""){
  if(!is.null(find)){
    page <- gsub(find, replace, page, fixed = TRUE)
  }
  page <- gsub(" + ","|", page)
  page <- gsub("^\\|", "\n\n|", page)
  page <- gsub("\\n[ A-Z]", "\n|X", page)
  # save as a text file
  f <- tempfile() # f= "temp.txt"
  writeLines(page, f)
  
  # read back in
  tab <- read_delim(f, delim = "|", col_types = "t")
  
  # for(i in 1:nrow(tab)){
  #   if(!is.na(tab[i, 1])){
  #     tab[i, 4] <- tab[i, 3]
  #     tab[i, 3] <- tab[i, 2]
  #     tab[i, 2] <- tab[i, 1]
  #     tab[i,1] <- NA
  #   }
  # }
  
  return(tab)
}

economic <- rbind(
  pdf_to_tab(page = sachs_pdf[[35]], find = "Table A10. MVI Economic dimension"),
  pdf_to_tab(page = sachs_pdf[[36]]),
  pdf_to_tab(sachs_pdf[[37]]),
  pdf_to_tab(page = sachs_pdf[[38]]),
  pdf_to_tab(page = sachs_pdf[[39]])) |>
  rename(value = `Economic Pillar`) |>
  mutate(variable = "Economic Pillar")


development <- rbind(
  pdf_to_tab(page = sachs_pdf[[40]], find = "Table A11. MVI Structural Development dimension"),
  pdf_to_tab(page = sachs_pdf[[41]]),
  pdf_to_tab(sachs_pdf[[42]]),
  pdf_to_tab(page = sachs_pdf[[43]])) |>
  rename(value = `Development Pillar`) |>
  mutate(variable = "Development Pillar")

environment <- rbind(
  pdf_to_tab(page = sachs_pdf[[44]], find = "Table A12. MVI Environmental dimension"),
  pdf_to_tab(page = sachs_pdf[[45]]),
  pdf_to_tab(sachs_pdf[[46]]),
  pdf_to_tab(page = sachs_pdf[[47]]),
  pdf_to_tab(page = sachs_pdf[[48]])) |>
  rename(value = `Environmental Pillar`) |>
  mutate(variable = "Environmental Pillar")

sachs_mvi <- rbind(
  economic, environment, development
) |>
  mutate(Country = ifelse(Country == "(the)", "United Kingdom", Country)) |>
  # remove page numbers
  filter(!is.na(Regions)) |>
  select(-...1) |>
  spread(variable, value) |>
  mutate(mvi_sachs = (`Development Pillar` + `Economic Pillar` + `Environmental Pillar`) / 3) |>
  # some one off tweaks to names:
  mutate(Country = gsub(" (the)", "", Country, fixed = TRUE)) |>
  mutate(Country = case_when(
    grepl("ao People", Country) ~ "Lao PDR",
    Country == "Tanzania, United Republic of" ~ "United Republic of Tanzania",
    Country == "Côte d'Ivoire" ~ "Côte D'Ivoire",
    Country == "Xongo (the Democratic Republic of the)" ~ "Democratic Republic of the Congo",
    TRUE ~ Country
    ))
  
sachs_mvi |>
  group_by(Regions) |>
  summarise(mvi_sachs = mean(mvi_sachs, na.rm = TRUE))

# all the countries from India to Myanmar are missing in the 'development' pillar
economic |>
  anti_join(development, by = "Country") |>
  pull(Country)

# In Sachs but not UN - 58 countries
sachs_mvi |>
  filter(!Country %in% mvi$Country) |>
  arrange(Country) |>
  pull(Country)

# In Un but not Sachs - 5 countries
mvi |>
  filter(!Country %in% sachs_mvi$Country) |> 
  arrange(Country) |>
  pull(Country)


mvi_both <- sachs_mvi |>
  select(Country, Regions, mvi_sachs) |>
  inner_join(mvi, by = "Country") |>
  mutate(pic = if_else(Regions == "SIDS_Pacific", "Pacific", "Other"),
         sid = if_else(grepl("SIDS", Regions), "SIDS", "Other")) |>
  mutate(pic = fct_relevel(pic, "Other", after = Inf),
         sid = fct_relevel(sid, "Other", after = Inf))

library(MASS)
p <- mvi_both |>
  ggplot(aes(x = mvi_sachs, y = `MVI - Score`)) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey80") +
  geom_point(aes(colour = Regions), size = 2) +
  geom_point(data = filter(mvi_both, Regions == "SIDS_Pacific"),
                 aes(colour = Regions), size = 3, colour = "black", shape = 1) +
  geom_text_repel(data = filter(mvi_both, Regions == "SIDS_Pacific"),
                  aes(label = Country), seed = 123) +
  labs(x = "MVI - Sachs et al version",
       y = "MVI - latest UN version",
       title = "Comparison of two candidate Multidimensional Vulnerability Indexes",
       subtitle = "Around 40 countries missing from the Sachs et al comparison")

svg_png(p, "../img/0258-scatter", w = 11, h = 7.5)


rbind(
  mvi_both |>
    group_by(sid) |>
    summarise(across(where(is.numeric), \(x)mean(x, na.rm = TRUE))) |>
    select(` ` = sid, `MVI - UN` = `MVI - Score`,
           `MVI - Sachs` = mvi_sachs),
  
  mvi_both |>
    group_by(pic) |>
    summarise(across(where(is.numeric), \(x)mean(x, na.rm = TRUE)))  |>
    select(` ` = pic, `MVI - UN` = `MVI - Score`,
           `MVI - Sachs` = mvi_sachs),
  
  mvi_both |>
    summarise(across(where(is.numeric), \(x)mean(x, na.rm = TRUE)))  |>
    mutate(` `= "All countries") |>
    select(` `, `MVI - UN` = `MVI - Score`,
           `MVI - Sachs` = mvi_sachs)
) |>
  filter(` ` != "Other") |>
  mutate(`MVI - Sachs - rescaled` = `MVI - Sachs` / 26.5 * 52.8) |>
  kable(digits = 1) |>
  kable_styling()


library(kableExtra)
