library(GGally)
# you need to have run 0286-voting-and-depression.R first


# county characteristis from US Census Bureau
# see https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2023/CC-EST2023-ALLDATA.pdf
# for metadata



df <- "cc-est2023-alldata.csv"
if(!file.exists(df)){
  download.file("https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/asrh/cc-est2023-alldata.csv",
                destfile = df)
}

# key columns:
# TOT_POP total population
# WA_MALE "White alone" male
# WAC_MALE "white alone or in combination" male
# H_MALE Hispanic male
# HTOM_MALE Hispanic or more races male

race <- read_csv(df) |>
  # just 2023 and TOTAL age group:
  filter(YEAR == 5 & AGEGRP == 0) |>
  mutate(white_alone = (WA_MALE + WA_FEMALE) / TOT_POP,
         white_all = (WAC_MALE + WAC_FEMALE) / TOT_POP,
         hispanic = (H_MALE + H_FEMALE) / TOT_POP,
         hispanic_multi = (HTOM_MALE + HTOM_FEMALE) / TOT_POP) |>
  mutate(county_fips = paste0(STATE, COUNTY)) |>
  select(white_alone:county_fips)


race |>
  sample_n(500) |>
  select(-county_fips) |>
  ggpairs()

# White-alone and white-all are too closely correlated (.991) to use them
# both so will drop white-all (this decision taken without looking at
# relationship to the response variable)


combined4 <- combined2 |>
  left_join(race, by = "county_fips")


summary(model6)


model7 <- gam(per_gop ~ cpe + white_alone + hispanic + hispanic_multi + 
                s(x, y) + s(state_name, bs = 're') , 
              family = quasibinomial, weights = total_votes,
              data = combined4)

summary(model7)


model7a <- gam(per_gop ~ cpe + white_alone + hispanic + hispanic_multi + 
                s(x, y) + s(state_name, bs = 're') , 
              family = gaussian, weights = total_votes,
              data = combined4)

summary(model7a)
anova(model7a)
