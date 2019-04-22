
#====================Preparation=============

#----------------------------packages---------------
library(tidyverse)
library(scales)
library(rio)
library(svglite)
library(frs)
library(readxl)
library(testthat)
library(survey)
library(vcd)

#---------------------Import and tidy data-----------------
# Download by hand from https://dataverse.ada.edu.au/dataset.xhtml?persistentId=doi:10.4225/87/7OZCZA

aes2016_orig <- import("aes/2. Australian Election Study, 2016.sav")
aes2016_code_orig <- read_excel("aes/1. Australian Election Study, 2016 - Codebook.xlsx",
                                sheet = "Data Dictionary")

aes2016_code <- aes2016_code_orig %>%
  rename(question_label = Label...3,
         answer_label = Label...5) %>%
  rename_all(tolower) %>%
  fill(variable, position, question_label) %>%
  mutate(var_abb = substring(variable, 1, 8)) %>%
  mutate(var_abb = case_when(
    var_abb == "H19_STAT" ~ "H19STATE",
    var_abb == "H19_PCOD" ~ "H19pcoRV",
    var_abb == "H20_othe" ~ "H20_oth",
    var_abb == "H25_othe" ~ "H25_oth",
    var_abb == "Final_ST" ~ "finSTATE",
    var_abb == "Samp_STA" ~ "SamSTATE",
    var_abb == "detailed" ~ "doutcome",
    var_abb == "Responde" ~ "responID",
    var_abb == "Start_Ti" ~ "Sta_time",
    var_abb == "Samp_PCO" ~ "SampcoRV",
    var_abb == "Final_PC" ~ "finpcoRV",
    var_abb == "Total_Du" ~ "totaldur",
    TRUE ~ var_abb
  )) %>%
  rbind(tibble(
    variable = "StateMap", position = NA, question_label = "Unidentified state variable",
    value = 1:8, 
    answer_label = c("New South Wales", "Victoria", "Queensland", "South Australia", "Western Australia",
                     "Tasmania", "Northern Territory", "Australian Capital Territory"),
    var_abb = "StateMap"
  ))

aes2016_questions <- distinct(aes2016_code, var_abb, position, question_label)

aes2016_answers <- aes2016_code %>%
  select(var_abb, variable, value, answer_label) %>%
  filter(!is.na(value))

# Check all the names in the data now match those in the data dictionary:  
expect_equal(
  names(aes2016_orig)[!names(aes2016_orig) %in% aes2016_questions$var_abb],
  character(0)
)

# ... and vice versa:
expect_equal(
  unique(aes2016_questions$var_abb)[!unique(aes2016_questions$var_abb) %in% names(aes2016_orig)],
  character(0)
)

aes2016 <- aes2016_orig %>%
  as_tibble() 

attributes(aes2016)$question_labels <- names(aes2016)

for(i in 1:ncol(aes2016)){
  this_q <- filter(aes2016_questions, var_abb == names(aes2016)[i])
  
  
  # Sometimes a code like 999 'Skipped' is not present in the data but has already
  # been replaced with an NA, so we don't want it in our factor level. So we need
  # to find out what answers are actually used in the i'th column
  used_a <- unique(pull(aes2016, i))
  
  # the answer labels for this particular question
  these_a <- aes2016_code %>%
    filter(var_abb == names(aes2016)[i]) %>%
    filter(value %in% used_a) %>%
    arrange(value)
  
  attributes(aes2016)$question_labels[i] <- pull(this_q, question_label)
  
  # half a dozen open text questions don't match to the data dictionary so we exclude those:
  if(nrow(these_a) > 0 & 
     !pull(this_q, question_label) %in% c("What kind of work do you do? Full job title",
                                          "What are (or were) the main tasks that you usually perform(ed)?",
                                          "What kind of business or industry is (or was) that in?",
                                          "What was your partner's main activity last week? (other specify)",
                                          "What kind of work does (or did) your partner do? Provide job title",
                                          "Generally speaking, does your partner usually think of himself or herself as Liberal, Labor, National or what? (other specify)")
  ){
    # for the rest, we turn the response into a factor with the correct labels
    aes2016[ , i] <- factor(pull(aes2016, i), labels = pull(these_a, answer_label))  
  }
}

aes2016 <- aes2016 %>%
  mutate(age_2016 = 2016 - H2,
         agegrp_2016 = cut(age_2016, 
                           breaks = c(0, 17.5, 24.5, 34.5, 44.5, 54.5, 64.5, Inf),
                           labels = c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")),
         agegrp_2016 = as.factor(replace_na(as.character(agegrp_2016), "Age unknown")),
         sex = replace_na(as.character(H1), "Sex unknown"),
         first_pref_hr_grp = case_when(
           B9_1 %in% c("Liberal Party", "National (Country) Party") ~ "Coalition",
           B9_1 == "Labor Party (ALP)"                              ~ "Labor",
           B9_1 == "Greens"                                         ~ "Greens",
           B9_1 == "Voted informal" | is.na(B9_1)                   ~ "Did not vote/voted informal",
           TRUE                                                     ~ "Other"
         ),
         first_pref_sen_grp = case_when(
           B9_2 %in% c("Liberal Party", "National (Country) Party") ~ "Coalition",
           B9_2 == "Labor Party (ALP)"                              ~ "Labor",
           B9_2 == "Greens"                                         ~ "Greens",
           B9_2 == "Voted informal" | is.na(B9_2)                   ~ "Did not vote/voted informal",
           TRUE                                                     ~ "Other"
         ),
         first_pref_sen_grp2 = case_when(
           B9_2 == "Liberal Party"                                  ~ "Liberal",
           B9_2 == "National (Country) Party"                       ~ "National",
           B9_2 == "One Nation"                                     ~ "One Nation",
           B9_2 == "Labor Party (ALP)"                              ~ "Labor",
           B9_2 == "Greens"                                         ~ "Greens",
           TRUE                                                     ~ "Other (including did not vote)"
         )
  ) %>%
  mutate(first_pref_sen_grp2 = toupper(first_pref_sen_grp2),
         first_pref_sen_grp2 = ifelse(grepl("^OTHER", first_pref_sen_grp2),
                                      "OTHER (incl. did not vote)",
                                      first_pref_sen_grp2)) %>%
  mutate(first_pref_sen_grp2 = fct_relevel(first_pref_sen_grp2,
                                           toupper(c("Greens", "Labor", "Liberal",
                                                     "National", "One Nation")))) %>%
  mutate(tpp_alp = B11_1  %in% "Labor Party (ALP)" | B9_1 %in% "Labor Party (ALP)")
   


#--------------------Weighting-----------------------
# The sample has two parts - addresses supplied by the AEC, and addresses from 
# the GNAF (Geocoded National Address File). There are separate weights for each,
# so they can be analysed as two separate surveys or you can use the combined set of weights:
table(aes2016$wt_aec > 0, useNA = 'always')    # for comparison to previous waves of AES
table(aes2016$wt_gnaf > 0, useNA = 'always')   # inference about Australian adults
table(aes2016$wt_enrol > 0, useNA = 'always')  # inference about enrolled Australian adults
# There are 107 cases from the GNAF sample that have zero weight in the final combined sample:
filter(aes2016, wt_enrol <= 0) %>% select(wt_aec, wt_gnaf, wt_enrol)

# wt_enrol had been raked for age group, sex, state and first preference vote. See pages 32-33
# of the technical guide.

# These three state variables seem identical, and are all different from those in Table 16
# of the technical report:
cbind(
  table(aes2016$finSTATE),
  table(aes2016$StateMap),
  table(aes2016$SamSTATE)
  )

# In fact, Table 16 has the state, age and sex only of the GNAF sample, not the whole sample:
table(filter(aes2016, wt_gnaf >0)$finSTATE)
table(filter(aes2016, wt_gnaf >0)$agegrp_2016)
table(filter(aes2016, wt_gnaf >0)$H1)

# The first pref party vote in Table 16 looks to be just wrong. The results below match those
# in the code book, but not in Table 16
table(aes2016$first_pref_hr_grp, useNA = 'always')
table(aes2016$first_pref_sen_grp, useNA = 'always')

# Well, we'll assume that wt_enrol has actually been weighted as described, and ignore
# the problems in Table 16.

# Set up survey design:
aes2016_svy <- svydesign(~1, strata = ~SamSTATE, weights = ~wt_enrol, data = aes2016)

# Deduce population totals from the sums of weights (if weights were raked to match pop totals,
# then the sum of the weights should reveal what they were)
age_pop <- aes2016 %>%
  group_by(agegrp_2016) %>%
  summarise(Freq = sum(wt_enrol))

sex_pop <- aes2016 %>%
  group_by(sex) %>%
  summarise(Freq = sum(wt_enrol))

state_pop <- aes2016 %>%
  group_by(SamSTATE) %>%
  summarise(Freq = sum(wt_enrol))

age2016_svy <- rake(aes2016_svy, sample.margins = list(~sex, ~agegrp_2016, ~SamSTATE),
                    population.margins = list(sex_pop, age_pop, state_pop))


# Compare the weighted and unweighted response to a survey question
filter(aes2016_questions, var_abb == "H14_2")
svytable(~H14_2, aes2016_svy)
table(aes2016$H14_2)
# Many more "yes" posted answers to the Internet when weighted. So people active on the internet
# needed extra weight (probably younger)


#=================Batteries of similar questions========================


#------------------Exploration---------------------
svytable(~ F7 + first_pref_sen_grp, aes2016_svy, Ntotal = 1000, round = TRUE)   # Immigration
svytable(~ F6_4 + first_pref_sen_grp, aes2016_svy, Ntotal = 1000, round = TRUE) # war on terror
svytable(~ F2 + first_pref_sen_grp, aes2016_svy, Ntotal = 1000, round = TRUE)   # Republic
svytable(~ E9_13 + first_pref_sen_grp, aes2016_svy, Ntotal = 1000, round = TRUE)   # trust universities
svytable(~ E9_14 + first_pref_sen_grp, aes2016_svy, Ntotal = 1000, round = TRUE)   # trust political system


#-----------Confidence in institutions----------------------
d1 <- aes2016 %>%
  select(E9_1:E9_14, wt_enrol, first_pref_sen_grp2) %>%
  gather(variable, value, -wt_enrol, -first_pref_sen_grp2) %>%
  group_by(variable, value, first_pref_sen_grp2) %>%
  summarise(freq = sum(wt_enrol)) %>%
  ungroup() %>%
  left_join(aes2016_questions, by = c("variable" = "var_abb")) %>%
  mutate(institution = gsub("How much .+\\? ", "", question_label)) %>%
  filter(!is.na(value)) %>%
  group_by(variable, first_pref_sen_grp2, institution) %>%
  mutate(negative = value %in% c("Not very much confidence", "None at all"),
         prop = freq/sum(freq) * ifelse(negative, -1, 1)) %>%
  mutate(value = factor(value, levels = c("None at all", 
                                          "Not very much confidence",
                                          "A great deal of confidence",
                                          "Quite a lot of confidence"))) %>%
  ungroup() %>%
  mutate(institution = fct_reorder(institution, prop, .fun = sum))

pal <- brewer.pal(4, "RdYlGn")
names(pal) <- levels(d1$value)[c(1,2,4,3)]

p1 <- d1 %>%
  ggplot(aes(x = institution, fill = value, weight = prop)) +
  geom_bar(data = filter(d1, negative), position = "stack") +
  geom_bar(data = filter(d1, !negative), position = "stack") +
  facet_wrap(~first_pref_sen_grp2, ncol = 3) +
  coord_flip() +
  scale_fill_manual("", 
                    values = pal,
                    guide = guide_legend(reverse = FALSE),
                    breaks = c("None at all", 
                                  "Not very much confidence",
                                  "Quite a lot of confidence",
                                  "A great deal of confidence")) +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1), labels = c("100%", "50%", "0", "50%", "100%")) + 
  expand_limits(y = c(-1, 1)) +
  theme(panel.spacing = unit(1.5, "lines"))+
  ggtitle("Attitudes to institutions, by first preference Senate vote in 2016",
          "How much confidence do you have in the following organisation? ...") +
  labs(x = "", y = "", caption = "Source: Australian Election Study 2016; analysis by freerangestats.info.")

svglite("../img/0152-trust.svg", 11, 8)
print(p1)
dev.off()

#-----------more or less expenditure than now----------------------
d2 <- aes2016 %>%
  select(D8_1:D8_10, wt_enrol, first_pref_sen_grp2) %>%
  gather(variable, value, -wt_enrol, -first_pref_sen_grp2) %>%
  group_by(variable, value, first_pref_sen_grp2) %>%
  summarise(freq = sum(wt_enrol)) %>%
  ungroup() %>%
  left_join(aes2016_questions, by = c("variable" = "var_abb")) %>%
  mutate(item = gsub("Should there be .+\\? ", "", question_label)) %>%
  filter(!is.na(value)) %>%
  group_by(variable, first_pref_sen_grp2, item) %>%
  mutate(negative = value %in% c("Much less than now", "Somewhat less than now"),
         positive = value %in% c("Much more than now", "Somewhat more than now"),
         same = !negative & !positive,
         prop = freq/sum(freq) * case_when(
           negative ~ -1,
           positive ~ 1,
           TRUE ~ 0.5)) %>%
  mutate(value = factor(value, levels = c("Much less than now", 
                                          "Somewhat less than now",
                                          "Much more than now",
                                          "Somewhat more than now",
                                          "The same as now"))) %>%
  ungroup() %>%
  mutate(item = fct_reorder(item, prop, .fun = sum))

pal <- brewer.pal(5, "RdYlGn")
names(pal) <- levels(d2$value)[c(1,2,5,4,3)]

d2a <- d2 %>% 
  filter(negative | same) %>%
  mutate(prop = -abs(prop))

p2 <- d2 %>%
  ggplot(aes(x = item, fill = value, weight = prop)) +
  geom_bar(data = d2a, position = "stack") +
  geom_bar(data = filter(d2,positive | same), position = "stack") +
  facet_wrap(~first_pref_sen_grp2, ncol = 3) +
  coord_flip() +
  scale_fill_manual("", 
                    values = pal,
                    guide = guide_legend(reverse = FALSE),
                    breaks = levels(d2$value)[c(1,2,5,4,3)]) +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1), labels = c("100%", "50%", "0", "50%", "100%")) + 
  expand_limits(y = c(-1, 1)) +
  theme(panel.spacing = unit(1.5, "lines"))+
  ggtitle("Attitudes to public spend, by first preference Senate vote in 2016",
          "Should there be more or less public expenditure in the following area? ...") +
  labs(x = "", y = "", caption = "Source: Australian Election Study 2016; analysis by freerangestats.info.")

svglite("../img/0152-spend.svg", 11, 8)
print(p2)
dev.off()

#----------change gone far enough----------
d3 <- aes2016 %>%
  select(E2_1:E2_7, wt_enrol, first_pref_sen_grp2) %>%
  gather(variable, value, -wt_enrol, -first_pref_sen_grp2) %>%
  group_by(variable, value, first_pref_sen_grp2) %>%
  summarise(freq = sum(wt_enrol)) %>%
  ungroup() %>%
  left_join(aes2016_questions, by = c("variable" = "var_abb")) %>%
  mutate(item = gsub("Do you think .+\\? ", "", question_label)) %>%
  filter(!is.na(value)) %>%
  group_by(variable, first_pref_sen_grp2, item) %>%
  mutate(negative = value %in% c("Gone much too far", "Gone too far"),
         positive = value %in% c("Not gone nearly far enough", "Not gone far enough"),
         same = !negative & !positive,
         prop = freq/sum(freq) * case_when(
           negative ~ -1,
           positive ~ 1,
           TRUE ~ 0.5)) %>%
  mutate(value = factor(value, levels = c("Gone much too far", 
                                          "Gone too far",
                                          "Not gone nearly far enough",
                                          "Not gone far enough",
                                          "About right"))) %>%
  ungroup() %>%
  mutate(item = fct_reorder(item, prop, .fun = sum))

pal <- brewer.pal(5, "RdYlGn")
names(pal) <- levels(d3$value)[c(1,2,5,4,3)]

d3a <- d3 %>% 
  filter(negative | same) %>%
  mutate(prop = -abs(prop))

p3 <- d3 %>%
  ggplot(aes(x = item, fill = value, weight = prop)) +
  geom_bar(data = d3a, position = "stack") +
  geom_bar(data = filter(d3, positive | same), position = "stack") +
  facet_wrap(~first_pref_sen_grp2, ncol = 3) +
  coord_flip() +
  scale_fill_manual("", 
                    values = pal,
                    guide = guide_legend(reverse = FALSE),
                    breaks = levels(d3$value)[c(1,2,5,4,3)]) +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1), labels = c("100%", "50%", "0", "50%", "100%")) + 
  theme(panel.spacing = unit(1.5, "lines")) +
  expand_limits(y = c(-1, 1)) +
  ggtitle("Attitudes to change, by first preference Senate vote in 2016",
          "Do you think the following change that has been happening in Australia over the years has gone...?") +
  labs(x = "", y = "", caption = "Source: Australian Election Study 2016; analysis by freerangestats.info.")

svglite("../img/0152-change.svg", 11, 8)
print(p3)
dev.off()

#----------agree with various economic statements----------
d4 <- aes2016 %>%
  select(D13_1:D13_6, wt_enrol, first_pref_sen_grp2) %>%
  gather(variable, value, -wt_enrol, -first_pref_sen_grp2) %>%
  group_by(variable, value, first_pref_sen_grp2) %>%
  summarise(freq = sum(wt_enrol)) %>%
  ungroup() %>%
  left_join(aes2016_questions, by = c("variable" = "var_abb")) %>%
  mutate(item = gsub("Do you strongly .+\\? ", "", question_label)) %>%
  filter(!is.na(value)) %>%
  group_by(variable, first_pref_sen_grp2, item) %>%
  mutate(negative = value %in% c("Disagree", "Strongly disagree"),
         positive = value %in% c("Agree", "Strongly agree"),
         same = !negative & !positive,
         prop = freq/sum(freq) * case_when(
           negative ~ -1,
           positive ~ 1,
           TRUE ~ 0.5)) %>%
  mutate(value = factor(value, levels = c("Strongly disagree", 
                                          "Disagree",
                                          "Strongly agree",
                                          "Agree",
                                          "Neither agree nor disagree"))) %>%
  ungroup() %>%
  mutate(item = fct_reorder(item, prop, .fun = sum))

pal <- brewer.pal(5, "RdYlGn")
names(pal) <- levels(d4$value)[c(1,2,5,4,3)]

d4a <- d4 %>% 
  filter(negative | same) %>%
  mutate(prop = -abs(prop))

p4 <- d4 %>%
  ggplot(aes(x = item, fill = value, weight = prop)) +
  geom_bar(data = d4a, position = "stack") +
  geom_bar(data = filter(d4, positive | same), position = "stack") +
  facet_wrap(~first_pref_sen_grp2, ncol = 3) +
  coord_flip() +
  scale_fill_manual("", 
                    values = pal,
                    guide = guide_legend(reverse = FALSE),
                    breaks = levels(d4$value)[c(1,2,5,4,3)]) +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1), labels = c("100%", "50%", "0", "50%", "100%")) + 
  theme(panel.spacing = unit(1.5, "lines")) +
  expand_limits(y = c(-1, 1)) +
  ggtitle("Attitudes to left-right economic issues, by first preference Senate vote in 2016",
          "Do you strongly agree ... or strongly disagree with the following statement?") +
  labs(x = "", y = "", caption = "Source: Australian Election Study 2016; analysis by freerangestats.info.")

svglite("../img/0152-agree-economic.svg", 11, 8)
print(p4)
dev.off()

#----------agree with various social statements----------
d5 <- aes2016 %>%
  select(E6_1:E6_7, wt_enrol, first_pref_sen_grp2) %>%
  gather(variable, value, -wt_enrol, -first_pref_sen_grp2) %>%
  group_by(variable, value, first_pref_sen_grp2) %>%
  summarise(freq = sum(wt_enrol)) %>%
  ungroup() %>%
  left_join(aes2016_questions, by = c("variable" = "var_abb")) %>%
  mutate(item = gsub("Do you strongly .+\\? ", "", question_label)) %>%
  filter(!is.na(value)) %>%
  group_by(variable, first_pref_sen_grp2, item) %>%
  mutate(negative = value %in% c("Disagree", "Strongly disagree"),
         positive = value %in% c("Agree", "Strongly agree"),
         same = !negative & !positive,
         prop = freq/sum(freq) * case_when(
           negative ~ -1,
           positive ~ 1,
           TRUE ~ 0.5)) %>%
  mutate(value = factor(value, levels = c("Strongly disagree", 
                                          "Disagree",
                                          "Strongly agree",
                                          "Agree",
                                          "Neither agree nor disagree"))) %>%
  ungroup() %>%
  mutate(item = fct_reorder(item, prop, .fun = sum))

pal <- brewer.pal(5, "RdYlGn")
names(pal) <- levels(d5$value)[c(1,2,5,4,3)]

d5a <- d5 %>% 
  filter(negative | same) %>%
  mutate(prop = -abs(prop))

p5 <- d5 %>%
  ggplot(aes(x = item, fill = value, weight = prop)) +
  geom_bar(data = d5a, position = "stack") +
  geom_bar(data = filter(d5, positive | same), position = "stack") +
  facet_wrap(~first_pref_sen_grp2, ncol = 3) +
  coord_flip() +
  scale_fill_manual("", 
                    values = pal,
                    guide = guide_legend(reverse = FALSE),
                    breaks = levels(d5$value)[c(1,2,5,4,3)]) +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1), labels = c("100%", "50%", "0", "50%", "100%")) + 
  theme(panel.spacing = unit(1.5, "lines")) +
  expand_limits(y = c(-1, 1)) +
  ggtitle("Attitudes to liberal-conservative social issues, by first preference Senate vote in 2016",
          "Do you strongly agree ... or strongly disagree with the following statement?") +
  labs(x = "", y = "", caption = "Source: Australian Election Study 2016; analysis by freerangestats.info.")

svglite("../img/0152-agree-social.svg", 11, 8)
print(p5)
dev.off()

#----------constitutional knowledge----------
d6 <- aes2016 %>%
  select(F10_1:F10_6, wt_enrol, first_pref_sen_grp2) %>%
  gather(variable, value, -wt_enrol, -first_pref_sen_grp2) %>%
  group_by(variable, value, first_pref_sen_grp2) %>%
  summarise(freq = sum(wt_enrol)) %>%
  ungroup() %>%
  left_join(aes2016_questions, by = c("variable" = "var_abb")) %>%
  mutate(item = gsub("Do you think .+\\? ", "", question_label),
         item = str_wrap(item, 50)) %>%
  filter(!is.na(value)) %>%
  group_by(variable, first_pref_sen_grp2, item) %>%
  mutate(negative = value %in% c("False"),
         positive = value %in% c("True"),
         same = !negative & !positive,
         prop = freq/sum(freq) * case_when(
           negative ~ -1,
           positive ~ 1,
           TRUE ~ 0.5)) %>%
  mutate(value = factor(value, levels = c("False", 
                                          "True",
                                          "Don't know"))) %>%
  ungroup() %>%
  mutate(item = fct_reorder(item, prop, .fun = sum))

pal <- brewer.pal(3, "RdYlGn")
names(pal) <- levels(d6$value)[c(1,3,2)]

d6a <- d6 %>% 
  filter(negative | same) %>%
  mutate(prop = -abs(prop))

p6 <- d6 %>%
  ggplot(aes(x = item, fill = value, weight = prop)) +
  geom_bar(data = d6a, position = "stack") +
  geom_bar(data = filter(d6, positive | same), position = "stack") +
  facet_wrap(~first_pref_sen_grp2, ncol = 3) +
  coord_flip() +
  scale_fill_manual("", 
                    values = pal,
                    guide = guide_legend(reverse = FALSE),
                    breaks = names(pal)) +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1), labels = c("100%", "50%", "0", "50%", "100%")) + 
  theme(panel.spacing = unit(1.5, "lines")) +
  expand_limits(y = c(-1, 1)) +
  ggtitle("Knowledge of constitutional issues",
          "Do you think the following statement is true or false?
(correct answers in order from 'federation in 1901' to '75 members' are True, True, False, False, True, False)") +
  labs(x = "", y = "", caption = "Source: Australian Election Study 2016; analysis by freerangestats.info.")

svglite("../img/0152-knowledge.svg", 11, 8)
print(p6)
dev.off()


#======================selected bivariate============

#------------------tax/social services-------------
x <- svytable(~E1 + first_pref_sen_grp2, aes2016_svy)
x <- round(x, 2)
colnames(x)[grepl("OTHER", colnames(x))] <- "OTHER"
rownames(x) <- paste0(str_wrap(rownames(x), 20))

svglite("../img/0152-tax-mosaic.svg", 8, 6)
mosaic(x, shade = TRUE, border = "grey70", direction = "v", 
       legend = legend_resbased(10, fontfamily = "Roboto", pvalue = FALSE),
       xlab = "x", ylab = "y", 
       labeling = labeling_values(
         suppress = c(0, 1000),
         gp_labels = gpar(fontfamily = "Roboto", cex = 0.8),
         gp_varnames = gpar(col = "transparent"),
         just_labels = c("center", "right"),
         alternate_labels = c(TRUE, FALSE),
         rot_labels = c(0,90,0,0),
         offset_labels = c(1,0,1.5,0),
         digits = 0
       ))
dev.off()

#--------------how decide vote------------
x <- svytable(~B5 + first_pref_sen_grp2, aes2016_svy)
x <- round(x, 2)
colnames(x)[grepl("OTHER", colnames(x))] <- "OTHER"
rownames(x) <- paste0(str_wrap(rownames(x), 20))

svglite("../img/0152-decidin-vote-mosaic.svg", 8, 6)
mosaic(x, shade = TRUE, border = "grey70", direction = "v", 
       legend = legend_resbased(10, fontfamily = "Roboto", pvalue = FALSE),
       xlab = "x", ylab = "y", 
       labeling = labeling_values(
         suppress = c(0, 1000),
         gp_labels = gpar(fontfamily = "Roboto", cex = 0.8),
         gp_varnames = gpar(col = "transparent"),
         just_labels = c("center", "right"),
         alternate_labels = c(TRUE, FALSE),
         rot_labels = c(0,90,0,0),
         offset_labels = c(1,0,1.5,0),
         digits = 0
       ))
dev.off()

#=========modelling================
# H4 is ATSI status; H10 is whether own any investment properties; H17 is income

mod <- svyglm(tpp_alp ~ sex + H4 + H10 + H17 + finSTATE, design = aes2016_svy, 
              family = "quasibinomial")

anova(mod)


#=========wrap up===============
svgs <- list.files("../img/", pattern = "0152.+\\.svg", full.names = TRUE)
lapply(svgs, svg_googlefonts)

convert_pngs("0152")

thankr::shoulders() %>% 
  mutate(maintainer = str_squish(gsub("<.+>", "", maintainer)),
         maintainer = ifelse(maintainer == "R-core", "R Core Team", maintainer)) %>%
  group_by(maintainer) %>%
  summarise(`Number packages` = sum(no_packages),
            packages = paste(packages, collapse = ", ")) %>%
  arrange(desc(`Number packages`)) %>%
  knitr::kable() %>% 
  clipr::write_clip()


