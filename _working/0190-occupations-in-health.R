library(tidyverse)
library(readxl)
library(janitor)
library(scales)
library(RColorBrewer)
library(lubridate)

#==================Data management=======================

#----------Industry by occupation----------------
url <- "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&eq09.zip&6291.0.55.003&Data%20Cubes&0A68E700485EF985CA2585910016AF26&0&May%202020&25.06.2020&Latest"
download.file(url, "lfs-eq09.zip", mode = "wb")
unzip("lfs-eq09.zip")

eq09 <- read_excel("EQ09.xlsx", sheet = "Data 1", skip = 3) %>%
  clean_names() %>% 
  rename(industry1 = industry_division_of_main_job_anzsic_2006_rev_2_0,
         occupation1 = occupation_major_group_of_main_job_anzsco_2013_v1_2) %>%
  mutate(total_hours = 
           number_of_hours_actually_worked_in_all_jobs_employed_full_time_000_hours +
           number_of_hours_actually_worked_in_all_jobs_employed_part_time_000_hours)

#------------Detailed occupation-------------
url <- "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&eq08.zip&6291.0.55.003&Data%20Cubes&CB6124B8CB5B515DCA2585910016A499&0&May%202020&25.06.2020&Latest"
download.file(url, "lfs-eq08.zip", mode = "wb")
unzip("lfs-eq08.zip")

eq08 <- read_excel("EQ08.xlsx", sheet = "Data 1", skip = 3) %>%
  clean_names() %>%
  rename(
    occupation4 = occupation_of_main_job_anzsco_2013_v1_2,
    total_hours = number_of_hours_actually_worked_in_all_jobs_000_hours
  )

health_profs <- c(
 "2500 Health Professionals nfd"                                          ,
 "2510 Health Diagnostic and Promotion Professionals nfd"                 ,
 "2511 Nutrition Professionals"                                           ,
 "2512 Medical Imaging Professionals"                                     ,
 "2513 Occupational and Environmental Health Professionals"               ,
 "2514 Optometrists and Orthoptists"                                      ,
 "2515 Pharmacists"                                                       ,
 "2519 Other Health Diagnostic and Promotion Professionals"               ,
 "2520 Health Therapy Professionals nfd"                                  ,
 "2521 Chiropractors and Osteopaths"                                      ,
 "2522 Complementary Health Therapists"                                   ,
 "2523 Dental Practitioners"                                              ,
 "2524 Occupational Therapists"                                           ,
 "2525 Physiotherapists"                                                  ,
 "2526 Podiatrists"                                                       ,
 "2527 Audiologists and Speech Pathologists \\ Therapists"                ,
 "2530 Medical Practitioners nfd"                                         ,
 "2531 General Practitioners and Resident Medical Officers"               ,
 "2532 Anaesthetists"                                                     ,
 "2533 Specialist Physicians"                                             ,
 "2534 Psychiatrists"                                                     ,
 "2535 Surgeons"                                                          ,
 "2539 Other Medical Practitioners"                                       ,
 "2540 Midwifery and Nursing Professionals nfd"                           ,
 "2541 Midwives"                                                          ,
 "2542 Nurse Educators and Researchers"                                   ,
 "2543 Nurse Managers"                                                    ,
 "2544 Registered Nurses"
)

#---------------------mucking around with classifications-----------
# We will find the total hours by health professionals (codes above)...
health_profs_hours <- eq08 %>%
  filter(occupation4 %in% health_profs) %>%
  group_by(mid_quarter_month) %>%
  summarise(health_prof_hours = sum(total_hours)) %>%
  mutate(occupation1 = "Professionals")

# And subtract it from the Professionals in the Health Care and Social Assistance Industry,
# to get a data frame that has two types of professionals. Note this is problematic because
# of health professionals in other industries.
separated_profs <- eq09 %>%
  filter(industry1 == "Health Care and Social Assistance") %>%
  group_by(occupation1, mid_quarter_month) %>%
  summarise(total_hours = sum(total_hours)) %>%
  inner_join(health_profs_hours, by = c("mid_quarter_month", "occupation1")) %>%
  mutate(other_profs_hours = total_hours - health_prof_hours) %>%
  ungroup() %>%
  select(-total_hours, -occupation1) %>%
  gather(occupation, total_hours, -mid_quarter_month) %>%
  mutate(occupation = case_when(
    occupation == "health_prof_hours" ~ "Health Professionals",
    occupation == "other_profs_hours" ~ "Non-Health Professionals"
  )) 

# join back to the original data
d <- eq09 %>%
  filter(industry1 == "Health Care and Social Assistance" & occupation1 != "Professionals") %>%
  rename(occupation = occupation1) %>%
  group_by(occupation, mid_quarter_month) %>%
  summarise(total_hours = sum(total_hours)) %>%
  ungroup() %>%
  rbind(separated_profs) %>%
  mutate(occupation = fct_reorder(occupation, total_hours))

#==========Plotting============

#----------named palette and caption-------------

occ_palette <- brewer.pal(length(unique(d$occupation)), "Set1")
names(occ_palette) <- unique(d$occupation)

the_caption <- "Source: ABS Labour Force Survey EQ08 and EQ09, analysis by http://freerangestats.info"

#------------Line chart------------
# This is equivalent to an index - showing cumulative growth
p3 <- d  %>%
  mutate(yr = year(mid_quarter_month)) %>%
  group_by(yr, occupation) %>%
  summarise(total_hours = mean(total_hours)) %>%
  group_by(occupation) %>%
  arrange(yr) %>%
  mutate(growth = total_hours / total_hours[1] - 1) %>%
  ungroup() %>%
  mutate(occupation = fct_reorder(occupation, -growth, .fun = last)) %>%
  ggplot(aes(x = yr, y = growth, colour = occupation)) +
  scale_y_continuous(label = percent) + 
  geom_point(aes(size = total_hours / 1000)) +
  geom_line(stat="smooth", method = "loess", span = 1/2, alpha = 0.5, size = 2) +
  scale_colour_manual(values = occ_palette) +
  scale_size_area(label = comma_format(accuracy = 1)) +
  theme(legend.position = "right") +
  labs(x = "",
       y = "Cumulative growth in hours since 1986",
       size = "Million hours per quarter",
       colour = "Occupation",
       subtitle = "Hours by occupation of workers in Australia's health care and social assistance industry",
       title = "Strong growth in managers and non-health professionals, but absolute numbers are small",
       caption = the_caption)



#----------stacked and filled area charts-----------------

p <- d  %>%
  ggplot(aes(x = mid_quarter_month, y = total_hours / 1000, fill = occupation)) +
  scale_fill_manual(values = occ_palette) +
  theme(legend.position = "right") +
  labs(caption = the_caption,
       x = str_wrap("Caution: the split between health and non-health professionals has challenges and will
       overestimate the former at the expense of the latter, by including health professionals in
       other industries.", 120),
       fill = "Occupation",
       subtitle = "Hours by occupation of workers in Australia's health care and social assistance industry") +
  theme(axis.title.x = element_text(size = 9, hjust = 0, colour = "grey"))

# chart: stacked so we see absolute size
p2 <- p + geom_area(position = "stack") +
  scale_y_continuous(label = comma) +
  labs(y = "Millions of hours worked per quarter",
       title = "Steady growth over time")

# chart: filled to top, showing proportions
p1 <- p + geom_area(position = "fill") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  labs(y = "Proportion of hours worked",
       title = "More community and personal service workers and less labourers")


#==================Medical practitioners=======
# Out of curiousity, let's look more at the breakdown of those health professionals

med_prac <- c(
  "2530 Medical Practitioners nfd"                                         ,
  "2531 General Practitioners and Resident Medical Officers"               ,
  "2532 Anaesthetists"                                                     ,
  "2533 Specialist Physicians"                                             ,
  "2534 Psychiatrists"                                                     ,
  "2535 Surgeons"                                                          ,
  "2539 Other Medical Practitioners"                                       
)


profs_only <- eq08 %>%
  filter(occupation4 %in% health_profs) %>%
  mutate(med_prac = if_else(occupation4 %in% med_prac, "Medical practitioner", "Other health professional")) %>%
  group_by(mid_quarter_month, med_prac) %>%
  summarise(total_hours = sum(total_hours))

# chart: medical practitioners as a proportion of health professionals
p4 <- profs_only %>%
  ggplot(aes(x = mid_quarter_month, y = total_hours, fill = med_prac)) +
  geom_area(position = "fill") +
  scale_y_continuous(label = percent) +
  labs(x = str_wrap("Medical practitioners defined as GPs, Resident Medical Officers, Anaesthetists, Specialist Physicians,
       Psychiatrists, Surgeons, and other Medical Practitioners. 'Other health professional' examples includes nurses,
       pharmacists, midwives, nutritional practitioners, dental practitioners.", 120),
       fill = "",
       y = "Percentage of all health professionals' hours",
       subtitle = "Hours worked by all health professionals (unit group code 2500 to 2544)",
       title = "Medical practitioners' labour has remained a constant proportion of health professionals'",
       caption = the_caption) +
  theme(axis.title.x = element_text(size = 9, hjust = 0, colour = "grey"))



#==============save images==============
svg_png(p1, "../img/0190-stack", w = 10)
svg_png(p2, "../img/0190-fill", w = 10)
svg_png(p3, "../img/0190-line", w = 10)
svg_png(p4, "../img/0190-medical-practitioners", w = 10)
