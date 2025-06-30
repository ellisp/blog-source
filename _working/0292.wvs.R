library(tidyverse)
library(frs)
library(data.table)
library(odbc)
library(viridis)
library(ggthemes)
library(scales)


# http://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp
# This time we should use the time series!

unzip("F00011932-WVS_Time_Series_1981-2022_Rdata_v5_0.zip")

load("WVS_Time_Series_1981-2022_Rdata_v5_0.rdata")

wvs <- `WVS_Time_Series_1981-2022_v5_0`
dim(wvs)

# full variable labels:
attributes(wvs)[[3]][1:10]

variables <- tibble(
  col_num = 1:ncol(wvs),
  tibble_name = names(wvs),
  attribute_name = attributes(wvs)[['names']],
  full_label = attributes(wvs)[[3]]
)

View(variables)

variables |> mutate(same = tibble_name == attribute_name) |>  count(same)



# see on Welzel secular and emancipative values
# https://www.worldvaluessurvey.org/WVSContents.jsp?CMSID=welzelidx&CMSID=welzelidx

filter(variables, grepl("sex", full_label, ignore.case = TRUE)) |> pull(full_label)
# [1] "Neighbours: Homosexuals"                                                             
# [2] "Sharing with partner: sexual attitudes"                                              
# [3] "Sharing with parents: sexual attitudes"                                              
# [4] "Enjoy sexual freedom"                                                                
# [5] "Important for succesful marriage: Happy sexual relationship"                         
# [6] "Homosexual couples are as good parents as other couples"                             
# [7] "Churches speak out on: homosexuality"                                                
# [8] "Justifiable: Homosexuality"                                                          
# [9] "Justifiable: Having casual sex"                                                      
# [10] "Justifiable: Sex under the legal age of consent"                                     
# [11] "Justifiable: Sex before marriage"                                                    
# [12] "How frequently do the following things occur in your neighborhood: Sexual harassment"
# [13] "Sex"                                                                                 
# [14] "HOMOLIB- Welzel choice-1: Homosexuality acceptance" 

filter(variables, grepl("gender", full_label, ignore.case = TRUE)) |> pull(full_label)
# [1] "WOMJOB- Welzel equality-1: Gender equality: job"      
# [2] "WOMPOL- Welzel equality-2: Gender equality: politics" 
# [3] "WOMEDU- Welzel equality-3: Gender equality: education"

filter(variables, grepl("wife", full_label, ignore.case = TRUE)) |> pull(full_label)
# [1] "Being a housewife just as fulfilling"                                                             
# [2] "Husband and wife should both contribute to income"                                                
# [3] "Traits in a woman: Woman good wife"                                                               
# [4] "More than one wife"                                                                               
# [5] "Wife must obey"                                                                                   
# [6] "By requiring man treat all wives equally, Islam true intent is prohibit taking more than one wife"
# [7] "Justifiable: For a man to beat his wife"


filter(variables, grepl("girl", full_label, ignore.case = TRUE)) |> pull(full_label)
# [1] "If only one child allowed: boy or girl"                 
# [2] "University is more important for a boy than for a girl"


filter(variables, grepl("mother", full_label, ignore.case = TRUE)) |> pull(full_label)
# [1] "Abortion when the mothers health is at risk"                                                        
# [2] "Child needs a home with father and mother"                                                          
# [3] "Relationship working mother"                                                                        
# [4] "Pre-school child suffers with working mother"                                                       
# [5] "Traits in a woman: Woman good mother"                                                               
# [6] "When you were growing up, did your father or mother have more influence in the affairs of the house"
# [7] "Mother immigrant"                                                                                   
# [8] "Mother's Country of origin"                                                                         
# [9] "Mothers country of birth - ISO 3166-1 code"                                                         
# [10] "Highest educational level attained - Respondent´s Mother ISCED"                                     
# [11] "Highest educational level attained - Respondent´s Mother (Recoded)"  


filter(variables, grepl("abortion", full_label, ignore.case = TRUE)) |> pull(full_label)
# [1] "Abortion when the mothers health is at risk"   
# [2] "Abortion when child physically handicapped"    
# [3] "Abortion when woman not married"               
# [4] "Abortion if not wanting more children"         
# [5] "Churches speak out on: abortion"               
# [6] "Justifiable: Abortion"                         
# [7] "ABORTLIB- Welzel choice-2: Abortion acceptable"

filter(variables, grepl("welzel", full_label, ignore.case = TRUE)) |> select(tibble_name, full_label) |>  print(n=35)
# 1 Y010        SACSECVAL.- Welzel Overall Secular Values                     
# 2 Y011        DEFIANCE.- Welzel defiance sub-index                          
# 3 Y012        DISBELIEF.- Welzel disbelief sub-index                        
# 4 Y013        RELATIVISM.- Welzel relativism                                
# 5 Y014        SCEPTICISM.- Welzel scepticism index                          
# 6 Y020        RESEMAVAL.- Welzel emancipative values                        
# 7 Y022        EQUALITY.- Welzel equality sub-index                          
# 8 Y023        CHOICE.- Welzel choice sub-index                              
# 9 Y024        VOICE.- Welzel voice sub-index                                
# 10 Y011A       AUTHORITY - Welzel defiance - 1: Inverse respect for authority
# 11 Y011B       NATIONALISM - Welzel defiance - 2: Inverse national pride     
# 12 Y011C       DEVOUT- Welzel defiance - 3: Inverse devoutness               
# 13 Y012A       RELIGIMP - Welzel disbelief- 1: Inverse importance of religion
# 14 Y012B       RELIGBEL - Welzel disbelief- 2: Inverse religious person      
# 15 Y012C       RELIGPRAC - Welzel disbelief- 3: Inverse religious practice   
# 16 Y013A       NORM1 - Welzel relativism- 1: Inverse norm conform1           
# 17 Y013B       NORM2 - Welzel relativism- 2: Inverse norm conform2           
# 18 Y013C       NORM3 - Welzel relativism- 3: Inverse norm conform3           
# 19 Y014A       TRUSTARMY- Welzel scepticism- 1: Inverse trust in army        
# 20 Y014B       TRUSTPOLICE- Welzel scepticism- 2: Inverse trust in police    
# 21 Y014C       TRUSTCOURTS- Welzel scepticism- 3: Inverse trust in courts    
# 22 Y021A       INDEP- Welzel autonomy-1: Independence as kid quality         
# 23 Y021B       IMAGIN- Welzel autonomy-2: Imagination as kid quality         
# 24 Y021C       NONOBED- Welzel autonomy-3: Obedience as kid quality          
# 25 Y022A       WOMJOB- Welzel equality-1: Gender equality: job               
# 26 Y022B       WOMPOL- Welzel equality-2: Gender equality: politics          
# 27 Y022C       WOMEDU- Welzel equality-3: Gender equality: education         
# 28 Y023A       HOMOLIB- Welzel choice-1: Homosexuality acceptance            
# 29 Y023B       ABORTLIB- Welzel choice-2: Abortion acceptable                
# 30 Y023C       DIVORLIB- Welzel choice-3: Divorce acceptable                 
# 31 Y024A       VOICE1- Welzel voice-1                                        
# 32 Y024B       VOICE2- Welzel voice-2                                        
# 33 Y024C       VOI2_00- Welzel voice-3 (auxiliary)         
  
plot(density(wvs$Y023A, na.rm = TRUE))
table(as.character(wvs$Y023A)) # only 10 actual possible values, from 0 to 1

wvs |> 
  select(year = S020,
         wt = S017,
         hsa = Y023A) |> 
  drop_na() |> 
  # group by year of survey
  group_by(year) |> 
  # average homosexuality acceptance
  summarise(hsa =  sum(hsa * wt) / sum(wt),
            n = n(),
            pop = sum(wt)) |> 
  ggplot(aes(x = year, y = hsa)) +
  geom_point(aes(size = n)) +
  geom_smooth(method = "gam") +
  labs(size = "Sample size",
       x = "",
       y = "Welzel choice-1: Homosexuality acceptance")



hsa <- wvs |> 
  select(year = S020,
         wt = S017,
         hsa = Y023A,
         country = COUNTRY_ALPHA) |> 
  drop_na() |> 
  # group by year of survey
  group_by(country, year) |> 
  # average homosexuality acceptance
  summarise(hsa =  sum(hsa * wt) / sum(wt),
            n = n(),
            pop = sum(wt)) 

hsa |> 
  ggplot(aes(x = year, y = hsa, colour = country)) +
  geom_point(aes(size = n)) +
  geom_path() +
  labs(size = "Sample size",
       x = "",
       y = "Welzel choice-1: Homosexuality acceptance") +
  theme(legend.position = "none")


mod <- lm(hsa ~ year + country, data = hsa)
anova(mod)
confint(mod)[1:2, ]
# increasing about 0.007 per year


# dimensions of all the attributes
sapply(attributes(wvs), length)

# codes eg -5 "Unknown" for column 9
attributes(wvs)[[6]][1:10]

cbind(
  code = as.numeric(attributes(wvs)[[6]][[9]]),
  label = names(attributes(wvs)[[6]][[9]])
)

lab <- function(col_num){
  cbind(
    code = as.numeric(attributes(wvs)[[6]][[col_num]]),
    label = names(attributes(wvs)[[6]][[col_num]]),
    column_name = attributes(wvs)[[3]][col_num],
    col_num = col_num
  )
}

lab(90)

lab(1001)

lab(1046)
lab(1045)
dim(wvs)


