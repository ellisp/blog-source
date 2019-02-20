library(tidyverse)
library(frs)
library(data.table)
library(odbc)
library(viridis)
library(ggthemes)
library(scales)

# correct citation must be used WV6_Data_R_v20180912; . 

# http://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp


wvs <- readRDS("../data/F00007762-WV6_Data_R_v20180912.Rds")

# label code from David Hood @Thoughtfulnz https://gist.github.com/thoughtfulbloke/8d6e016a74039c030ba3871dca36c19c

unlabel <- function(x){
  codes <- attr(x, "labels")
  if(is.null(codes)) {
    print(paste("No labels with",attr(x, "label")))
    return(x)
  }
  df <- data.frame(y = as.numeric(x), stringsAsFactors = FALSE)
  
  replacement <- data.frame(
    y = as.numeric(codes),
    newform = names(codes),
    stringsAsFactors = FALSE
  )
  df2 <- df %>% left_join(replacement, by="y") %>%
    mutate(newform = ifelse(is.na(newform), as.character(x), newform))
  if(length(x) == length(df2$newform) &
     sum(is.na(x)) == sum(is.na(df2$newform))){
    return(df2$newform)
  } else {
    print(paste("Problem with",attr(x, "label")))
    return(x)
  }
}

wv <- wvs %>% 
  mutate_all(unlabel) 

labs <-  wvs %>% 
  map_chr(~attributes(.)$label) %>% 
  make.names(unique = TRUE)

names(wv) <- labs
# End of David Hood's tidying code

#------------turn into a star schema--------------------
wv$respondent_id <- 1:nrow(wv)

respondent_vars <- c("Wave", "Country.Code", "Country.regions..with.split.ups.",
                 "Interview.number", "Study", "Unified.respondent.number", 
                 "X1000.equilibrated.weight", "X1500.equilibrated.weight",
                 "Country...wave...study...set...year", "Nation.Wave", "Nation.Year",
                 "COW.Country.Code", "Weight.for.overal.secular.values", 
                 "Weight.for.Emancipative.values", "Weight", "Weight.with.split.ups",
                 "Questionnaire.version", "Date.of.interview", "Survey.year", "respondent_id")


#--------long thin version that will be the basis of the main fact table
wvt <- wv[ , c(names(wv)[!names(wv) %in% respondent_vars] , "respondent_id")] %>%
  gather(question, response, -respondent_id) %>%
  mutate(question = as.factor(question),
         response = as.factor(response),
         question_id = as.integer(question),
         response_id = as.integer(response)) %>%
  # one mystery NA to Highest.educational.level.attained
  filter(!is.na(response))
# note that this version is 38 million rows long

d_questions <- wvt %>%
  distinct(question_id, question) %>%
  mutate(question = gsub("..", ": ", question, fixed = TRUE),
         question = gsub(".", " ", question, fixed = TRUE),
         question = str_squish(question),
         question = gsub("country s ", "country's ", question, fixed = TRUE),
         question = gsub("Mother s ", "Mother's ", question, fixed = TRUE),
         question = gsub("Father s ", "Father's ", question, fixed = TRUE)) %>%
  separate(question, into = c("super_broad", "very_broad", "broad_question", "mid_question", "narrow_question"), 
           fill = "left", remove = FALSE, sep = ":") %>%
  mutate(narrow_question = str_squish(narrow_question),
         mid_question = str_squish(mid_question),
         broad_question = str_squish(broad_question))

d_responses <- wvt %>%
  distinct(response_id, response) %>%
  # cleaning
  mutate(response = str_squish(gsub("Dont ", "Don't ", response))) %>%
  # creating alternative versions of some of the responses:
  mutate(response_no_commas = gsub(",", "", response, fixed = TRUE),
         response_no_spaces = gsub(" ", "", response_no_commas, fixed = TRUE),
         response_lower_case = tolower(response),
         response_numeric = suppressWarnings(as.numeric(response_no_spaces)),
         response_any_numerals = ifelse(is.na(response_numeric),
                                        str_extract(response_no_spaces, "[0-9]+"),
                                        response_numeric),
         response_class = ifelse(is.na(response_numeric), "character", "numeric"),
         
         agrees = as.numeric(response_lower_case %in% 
                               c("strongly agree", "agree", "completely agree", "agree strongly")),
         
         important = as.numeric(response_lower_case %in% 
                                  c("very important", "important", "absolutely important", "rather important")),
         
         trust = as.numeric(response_lower_case %in% 
                              c("trust somewhat", "trust completely", "most people can be trusted")),
         
         often = as.numeric(response_lower_case %in%
                              c("fairly often", "very often", "often", "quite frequently", "very frequently", "frequently")),
         
         like_me = as.numeric(response_lower_case %in%
                                c("very much like me", "like me", "somewhat like me", "a little like")),
         
         interested = as.numeric(response_lower_case %in%
                                   c("somewhat interested", "very interested", "respondent was somewhat interested",
                                     "respondent was very interested")),
         
         satisfied = as.numeric(response_lower_case %in%
                                  c("completely satisfied", "fairly satisfied", "very satisfied", "strongly satisfied")),
         
         happy = as.numeric(response_lower_case %in%
                              c("rather happy", "very happy")),
         
         respect = as.numeric(response_lower_case %in%
                                c("fairly much respect")),
         
         justifiable = as.numeric(response_lower_case %in%
                                    c("Always justifiable")),
         
         invalid = as.numeric(response_lower_case %in% 
                                c("no answer") | 
                                grepl("not asked", response_lower_case) |
                                grepl("don't know", response_lower_case) |
                                grepl("unsure", response_lower_case) |
                                grepl("unknown", response_lower_case) |
                                grepl("inapplicable", response_lower_case) |
                                grepl("dropped out survey", response_lower_case) |
                                grepl("inappropriate response", response_lower_case) |
                                grepl("missing", response_lower_case) |
                                grepl("not applicable", response_lower_case))
         
         
         
         ) %>%
  
  select(response_id, everything())

d_responses %>%
  filter(grepl("inappropriate", response_lower_case)) %>%
  select(response, important, agrees, satisfied, respect, invalid) 

d_respondents <- wv[ , respondent_vars] %>%
  rename_all(tolower) %>%
  rename_all(function(x){gsub("\\.+", "_", x)}) %>%
  select(respondent_id, everything()) %>%
  mutate(weight = ifelse(weight == "No weighting", 1, as.numeric(weight)))

f_wvs <- wvt %>%
  select(respondent_id, question_id, response_id)


#-----------upload to database--------------
fwrite(d_respondents[ , c("respondent_id", "wave", "country_code", "weight", "survey_year")], 
       "d_respondents.txt", sep = "|", col.names = FALSE)

bcp("localhost", "survey_microdata", "wvs", table = "d_respondents", file = "d_respondents.txt")


fwrite(d_questions[ , c("question_id", "question")], 
       "d_questions.txt", sep = "|", col.names = FALSE)

bcp("localhost", "survey_microdata", "wvs", table = "d_questions", file = "d_questions.txt")

fwrite(d_responses, "d_responses.txt", sep = "|", col.names = FALSE)

bcp("localhost", "survey_microdata", "wvs", table = "d_responses", file = "d_responses.txt")

# this is astonishly fast - less than a second to write 38 million rows to disk
fwrite(f_wvs, "f_wvs.txt", sep = "|", col.names = FALSE)

# this is more like a few minute. As well as writing to the database server disk,
# it's checking for uniqueness of primary keys and the foreign key constraints.
bcp("localhost", "survey_microdata", "wvs", table = "f_wvs", file = "f_wvs.txt")


#-----------------analysis--------------

# 40 most commonly used responses:
f_wvs %>%
  group_by(response_id) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  slice(1:4000) %>%
  inner_join(d_responses, by = "response_id") %>% View
  mutate(response = fct_reorder(response, freq)) %>%
  ggplot(aes(x = freq, y = response)) +
  geom_point()

# uses of integers from 0 to 30:
f_wvs %>%
  group_by(response_id) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  inner_join(filter(d_responses, response_numeric %in% 0:30), by = "response_id") %>%
  mutate(response = fct_reorder(response, response_numeric)) %>%
  filter(freq > 0) %>%
  ggplot(aes(x = freq, y = response)) +
  geom_point()


# number of agreeing responses. This of course mostly reflects the number of times a question
# was asked and the valid responses, not the actual amount of agreement
d_responses %>%
  filter(agrees == 1) %>%
  inner_join(f_wvs, by = "response_id") %>%
  inner_join(d_questions, by = "question_id") %>%
  group_by(question) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  mutate(question = fct_reorder(question, freq)) %>%
  ggplot(aes(x = freq, y = question)) +
  geom_point()


# more useful lets look at the amount of agreeing going on
agree_questions <- d_responses %>%
  filter(agrees == 1) %>%
  inner_join(f_wvs, by = "response_id") %>%
  inner_join(d_questions, by = "question_id") %>%
  distinct(question, question_id)



# unweighted responses here
f_wvs %>%
  inner_join(agree_questions, by = "question_id") %>%
  inner_join(d_responses, by = "response_id") %>%
  group_by(question) %>%
  summarise(agrees = mean(agrees)) %>%
  mutate(question = fct_reorder(question, agrees)) %>%
  ggplot(aes(x = agrees, y = question)) +
  geom_point()

# To weight them we need to do an inner join of my 5 million facts (after filtering) with my 90,000 row respondent dimension table
# and this is too much for my computer


sql <- "
WITH agree_questions AS
  (SELECT	DISTINCT question, c.question_id
   FROM wvs.d_responses AS a
   INNER JOIN wvs.f_wvs AS b
     ON a.response_id = b.response_id
   INNER JOIN wvs.d_questions AS c
     ON b.question_id = c.question_id
   WHERE agrees = 1)
SELECT
  sum(g.weight * f.agrees) / sum(g.weight) AS weighted_agree,
  question,
  country_code
FROM wvs.f_wvs AS d
INNER JOIN agree_questions AS e
  ON d.question_id = e.question_id
INNER JOIN wvs.d_responses AS f
  ON d.response_id = f.response_id
INNER JOIN wvs.d_respondents AS g
  ON d.respondent_id = g.respondent_id
WHERE f.invalid != 1
GROUP BY question, country_code
ORDER by weighted_agree DESC"

con <- dbConnect(odbc(), "sqlserver", database = "survey_microdata")

d <- dbGetQuery(con, sql)

d2 <- d %>%
  as_tibble() %>%
  mutate(question = fct_reorder(question, weighted_agree)) %>%
  mutate(country_code = fct_reorder(country_code, weighted_agree)) 

yup <- length(unique(d2$question))

p <- d2 %>%
  ggplot(aes(x = country_code, y = question)) +
  theme_tufte(base_family = "main_font") +
  geom_tile(aes(fill = weighted_agree)) +
  scale_fill_viridis("Weighted percentage of valid answers that agreed or strongly agreed:", label = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.position = "bottom",
        axis.text.y = element_text(size = 7)) +
  labs(x = "The countries that 'agree' with more statements are on the right.",
       y = "The statements most agreed with (when asked) are at the top.\n",
       caption = "Source: http://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp, WV6_Data_R_v20180912. Analysis by http://freerangestats.info") +
  # draw the countries along the top as a sort of repeat of the x axis:
  geom_text(data = distinct(d2, country_code), aes(label = country_code), 
            y = yup + 1, angle = 45, hjust = 0, size = 2.7) +
  expand_limits(y = yup + 9, x = length(unique(d2$country_code)) + 3) +
  ggtitle("What do people of the world agree with?",
          "All questions in the World Values survey with an agree/disagree style of response.")
  
CairoSVG("../img/0143-heatmap.svg", 18, 9.5)
print(p)
dev.off()

convert_pngs("0143")
