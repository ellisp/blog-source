---
layout: post
title: What the world agrees with
date: 2019-01-26
tag: 
   - Surveys
   - Tools
   - R
description: I load wave 6 of the World Values Survey into a database so it's possible to analyse more questions and countries at once, and find some interesting variations in what people agree with in different parts of the world.
image: /img/0143-heatmap.svg
socialimage: https:/freerangestats.info/img/0143-heatmap.png
category: R
---

## A serious, decades-long attempt to understand different peoples' values

David Hood (@Thoughfulnz) has been posting some interesting snippets of analysis using the World Values Survey data (like [this example](https://twitter.com/Thoughtfulnz/status/1088339318752464897)). This inspired me to have a look at the data myself; something that's been on my to-do list for years. I have analysed it before, but a long while ago and for only a single specific purpose, and I wanted to get a more general overview of it.

The [World Values Survey](http://www.worldvaluessurvey.org/WVSContents.jsp) is an amazing multi-decade (started in 1981) network of scientists doing their best to create standard, internationally comparable, nationally representative data on values and their impact on social and political life.

It's an impressive resource, with six waves of data already published and a seventh in preparation. I've only had a few hours today to look at it and that's enough to make me want to come back. But let's start with my finishing point for today - a big graphic of around 40 of the questions from the various surveys, showing how much people in different countries agree with various statements:

<a href = "/img/0143-heatmap.svg"  target="_blank"><img src='/img/0143-heatmap.svg' width='100%'></a>

It really needs a big screen of course. Click on the image to open it in its own tab of the browser.

I find this fascinating and hope you do to. It's a graphic that repays close attention, and the more I look at it the more interesting snippets I find. The ordering of the graphic with the most "agreed-with" statements at the top and the most "agreeing" countries on the right is crucial. It means that you can look up or across the graphic and look the squares of unusual colour that indicate exceptions. Here's just some of the things that I noticed:

* The United States stands out for its residents agreement with "Under some conditions, war is necessary to obtain justice"
* The people of Hong Kong don't agree that they are autonomous individuals
* The people of Sweden are inclined to agree that all religions should be taught in public schools; and to disagree that "when jobs are scarce, employers should give priority to people of this country:
* Residents of former Soviet Union countries (Belarus, Russia, Kazakhstan, Ukraine) stand out for not agreeing with "I see myself as part of my local community"

## Data wrangling

I have in mind the plan to eventually grab all six waves of the World Values Survey, as the real benefit of this sort of data comes not just from cross-country comparisons but comparisons over time. I also wanted a way of wrangling the data that would lend itself to efficient analysis of multiple questions at once. Both of these needs pushed me away from the common way of treating survey with one row per respondent, one column per question to a more efficient data model as used in data warehousing.

At first I did this all in R, but then I found that when I got to the analysis stage and wanted to join my 38 million row fact table to the 90,000 row dimension table I ran out of memory. I needed a database so that hard disk could be used when the going gets tough, and to draw on all the optimisation built into that software for exactly this job. So I ended up with this toolchain (which is a very common one for me):

* data import, cleaning and some normalising in R
* upload into a database with the right disciplines and constraints, and (sometimes, not today) finishing off the cleaning and normalising
* for analysis, do the heavy lifting in SQL queries but bring filtered or aggregated data into R for any statistical modelling or for presentation.

Here's the R code for the first step. It draws first on code by David Hood for extracting the infromation on questions and answers that was encoded in the original file as attributes; then moves on to split the data into four tables representing respondents, questions, possible responses and (finally) the 38 million combinations of respondent - question - response combinations in a single long, thin table.

{% highlight R lineanchors %}
library(tidyverse)
library(frs)
library(data.table)
library(odbc)
library(viridis)
library(ggthemes)
library(scales)

# correct citation must be used WV6_Data_R_v20180912; . 


# Obtain data from here:
# http://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp
# including filling in a form re appropriate use etc.

# load in data:
wvs <- readRDS("../data/F00007762-WV6_Data_R_v20180912.Rds")

#--------------extract labels and questions-----------
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

#=============turn into a star schema===================
# we need to make our own ID because nothing in the data seems to be a unique identifier
wv$respondent_id <- 1:nrow(wv)

# Define the variables that will eventually be in the respondent dimension table.
# This is things about the respondent and the interview and wave, rather than questions
# they've answered.
respondent_vars <- c("Wave", "Country.Code", "Country.regions..with.split.ups.",
                 "Interview.number", "Study", "Unified.respondent.number", 
                 "X1000.equilibrated.weight", "X1500.equilibrated.weight",
                 "Country...wave...study...set...year", "Nation.Wave", "Nation.Year",
                 "COW.Country.Code", "Weight.for.overal.secular.values", 
                 "Weight.for.Emancipative.values", "Weight", "Weight.with.split.ups",
                 "Questionnaire.version", "Date.of.interview", "Survey.year", "respondent_id")


#--------long thin version that will be the basis of the main fact table----------
# we want all the original columns (ie questions), *except* those
# that are going to end up in the respondent dimension table. But
# we need respondent_id as that is going to link the two.
wvt <- wv[ , c(names(wv)[!names(wv) %in% respondent_vars] , "respondent_id")] %>%
  gather(question, response, -respondent_id) %>%
  mutate(question = as.factor(question),
         response = as.factor(response),
         question_id = as.integer(question),
         response_id = as.integer(response)) %>%
  # one mystery NA to Highest.educational.level.attained
  filter(!is.na(response))
# note that this version is 38 million rows long

# extract the questions and do a bit of cleaning:
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

# extract the responses and do some cleaning and add some 
# classifications that might be useful:		 
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


# extract all the individual responses  
d_respondents <- wv[ , respondent_vars] %>%
  rename_all(tolower) %>%
  rename_all(function(x){gsub("\\.+", "_", x)}) %>%
  select(respondent_id, everything()) %>%
  mutate(weight = ifelse(weight == "No weighting", 1, as.numeric(weight)))

  
# final version of the factless fact table, with no information other than the combinations
# of respondent, question and response:
f_wvs <- wvt %>%
  select(respondent_id, question_id, response_id)
{% endhighlight %}

One of the key things I've done here is coded some of the responses with new classification columns such as `agree` - which is 1 for every response that is some kind of partly or fully agree, and 0 otherwise. This will let us analyse the multiple questions that use variants of this response set efficiently down the track.  The `invalid` column for every variant on "not asked", "dropped out", "don't know", is also important.

## Creating a database

### Defining a data model

The data model I've split the data out into is basically this:

<img src='/img/0143-schema.png' width='100%'>

This is the classic data warehousing approach to this sort of data. A long, thin fact table; and shorter, wider dimension tables that hold all the text information and slowly changing information about the entities such as people, questions, etc.  For those interested in these things, `f_wvs` is a good example of a "factless fact table" - meaning that other than indicating unique combinations of the three dimensions (*this respondent* gave *this response* to *this question*) it has no additional numeric information or "facts".

There are many many advantages of this data model; as well as being convenient for analysis down the track, one of the advantages is that we can do cleaning and enhancements of the dimension data in a compact way. In this case, I can add classifications to the responses in the 28,000 row `d_responses` table much more efficiently (with each unique response represented only once) than if I'd tried to do it to the original data, where each response is repeated each time someone in the original survey ticked it. And those columns of 1s and 0s with names like `agree` and `invalid` are going to become crucial for efficient analysis down the track.

Here's the SQL that I ran in SQL Server to set up that simple empty `wvs` schema ready to accept the data from R:

{% highlight SQL lineanchors %}
USE survey_microdata

CREATE SCHEMA wvs;

DROP TABLE IF EXISTS wvs.f_wvs
DROP TABLE IF EXISTS wvs.d_respondents
DROP TABLE IF EXISTS wvs.d_responses
DROP TABLE IF EXISTS wvs.d_questions


CREATE TABLE wvs.d_respondents(
	respondent_id INT NOT NULL PRIMARY KEY,
	wave          NVARCHAR(63) NOT NULL,
	country_code  NVARCHAR(63) NOT NULL,
	weight        NUMERIC(20,17) NOT NULL,
	survey_year   SMALLINT
)


CREATE TABLE wvs.d_questions(
	question_id INT NOT NULL PRIMARY KEY,
	question    NVARCHAR(255) NOT NULL
)

-- response (second column below) isn't unique because of upper / lower case issues for agree strongly
CREATE TABLE wvs.d_responses(
	response_id            INT NOT NULL PRIMARY KEY,	
	response               NVARCHAR(255) NOT NULL,
	response_no_commas     NVARCHAR(255) NOT NULL,
	response_no_spaces     NVARCHAR(255) NOT NULL,
	response_lower_case    NVARCHAR(255) NOT NULL,
	response_numeric       NUMERIC(25, 17),
	response_any_numerals  VARCHAR(63),
	response_class         VARCHAR(63),
	agrees                 NUMERIC(2,1) NOT NULL,
	important              NUMERIC(2,1) NOT NULL,
	trust                  NUMERIC(2,1) NOT NULL,
	often                  NUMERIC(2,1) NOT NULL,
	like_me                NUMERIC(2,1) NOT NULL,
	interested             NUMERIC(2,1) NOT NULL,
	satisfied              NUMERIC(2,1) NOT NULL,
	happy                  NUMERIC(2,1) NOT NULL,
	respect                NUMERIC(2,1) NOT NULL,
	justifiable            NUMERIC(2,1) NOT NULL,
	invalid                NUMERIC(2,1) NOT NULL
	)

CREATE TABLE wvs.f_wvs (
	respondent_id INT NOT NULL,
	question_id	  INT NOT NULL,
	response_id   INT NOT NULL,
	FOREIGN KEY (respondent_id) REFERENCES wvs.d_respondents(respondent_id),
	FOREIGN KEY (question_id) REFERENCES wvs.d_questions(question_id),
	FOREIGN KEY (response_id) REFERENCES wvs.d_responses(response_id)
)

ALTER TABLE wvs.f_wvs ADD PRIMARY KEY (respondent_id, question_id);
{% endhighlight %}


### Uploading data from R

For larger datasets, my preferred approach to getting the data from R into a SQL Server database is:

* define the database tables in SQL (as done above)
* from R, save the tables as pipe-delimited text files with no columns. The `fwrite` function in `data.table` package is super-fast at doing this - less than a second for the 38 million rows in `f_wvs` in this case.
* upload the data from the local disk to the database server via SQL Server's `bcp` bulk import and export tool. This is a common enough task for me that I've made a convenience wrapper function `bcp()` in my [frs R package](https://github.com/ellisp/frs-r-package) of miscellaneous bits and pieces.

Here's how that upload process looks in R:

{% highlight R lineanchors %}
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

# this is more like a 15+ minutes. As well as writing to the database server disk,
# it's checking for uniqueness of primary keys and the foreign key constraints.
bcp("localhost", "survey_microdata", "wvs", table = "f_wvs", file = "f_wvs.txt")

{% endhighlight %}

## Analysis

After all this, the analysis is the easy and fun part. Because this post is already long enough I'm going to just show the creation of that graphic I started with. My workflow at this point is writing SQL queries in SQL Server Management Studio until I'm satisfied I've got the right filtering and aggregation; then I copy and paste that query into my R script so everything is in one place. If the SQL gets much more complicated than the below I'd want to save separate SQL scripts rather than copy them into R.

So here it is, code to pull down the appropriately weighted responses for all questions in all countries that have an "agree" part of the response. The code below is specific to my setup in that I have an ODBC data source name "sqlserver" which I use to connect to the localhost database; that was just set up in the Windows ODBC administrator by following the prompts. If you've got a remote server it gets a little more complicated.

{% highlight R lineanchors %}
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

d2 %>%
  ggplot(aes(x = country_code, y = question)) +
  theme_tufte(base_family = "main_font") +
  geom_tile(aes(fill = weighted_agree)) +
  scale_fill_viridis("Weighted percentage of valid answers that agreed or strongly agreed:", label = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.position = "bottom",
        axis.text.y = element_text(size = 7)) +
  labs(x = "The countries that 'agree' with more statements are on the right.",
       y = "The statements most agreed with (when asked) are at the top.\n",
       caption = "Source: http://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp, WV6_Data_R_v20180912. Analysis by https:/freerangestats.info") +
  # draw the countries along the top as a sort of repeat of the x axis:
  geom_text(data = distinct(d2, country_code), aes(label = country_code), 
            y = yup + 1, angle = 45, hjust = 0, size = 2.7) +
  expand_limits(y = yup + 9, x = length(unique(d2$country_code)) + 3) +
  ggtitle("What do people of the world agree with?",
          "All questions in the World Values survey with an agree/disagree style of response.")
{% endhighlight %}

I've ignored statistical issues so far, in particular dealing with sampling uncertainty and the complex survey nature of the data. But I'm all set up to do this when I need to (and I need to understand better the sampling design to do it properly), down the track at some point I hope.

That's all for now. But I'll definitely be coming back to this data!

