library(tidyverse)
library(survey)
library(tidyverse)
library(scales)
library(readxl)
library(janitor)
library(rmarkdown)
library(kableExtra)
library(knitr)
library(clipr)
library(RColorBrewer)

download.file("https://github.com/ellisp/blog-source/raw/master/data/NZ%20gunlaw%20survey%202019%20Sep%20supplementary%20tables.xlsx",
              destfile = "gun-law-survey-tables.xlsx", mode = "wb")

q1 <- "How strongly do you support or oppose strengthening New Zealand’s existing gun laws?"


orig1 <- read_excel("gun-law-survey-tables.xlsx", sheet = "Strengthening of exisiting laws",
                    skip = 1)

names(orig1)[1:2] <-  c("variable", "value")

clean <- function(x){
  x <- gsub("/", " ", x, fixed = TRUE)
  x <- gsub(" ", "_", str_squish(x), fixed = TRUE)
  x <- gsub("M.ori", "Maori", x)
  return(x)
}

freq1c <- orig1 %>%
  select(-`Total support`, -`Total oppose`) %>%
  fill(variable) %>%
  filter(!is.na(value)) %>%
  gather(answer, prop, -variable, -value, -n) %>%
  group_by(variable, value) %>%
  mutate(Freq = pmax(0.01, n * prop / 100)) %>%
  ungroup() %>%
  select(variable, value, answer, Freq) 

ans_levs <- c("Strongly oppose", "Somewhat oppose", "Neither support or oppose",
              "Unsure", "Strongly support", "Somewhat support")

p1 <- freq1c %>%
  filter(variable != "All") %>%
  mutate(answer = ordered(answer, levels = ans_levs)) %>%
  group_by(variable, value) %>%
  mutate(prop = Freq / sum(Freq)) %>%
  filter(!answer %in% c("Unsure", "Neither support or oppose")) %>%
  mutate(prop = ifelse(grepl("oppose", answer), -prop, prop)) %>%
  ungroup() %>%
  mutate(variable = gsub("_", " ", variable),
         value = fct_relevel(str_wrap(value, 30), 
                             c("Other ethnicity", "$50k-$100k", "Under $50k",
                               "Canterbury", "Wellington/ Wairarapa", "Lower North Is.", "Upper North Is.", "Auckland"), 
                             after = Inf)) %>%
  ggplot(aes(weight = prop, x = value, fill = answer)) +
  facet_wrap(~variable, scales = "free_y") +
  geom_bar(position = "stack") +
  coord_flip() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_fill_manual(breaks = c("Strongly oppose", "Somewhat oppose", 
                                  "Somewhat support", "Strongly support"),
                    values = brewer.pal(4, "Spectral")[c(1,2,4,3)]) +
  labs(y = "'Unsure' and 'Neither' responses omitted from chart, but not from calculation of percentages",
       x = "",
       fill ="",
       subtitle = q1,
       title = "Support and opposition to gun control in New Zealand")

frs::svg_png(p1, "../img/0159-barchart", 11, 8)


#--------------Ethnicity variables and unknowns (income etc)--------------------
freq1c %>%
  group_by(variable) %>%
  summarise(sum(Freq)) %>%
  kable() %>%
  kable_styling() %>%
  write_clip()

freq1a <- freq1c %>%
  mutate(variable = ifelse(variable == "Ethnicity", value, variable),
         variable = clean(variable),
         value = gsub("$100,000k", "$100k", value, fixed = TRUE)) 

ethnicity_vars <- freq1a %>%
  filter(variable == clean(value)) %>%
  distinct(variable) %>%
  pull(variable)

totals <- freq1a %>%
  filter(variable == "All") %>%
  select(answer, answer_total = Freq)


unknowns <- freq1a %>%
  filter(variable != "All") %>%
  group_by(variable, answer) %>%
  summarise(variable_total = sum(Freq)) %>%
  left_join(totals, by = "answer") %>%
  mutate(Freq = round(answer_total - variable_total, 1),
         value = ifelse(variable %in% ethnicity_vars, 
                        paste("Not", variable), 
                        paste("Unknown", variable))) %>%
  ungroup() %>%
  select(variable, value, answer, Freq) %>%
  filter(Freq > 0)

freq1b <- freq1a %>%
  filter(variable != "All") %>%
  rbind(unknowns) %>%
  mutate(Freq = round(Freq)) %>%
  filter(Freq > 0)

head(freq1b, 10) %>% kable() %>% kable_styling() %>% write_clip()
#-------------------Create population dataset of all possible combinations of variables---------
poss_answers1 <- freq1b %>%
  distinct(answer) %>%
  rename(value = answer) %>%
  mutate(variable = "q1")

all_vars <- freq1b %>%
  distinct(variable, value)  %>%
  rbind(poss_answers1) %>%
  group_by(variable) %>%
  mutate(sequence = 1:n()) %>%
  ungroup() 

all_vars_l <- with(all_vars, split(value, variable))

all_combos <- do.call(expand.grid, all_vars_l) %>%
  as_tibble() %>%
  mutate_all(as.character) %>%
  mutate(wt = 1) %>%
  filter(!(q1 == "Unsure" & Living_Situation == "Renting from Housing New Zealand or other social housing organisation")) %>%
  filter(!(q1 == "Unsure" & Region == "Wellington/ Wairarapa")) %>%
  filter(!(Gender == "Unknown Gender" & q1 %in% c("Neither support or oppose", "Somewhat oppose", "Unsure"))) %>%
  # remove people with more than 2 ethnicities, to save 1+ million impossible combinations:
  mutate(number_ethnicities = (Asian == "Asian") + 
           (NZ_European_Other_European == "NZ European / Other European") + 
           (NZ_Maori == "NZ Māori") + 
           (Other_ethnicity == "Other ethnicity") + 
           (Pasifika == "Pasifika")) %>%
  filter(number_ethnicities %in% 1:2) %>%
  select(-number_ethnicities)


# Create a list with 13 different population tibbles, for each combination of a variable with Q1
pops1 <- freq1b %>%
  group_split(variable) %>%
  lapply(., function(x){
    names(x)[2:3] <- c(unique(x$variable), "q1")
    x <- x[, -1]
    return(x)})

full_data <- all_combos

d1 <- svydesign(~1, weights = ~wt, data = full_data)

d2 <- rake(d1, sample = list(~Age + q1,
                             ~Asian + q1,
                             ~Dependent_children + q1,
                             ~Employment + q1,
                             ~Gender + q1,
                             ~Household_income + q1,
                             ~Living_Situation + q1,
                             ~NZ_European_Other_European + q1,
                             ~NZ_Maori + q1,
                             ~Other_ethnicity + q1,
                             ~Pasifika + q1,
                             ~Region + q1,
                             ~Rural + q1
),
population = list(pops1[[1]],
                  pops1[[2]],
                  pops1[[3]],
                  pops1[[4]],
                  pops1[[5]],
                  pops1[[6]],
                  pops1[[7]],
                  pops1[[8]],
                  pops1[[9]],
                  pops1[[10]],
                  pops1[[11]],
                  pops1[[12]],
                  pops1[[13]]))

full_data$wt <- weights(d2)
# we now have a tibble of all the possible combinations of various variables,
# and a set of weights that between them add up to the observed data. If we just
# wanted to simulate microdata that is consistent with the survey, the job is
# done. But what if we want to simulate the exact microdata?

head(full_data, 10) %>% kable() %>% kable_styling() %>% write_clip()

full_data %>%
  group_by(Gender, q1) %>%
  summarise(full_data_total = sum(wt)) %>%
  left_join(pops1[[5]]) %>%
  rename(original_survey_total = Freq)
  

#' Evaluate the dissimilarity of a current sample's marginal totals compared to 
#' those in the various margin "population" totals
#' 
#' @param latest_sample a sample to evaluate
#' @param pops a list of population data frames with three columns; first two are variables, third is Freq
#' @details provides the sum of the absolute differences of the marginal totals from the sample with those
#' in pops that it is trying to resemble
evaluate_dissim <- function(latest_sample, pops){
  total_discrepency <- 0
  
  for(j in 1:length(pops)){
    var_names <- names(pops[[j]])[1:2]
    x <- latest_sample[ , var_names] %>%
      group_by_all() %>%
      summarise(sample = n()) %>%
      left_join(pops[[j]], by = var_names) %>%
      mutate(Freq = replace_na(Freq, 0)) %>%
      mutate(discrepency = abs(sample - Freq)) %>%
      ungroup() %>%
      summarise(discrepency = sum(discrepency)) %>%
      pull (discrepency)
    
    total_discrepency <- total_discrepency + x
  }
  return(total_discrepency)
  
}

#' Improve a sample on basis of discrepency with a single set of marginal totals, while
#' not making it worse based on the other marginal totals it is matching
#'
#' @param latest_sample A sample that is trying to look like the original survey
#' @param marg_totals A single data frame of marginal totals with three columns, of which the first
#' two must be variables from full data and the third, Freq, is counts we are trying to match
#' @param full_data A dataset of all possible combinations that the sample is drawn from, 
#' with population weights
#' @param disc 
#' @param strict if TRUE, whether to insist the end result sample has the same number of rows
#' as the original latest_sample
#' @param pops A full set of all marginal totals (as opposed to marg_totals which is just one 
#' of them) - needed for evaluating how well candidate samples go compared to their aspiration.
#' @param verbose Whether or not to print out which row of the extra sample data is being tried
#' to replace an excess row in the sample
#' @param max_attempts the maximum number of candidate rows to try as a replacement for an excess
#' row in the sample
improve_rnd <- function(latest_sample, marg_totals, full_data, 
                   disc = 0.1, strict = FALSE, pops, verbose = TRUE,
                   max_attempts = 100){
  
  # initial discrepency:
  starting_disc <- evaluate_dissim(latest_sample, pops)
  
  # initial number of rows (useful for checking if we lose any later):
  starting_rows <- nrow(latest_sample)
  
  # variable names we are checking against for just this marginal total
  var_names <- names(marg_totals)[1:2]
  
  # in case we have any excesses recorded from previously, make this NULL
  latest_sample$excesses <- NULL
  
  # for convenience, renaming, and being able to go back if necessary, copy the current sample data:
  x <- latest_sample
  names(x)[names(x) == var_names[1]] <- "var1"
  names(x)[names(x) == var_names[2]] <- "var2"
  
  # identify which combinations of the variables listed in marg_data are in latest_sample in excess:
  new_excesses <- x %>%
    group_by(var1, var2) %>%
    summarise(sample_freq = n()) %>%
    full_join(marg_totals, by = c("var1" = var_names[1], "var2" = var_names[2])) %>%
    mutate(sample_freq = replace_na(sample_freq, 0)) %>%
    mutate(excesses = sample_freq - Freq) %>%
    select(var1, var2, excesses)
  
  names(new_excesses)[1:2] <- var_names
  
  y <- latest_sample %>%
    left_join(new_excesses, by = var_names) 
  
  under_rep_vars <- new_excesses %>%
    ungroup() %>%
    filter(excesses < -disc)
  
  # Create and sort the under-represented rows of original data, with those that
  # are more under-represented more likely to be at the top
  under_rep_data <- full_data %>%
    inner_join(under_rep_vars, by = var_names) %>%
    mutate(wt = wt * abs(excesses),
           id = runif(n() * wt)) %>%
    arrange(desc(id)) %>%
    select(-id) %>%
    slice(1:max_attempts)
  
  if(nrow(under_rep_data) > 0){
    
    z <- y %>%
      # knock off one of the worst rows with too many excesses of some variable:
      arrange(desc(jitter(excesses))) %>%
      slice(-1)
    
    # cycle through all the possible candidates to replace a row of our sample with
    # one that is under represented, and choose the first one that reduces the overall
    # discrepency between our marginal totals and the ones we are aiming for:
    for(i in 1:nrow(under_rep_data)){
      if(verbose){cat(i)}
      
      candidate_sample <- z  %>%
        # replace it with a row from our under-represented set of data:
        rbind(under_rep_data[i, ])
      
      # evaluate our candidate.
      new_disc <- evaluate_dissim(candidate_sample, pops)
      
      # Have we made things worse for the other variables by trying to fix it for this one?:
      if(new_disc < starting_disc){
        # If things are better, we take this sample and break out of the loop
        latest_sample <- candidate_sample
        break()
      }
      # if things aren't better, we keep cycling through under_rep_data until we find one that is
    }
  } else {
    # if there's nothing to replace:
    new_disc <- starting_disc
  }
  
  if(strict){
    if(nrow(latest_sample) != starting_rows){
      print(c(nrow(latest_sample), starting_rows))
      stop("Somehow gained or lost some rows")
    }
  }
  
  return(list(latest_sample, 
              new_disc))
  
}

# we can draw a sample from the set of all combinations
set.seed(877)
latest_sample <- sample_n(full_data, 1000, replace = TRUE, weight = wt)

latest_sample %>%
  group_by(Gender, q1) %>%
  summarise(latest_sample_total = n()) %>%
  left_join(pops1[[5]]) %>%
  rename(original_sample_total = Freq)

discrepencies <- evaluate_dissim(latest_sample, pops1)
# best result from random version all the way is about 344
while(min(discrepencies) > 1200){
  
  # cycle through each of our sets of marginal totals (in random order), 
  # looking to improve the match if we can
  for(i in sample(1:length(pops1))){
    y <- latest_sample %>%
      improve_rnd(marg_totals = pops1[[i]], full_data = full_data, disc = 0.2,
             pops = pops1, max_attempts = 5, verbose = FALSE) 
    
    latest_sample <- y[[1]]
    discrepencies <- c(discrepencies, y[[2]])
    
    
    
  }
  
  # at the end of a cycle of variables, print how well we're going at improving things:
  print(min(discrepencies))
  
}

#-----------------Alternative approach - brute force, one variable at a time----

#' @details This method changes the sample by seeking a way to flick over var1 to a new value
#' that will improve the marginal combinations of var1 and var2 while leaving all other variables
#' unchanged. Unlike improve_rnd, which swaps out whole rows at a time.
improve_fix <- function(latest_sample, marg_totals, full_data, pops){
  
  # initial discrepency:
  starting_disc <- evaluate_dissim(latest_sample, pops)
  
  # variable names we are checking against for just this marginal total
  var_names <- names(marg_totals)[1:2]
  
  # in case we have any excesses recorded from previously, make this NULL
  latest_sample$excesses <- NULL
  latest_sample$id <- 1:nrow(latest_sample)
  
  # for convenience, renaming, and being able to go back if necessary, copy the current sample data:
  x <- latest_sample
  names(x)[names(x) == var_names[1]] <- "var1"
  names(x)[names(x) == var_names[2]] <- "var2"
  
  # identify which combinations of the variables listed in marg_data are in latest_sample in excess:
  changes_possible <- x %>%
    group_by(var1, var2) %>%
    summarise(sample_freq = n()) %>%
    full_join(marg_totals, by = c("var1" = var_names[1], "var2" = var_names[2])) %>%
    mutate(sample_freq = replace_na(sample_freq, 0)) %>%
    mutate(excesses = jitter(sample_freq - Freq)) %>%
    filter(round(abs(excesses)) > 0) %>%
    select(var1, var2, excesses) %>%
    ungroup()
  
  candidate_var2 <- changes_possible %>%
    group_by(var2) %>%
    filter(min(excesses) < 0 & max(excesses) > 0) %>%
    select(-excesses)
    
  changes_needed <- changes_possible %>%
    inner_join(candidate_var2, by = c("var1", "var2")) %>%
    arrange(desc(rnorm(n()))) %>%
    filter(var2 == var2[1]) %>%
    filter(excesses %in% range(excesses)) %>%
    mutate(excesses = round(excesses))
 
  names(changes_needed)[1:2] <- var_names
   
  number_changes <- min(abs(pull(changes_needed, excesses)))
  change_from <- filter(changes_needed, excesses > 0) %>% select(-excesses)
  change_to <- filter(changes_needed, excesses < 0)
  
  if(nrow(change_from) == 1 & nrow(change_to) == 1){
    knock_out <- latest_sample %>%
      inner_join(change_from, by = var_names) %>%
      sample_n(number_changes)
    
    replace_with <- knock_out
    replace_with[, var_names[1]] <- change_to[, var_names[1]]
    replace_with[, var_names[2]] <- change_to[, var_names[2]]
    
    latest_sample <- latest_sample %>%
      filter(!id %in% knock_out$id) %>%
      rbind(replace_with) 
    
  }
   latest_sample$id <- NULL

  # new  discrepency:
  new_disc <- evaluate_dissim(latest_sample, pops)
  
  # Note that if change_to has zero rows then
  stopifnot(new_disc <= starting_disc)
  return(list(latest_sample, 
              new_disc))
  
}


while(min(discrepencies) > 10){
  
  # cycle through each of our sets of marginal totals (in random order), 
  # looking to improve the match if we can
  for(i in sample(1:length(pops1))){
    y <-  improve_fix(latest_sample, 
                      marg_totals = pops1[[i]], 
                      full_data = full_data,
                      pops = pops1) 
    
    if(is.na(y[[2]])){stop("found an NA")}
    latest_sample <- y[[1]]
    discrepencies <- c(discrepencies, y[[2]])
    
    
    
  }
  
  # at the end of a cycle of variables, print how well we're going at improving things:
  print(min(discrepencies))
  
  # sometimes we get prematurely stuck with the "fixed" method and it is worth substituting
  # in and out some whole rows of data to kick-start the process:
  if(length(unique(tail(discrepencies, 20))) == 1){
    for(i in sample(1:length(pops1))){
      y <- latest_sample %>%
        improve_rnd(marg_totals = pops1[[i]], full_data = full_data, disc = 0.2,
                    pops = pops1, max_attempts = 5, verbose = FALSE) 
      
      latest_sample <- y[[1]]
      discrepencies <- c(discrepencies, y[[2]])
      
      
      
    }
  }
  
}


#-----------------End results--------------
latest_sample %>%
  group_by(Gender, q1) %>%
  summarise(latest_sample_total = n()) %>%
  left_join(pops1[[5]]) %>%
  rename(original_sample_total = Freq)

p6 <- tibble(discrepencies = discrepencies) %>%
  mutate(cycle = 1:n()) %>%
  ggplot(aes(x = cycle, y = discrepencies )) +
  geom_line() +
  labs(title = "Improvements in comparison of marginal totals from simulated sample and target totals")

frs::svg_png(p6, "../img/0159-discrepencies")
