# This script is prep for the shiny app in the ./_working/0108/ folder
# See previous blog posts for sourcing the original data
# App is deployed to https://ellisp.shinyapps.io/nzes2014_x_by_party/ 

library(foreign)
library(tidyverse)
library(forcats)
library(DT)
library(survey)
library(rsconnect)

# Import the New Zealand Election Study data:
nzes_orig <- read.spss("NZES2014GeneralReleaseApril16.sav", 
                       to.data.frame = TRUE, trim.factor.names = TRUE, reencode = FALSE)

# full names for all the questions
varlab <- cbind(attributes(nzes_orig)$variable.labels)

# subset which variables we want
vars <- varlab[c(10:22, 94:99, 178:212, 261, 270:288, 308:324, 331, 341:343,
                 351:358, 362:381, 385:403), ] 
vars <- gsub("M.ori", "Māori", vars)

# View(data.frame(as.character(vars)))
vc <- as.character(vars)
vars_list <- list(
  "Political engagement" = vc[c(1:8, 14:19, 56:66)],
  "Views on spending" = vc[20:30],
  "Other attitudes" = vc[31:55],
  "Views on governance" = vc[c(67:84, 87, 88)],
  "About you" = vc[c(85, 86, 89:142, 9:13)]
)

table(nzes$dwkret)
x <- nzes$dwkret

# tidy up the data
nzes <- nzes_orig %>%
  # Fix M?ori in any levels of factors:
  map_df(function(x){
    if(is.factor(x)){
      levels(x) <-  gsub("M.ori", "Māori", levels(x))
      x
    } else {
      x
    }}) %>%
  
  # Fix non-useful encoding of binary variables when some are NA
  map_df(function(x){
    if(is.factor(x) & length(levels(x)) == 1){
      lx <- tolower(as.character(levels(x)))
      x <- as.character(x)
      x[is.na(x)] <- paste("Not", lx)
      return(x)
    } else {
      return(x)
    }
  }) %>%
  
  # Fix non-useful encoding of binary variables when some are 0
  map_df(function(x){
    if(is.factor(x) & 0 %in% levels(x)){
      lx <- as.character(levels(x)[2])
      x <- as.character(x)
      x[x == 0] <- paste("Not", lx)
      return(x)
    } else {
      return(x)
    }
  }) %>%
  
  # update gender classification to match the 2015 Stats NZ form of words at
  # http://www.stats.govt.nz/tools_and_services/media-centre/media-releases-2015/gender-identity-17-july-15.aspx
  mutate(dsex = ifelse(dsex == "Transsexual or transgender", "Gender-diverse", as.character(dsex)),
         dsex = fct_infreq(dsex)) %>%
  
  # lump up party vote:
  mutate(partyvote = ifelse(is.na(dpartyvote), "Did not vote", as.character(dpartyvote)),
         partyvote = gsub("net.Man", "net-Man", partyvote),
         partyvote = fct_lump(partyvote, 10, other_level = "Another party"),
         partyvote = fct_infreq(partyvote),
         # This magic constant, 3140417 was the size of the electoral roll at the time of election;
         # see http://www.electionresults.govt.nz/electionresults_2014/e9/html/e9_part9_1.html
         dwtfin = dwtfin * 3140.417 / sum(dwtfin) ) 
  nzes <- nzes[ ,c(names(vars), "partyvote", "dwtfin")]

party_numbers <- nzes %>%
  group_by(partyvote) %>%
  summarise(sample_size = n())

nzes <- nzes %>%
  left_join(party_numbers, by = "partyvote") %>%
  mutate(partyvote_n = paste0(partyvote, ", n = ", sample_size),
         partyvote_n = fct_infreq(partyvote_n))

#------------2014 actual outcomes----------------
# http://www.elections.org.nz/news-media/new-zealand-2014-general-election-official-results
actual_vote_2014 <- data_frame(
  partyvote2014 = c("National", "Labour", "Green", "NZ First", "Other", "Did not vote"),
  Freq = c(1131501, 604534, 257356, 208300, 
           31850 + 16689 + 5286 + 95598 + 34095 + 10961 + 5113 + 1730 + 1096 + 872 + 639,
           NA)
)

# calculate the did not vote, from the 77.9 percent turnout
actual_vote_2014[6, 2] <- (100 / 77.9 - 1) * sum(actual_vote_2014[1:5, 2])

nzes_vote_totals <- nzes %>%
  ungroup() %>%
  filter(partyvote != "Don't know") %>%
  mutate(partyvote2014 = fct_other(partyvote, keep = actual_vote_2014$partyvote2014)) %>%
  group_by(partyvote2014) %>%
  summarise(original_weight = sum(dwtfin)) %>%
  left_join(actual_vote_2014) %>%
  mutate(multiplier = Freq / original_weight / 1000) %>%
  select(partyvote2014, multiplier) %>%
  rbind(data_frame(partyvote2014 = "Don't know", multiplier = 0))

nzes <- nzes %>%
  mutate(partyvote2014 = fct_other(partyvote, keep = c(actual_vote_2014$partyvote2014, "Don't know"))) %>%
  left_join(nzes_vote_totals) %>%
  mutate(calibrated_weight = dwtfin * multiplier) %>%
  mutate(partyvote = fct_relevel(partyvote, "Did not vote"),
         partyvote_n = fct_relevel(partyvote_n, levels(partyvote_n)[grepl("Did not vote", levels(partyvote_n))]))


save(vars_list, file = "0108/vars_list.rda")
save(vars, file = "0108/vars.rda")
save(nzes, file = "0108/nzes.rda")


rsconnect::deployApp("0108", appName = "nzes2014_x_by_party", account = "ellisp")



#=======old==========
# tried having the web app report on a Chisquare test such as the below
# but it is always zero so uninteresting.
nzes_svy <- svydesign(~1, weights = ~dwtfin, data = nzes)
tmp <- svychisq(~ partyvote + dinterest, design = nzes_svy)
str(tmp)
round(tmp$p.value, 3)