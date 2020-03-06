# This script is prep for the shiny app in the ./_working/0108/ folder
# See previous blog posts for sourcing the original data
# App is deployed to https://ellisp.shinyapps.io/nzes2014_x_by_party/ 

library(foreign)
library(tidyverse)
library(forcats)
library(DT)
library(survey)
library(rsconnect)
library(frs)

#----------------------2014----------------------
# Import the New Zealand Election Study data:
nzes14_orig <- read.spss("NZES2014GeneralReleaseApril16.sav", 
                       to.data.frame = TRUE, trim.factor.names = TRUE, reencode = FALSE)

# full names for all the questions
varlab14 <- cbind(attributes(nzes14_orig)$variable.labels)

# subset which variables we want
vars14 <- varlab14[c(10:22, 94:99, 178:212, 261, 270:288, 308:324, 331, 341:343,
                 351:358, 362:381, 385:403), ] 
vars14 <- gsub("M.ori", "Māori", vars14)

# View(data.frame(as.character(vars14)))
vc14 <- as.character(vars14)
vars14_list <- list(
  "Political engagement" = vc14[c(1:8, 14:19, 56:66)],
  "Views on spending" = vc14[20:30],
  "Other attitudes" = vc14[31:55],
  "Views on governance" = vc14[c(67:84, 87, 88)],
  "About you" = vc14[c(85, 86, 89:142, 9:13)]
)

# tidy up the data
nzes14 <- nzes14_orig %>%
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

nzes14 <- nzes14[ ,c(names(vars14), "partyvote", "dwtfin")]

party_numbers <- nzes14 %>%
  group_by(partyvote) %>%
  summarise(sample_size = n())

nzes14 <- nzes14 %>%
  left_join(party_numbers, by = "partyvote") %>%
  mutate(partyvote_n = paste0(partyvote, ", n = ", sample_size),
         partyvote_n = fct_infreq(partyvote_n)) 
  

#------------2014 actual outcomes----------------
# http://www.elections.org.nz/news-media/new-zealand-2014-general-election-official-results
actual_vote_2014 <- tibble(
  partyvote2014 = c("National", "Labour", "Green", "NZ First", "Other", "Did not vote"),
  Freq = c(1131501, 604534, 257356, 208300, 
           31850 + 16689 + 5286 + 95598 + 34095 + 10961 + 5113 + 1730 + 1096 + 872 + 639,
           NA)
)

# calculate the did not vote, from the 77.9 percent turnout
actual_vote_2014[6, 2] <- (100 / 77.9 - 1) * sum(actual_vote_2014[1:5, 2])

nzes14_vote_totals <- nzes14 %>%
  ungroup() %>%
  filter(partyvote != "Don't know") %>%
  mutate(partyvote2014 = fct_other(partyvote, keep = actual_vote_2014$partyvote2014)) %>%
  group_by(partyvote2014) %>%
  summarise(original_weight = sum(dwtfin)) %>%
  mutate(partyvote2014 = as.character(partyvote2014)) %>%
  left_join(actual_vote_2014, by = "partyvote2014") %>%
  mutate(multiplier = Freq / original_weight / 1000) %>%
  select(partyvote2014, multiplier) %>%
  rbind(tibble(partyvote2014 = "Don't know", multiplier = 0))

nzes14 <- nzes14 %>%
  mutate(partyvote2014 = fct_other(partyvote, keep = c(actual_vote_2014$partyvote2014, "Don't know"))) %>%
  mutate(partyvote2014 = as.character(partyvote2014)) %>%
  left_join(nzes14_vote_totals, by = "partyvote2014") %>%
  mutate(calibrated_weight = dwtfin * multiplier) %>%
  mutate(partyvote = fct_relevel(partyvote, "Did not vote"),
         partyvote_n = fct_relevel(partyvote_n, levels(partyvote_n)[grepl("Did not vote", levels(partyvote_n))]))

#========================2017===============================
# The R version doesn't load, you get
# > load("NZES2017R/NZES2017Release14-07-19.rdata")
# Error in load("NZES2017R/NZES2017Release14-07-19.rdata") : 
#   Value of SET_STRING_ELT() must be a 'CHARSXP' not a 'integer'

# unzip("NZES2017SPSS.zip")

nzes17_orig <- read.spss("NZES2017SPSS/NZES2017Release14-07-19.sav", 
                    to.data.frame = TRUE, trim.factor.names = TRUE, reencode = FALSE)

# full names for all the questions
varlab17 <- attributes(nzes17_orig)$variable.labels


# check that we have all the variables we used in 2014 (we don't)
# stopifnot(sum(!names(vars) %in% names(varlab)) == 0)
# stopifnot(sum(!vc %in% as.character(varlab)) == 0)

# So we will have to identify these by hand

# subset which variables we want
vars17 <- varlab17[c(18:42, 91:96, 98:155, 192:245,297:341, 342, 344, 356, 
                     357:360, 361, 365:377, 379:398, 406:440,466:485)] 
vars17 <- gsub("M.ori", "Māori", vars17)

vc17 <- as.character(vars17)
vars17_list <- list(
  "Political engagement" = vc17[c(284, 1:2, 8:31, 150:161)],
  "Views on spending" = vc17[c(98:108)],
  "Other economic views" = vc17[c(54:71, 109:112, 136:143)],
  "Other attitudes" = vc17[c(92:97, 113:134, 164:172)],
  "Views on governance" = vc17[c(32:53, 72:91, 135, 144:149, 162:163, 173:188)],
  "About you" = vc17[c(189:264, 276:283, 3:7)]
)



# tidy up the data
nzes17 <- nzes17_orig %>%
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
  mutate(rsex = ifelse(rsex == "Transsexual or transgender", "Gender-diverse", as.character(rsex)),
         rsex = fct_infreq(rsex)) %>%
  
  # lump up party vote:
  mutate(partyvote = ifelse(is.na(rpartyvote), "Did not vote", as.character(rpartyvote)),
         partyvote = gsub("net.Man", "net-Man", partyvote),
         partyvote = fct_lump(partyvote, 10, other_level = "Another party"),
         partyvote = fct_infreq(partyvote),
         # This magic constant, 4244355 was the size of the electoral roll at the time of election;
         # see https://www.electionresults.govt.nz/electionresults_2017/statistics/index.html
         dwtfin = rwt * 4244.355 / sum(rwt) ) 

nzes17 <- nzes17[ ,c(names(vars17), "partyvote", "dwtfin")]

party_numbers17 <- nzes17 %>%
  group_by(partyvote) %>%
  summarise(sample_size = n())

nzes17 <- nzes17 %>%
  left_join(party_numbers17, by = "partyvote") %>%
  mutate(partyvote_n = paste0(partyvote, ", n = ", sample_size),
         partyvote_n = fct_infreq(partyvote_n)) 


#------------2017 actual outcomes----------------
# http://www.elections.org.nz/news-media/new-zealand-2017-general-election-official-results
actual_vote_2017 <- tibble(
  partyvote2017 = c("National", "Labour", "Green", "NZ First", "Other", "Did not vote"),
  Freq = c(1152075, 956184, 162443, 186706, 
           13075 + 63261 + 30580 + 8075 + 6253 + 3642 + 3005 + 1890 + 1782 + 1620 + 806 + 499,
           NA)
)

# check adds up correct and I didn't have a typo"
stopifnot(sum(actual_vote_2017$Freq, na.rm = TRUE) == 2591896)

# calculate the did not vote, from the 79.75 percent turnout
actual_vote_2017[6, 2] <- (100 / 79.75 - 1) * sum(actual_vote_2017[1:5, 2])

nzes17_vote_totals <- nzes17 %>%
  ungroup() %>%
  filter(partyvote != "Don't know") %>%
  mutate(partyvote2017 = as.character(fct_other(partyvote, keep = actual_vote_2017$partyvote2017))) %>%
  group_by(partyvote2017) %>%
  summarise(original_weight = sum(dwtfin)) %>%
  left_join(actual_vote_2017, by = "partyvote2017") %>%
  mutate(multiplier = Freq / original_weight / 1000) %>%
  select(partyvote2017, multiplier) %>%
  rbind(data_frame(partyvote2017 = "Don't know", multiplier = 0))

nzes17 <- nzes17 %>%
  mutate(partyvote2017 = fct_other(partyvote, keep = c(actual_vote_2017$partyvote2017, "Don't know"))) %>%
  mutate(partyvote2017 = as.character(partyvote2017)) %>%
  left_join(nzes17_vote_totals, by = "partyvote2017") %>%
  mutate(calibrated_weight = dwtfin * multiplier) %>%
  mutate(partyvote = fct_relevel(partyvote, "Did not vote"),
         partyvote_n = fct_relevel(partyvote_n, levels(partyvote_n)[grepl("Did not vote", levels(partyvote_n))]))


#==================Combine and save==================

save(vars14_list, file = "0108a/data/vars14_list.rda")
save(vars14, file = "0108a/data/vars14.rda")
save(nzes14, file = "0108a/data/nzes14.rda")

save(vars17_list, file = "0108a/data/vars17_list.rda")
save(vars17, file = "0108a/data/vars17.rda")
save(nzes17, file = "0108a/data/nzes17.rda")

# rsconnect::deployApp("0108a", appName = "nzes_by_party", account = "ellisp")



