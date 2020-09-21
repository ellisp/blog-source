library(tidyverse)
library(haven)
library(gt)
library(tidytext)

# see tweet at https://twitter.com/sam_c_nz/status/1307808109000179713

nzes <- read_spss("NZES2017SPSS/NZES2017Release14-07-19.sav")
names(nzes)


apply(nzes, function(x)

  
  head(nzes$rmpissuex )

  
table(as_factor(nzes$rimpissue))

nzes %>%
  mutate(issue = as.character(as_factor(rimpissue)),
         issue = gsub("^[0-9]*\\. ", "", issue)) %>%
  group_by(issue) %>%
  summarise(est = sum(rwt)) %>%
  ungroup() %>%
  mutate(prop = round(est / sum(est) * 100, 1)) %>%
  select(issue, prop) %>%
  arrange(desc(prop)) %>%
  rename(`Percentage naming most important:` = prop,
         `Single most important issue in the New Zealand election (2017)` = issue) %>%
  gt()


full_resp <- nzes %>%
  filter(grepl("Environment", as_factor(rimpissue))) %>%
  select(rjacknum, rmpissuex, rwt) %>%
  mutate(rmpissuex = str_to_sentence(rmpissuex),
         rmpissuex = gsub("nz", "NZ", rmpissuex, ignore.case = TRUE))

full_resp %>%
  unnest_tokens("word", "rmpissuex") %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  summarise(sample_mentions = length(unique(rjacknum)),
            weighted_mentions = sum(rwt)) %>%
  mutate(prop_nz = weighted_mentions / sum(nzes$rwt),
         prop_env_issue_people = weighted_mentions / sum(full_resp$rwt)) %>%
  arrange(desc(weighted_mentions))
