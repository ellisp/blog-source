library(tidyverse)
library(haven)
library(gt)
library(tidytext)
library(SnowballC)
library(BTM)
library(textplot)
library(ggraph)
library(concaveman)
library(scales)
library(glue)
library(nzelect)
library(clipr)
library(kableExtra)

# see tweet at https://twitter.com/sam_c_nz/status/1307808109000179713

nzes <- read_spss("NZES2017SPSS/NZES2017Release14-07-19.sav")
the_caption <- "Data from the New Zealand Election Study, analysed at http://freerangestats.info"

# names(nzes)[grepl("issue", names(nzes))]
# attributes(nzes$rmpissuex)$label
# attributes(nzes$rimpissue)$label
# table(as_factor(nzes$rimpissue))
# # You might think the answer to "C3x: single most important issue in the election"
# # would be conceptually MECE, but the answers often (200+) include more than one
# set.seed(123)
# nzes %>%
#   filter(!is.na(rimpissue2)) %>%
#   select(rmpissuex, rimpissue, rimpissue2) %>%
#   sample_n(10)
# Consider the person who says this is their main issue: "No party had policy
# that would remedy major problems, police, education, health, councils". If you
# see the 'issue' as 'no party had the right policies' this is fair enough, even
# though it's not how the researchers were thinking of 'issue'. Or the many
# people who said things like "Housing and poverty"
#
# Basically, topic modelling that allows multiple topics, but prior is only one topic, seems appropriate


# Table of most important topics:
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
         `Single most important issue in the New Zealand election (2017)` = issue) %>% gt
  kable() %>%
  kable_styling() %>%
  write_clip()

# Full responses and human-chosen topics (issues), dropping uneeded variables
full_resp <- nzes %>%
  mutate(pv = fct_lump(as_factor(rpartyvote), 6)) %>%
  mutate(pv = gsub("^[0-9]\\. ", "", pv)) %>%
  mutate(issue = as.character(as_factor(rimpissue)),
         issue = gsub("^[0-9]*\\. ", "", issue)) %>%
  mutate(rmpissuex = str_to_sentence(rmpissuex),
         rmpissuex = gsub("nz", "NZ", rmpissuex, ignore.case = TRUE)) %>%
  select(rjacknum, issue, rmpissuex, rwt, pv) %>%
  filter(issue != "None Named")
  
# Word stems
stems <- full_resp %>%
  unnest_tokens("word", "rmpissuex") %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word_stem = wordStem(word)) 

grouped_stems <- stems %>%
  group_by(issue, word_stem) %>%
  summarise(sample_mentions = length(unique(rjacknum)),
            weighted_mentions = sum(rwt)) %>%
  mutate(prop_nz = weighted_mentions / sum(nzes$rwt))  %>%
  arrange(desc(weighted_mentions)) %>%
  ungroup()

#-------------------Biterm Topic Modelling---------------
k <- length(unique(stems$issue))
stems_for_btm <- select(stems, rjacknum, word_stem)

set.seed(123)
btm_mod <- BTM(data = stems_for_btm, 
               k    = k,
               alpha = 0.8)



topics_btm <- predict(btm_mod, newdata = stems_for_btm) %>%
  as.data.frame() %>%
  rownames_to_column(var = "rjacknum") %>%
  mutate(rjacknum = as.numeric(rjacknum)) %>%
  as_tibble()

names(topics_btm)[-1] <- glue("topic_{1:k}")

topics_btm <- topics_btm %>%
  mutate(top_topic_btm = apply(topics_btm[, -1], 1,
                               function(x){which(x == max(x))}),
         top_topic_btm = fct_reorder(as.character(top_topic_btm), 
                                     top_topic_btm)) %>%
  full_join(full_resp, by = "rjacknum")

#------------Various visualisations---------------
# Words used by human-chosen topic
p1 <- grouped_stems %>%
  mutate(issue = fct_lump(issue, 11, w = prop_nz)) %>%
  group_by(issue, word_stem) %>%
  summarise(prop_nz = sum(prop_nz)) %>%
  ungroup() %>%
  mutate(issue = fct_reorder(issue, -prop_nz, .fun = mean)) %>%
  group_by(issue) %>%
  arrange(desc(prop_nz)) %>%
  slice(1:10) %>%
  mutate(word_stem = tidytext::reorder_within(word_stem, prop_nz, issue)) %>%
  ggplot(aes(x = prop_nz, y = word_stem)) +
  geom_segment(aes(yend = word_stem, xend = 0)) +
  geom_point() +
  facet_wrap(~issue, scales = "free") +
  scale_y_reordered() +
  scale_x_continuous(label = percent_format(accuracy = 0.1)) +
  labs(caption = the_caption,
       x = "Proportion of voters using this word stem",
       y= "",
       subtitle = "Responses to question on the 'single most important issue in the election', grouped into topics by human coders. Most mentioned topics at top left of chart.",
       title = "Words used to describe important issues in New Zealand, 2017")

svg_png(p1, "../img/0193-stems-issues", w = 11, h = 8)



#  Human-coded topic by party
p5 <- topics_btm %>%
  group_by(issue, pv) %>%
  summarise(rwt = sum(rwt)) %>%
  group_by(pv) %>%
  mutate(prop = rwt / sum(rwt)) %>%
  drop_na() %>%
  ungroup() %>%
  mutate(pv = fct_reorder(pv, -rwt),
         issue = fct_reorder(issue, rwt)) %>%
  ggplot(aes(y = issue, x = pv, alpha = prop, fill = pv)) +
  scale_fill_manual(values = c(parties_v, Maori  = "#EF4A42", Other = "grey")) +
  geom_tile() +
  scale_alpha_continuous(guide = "none", range = c(0, 1)) +
  labs(caption = the_caption,
       x = "",
       fill = "Party Vote",
       y = "",
       subtitle = "Responses to question on the 'single most important issue in the election', grouped into topics by human coders. Most mentioned topics at top left of chart.",
       title = "Most important issues for voters in New Zealand, 2017")

svg_png(p5, "../img/0193-pv-issues", w = 9, h = 7)

# For each combination of 'issue' (human-chosen) and topic, what is the party
# for which this is most distinctive to that party? Used for colouring.
associated_parties <- topics_btm %>%
  filter(!is.na(pv) & pv != "Other") %>%
  count(top_topic_btm, pv, issue) %>%
  group_by(pv) %>%
  mutate(n = n / sum(n)) %>%
  group_by(top_topic_btm, issue) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  select(top_topic_btm, issue, pv)

d <- topics_btm %>%
  count(top_topic_btm, issue) %>%
  group_by(top_topic_btm) %>%
  mutate(prop_this_btm_topic = n / sum(n, na.rm = TRUE)) %>%
  group_by(issue) %>%
  mutate(prop_this_human_topic = n / sum(n, na.rm = TRUE)) %>%
  ungroup() %>%
  drop_na() %>%
  left_join(associated_parties, by = c("top_topic_btm", "issue")) %>%
  mutate(pv = replace_na(pv, "Other"))

p3 <- d %>%
  filter(prop_this_btm_topic > 0.05) %>%
  ggplot(aes(x = top_topic_btm, y = issue, 
             fill = pv, alpha = prop_this_btm_topic)) +
  scale_fill_manual(values = c(parties_v, Maori  = "#EF4A42", Other = "grey")) +
  geom_tile() +
  scale_alpha_continuous(guide = "none", range = c(0, 1)) +
  labs(x = "Topic identified by biterm topic modelling",
       y = "Human-coded topic",
       title = "Comparison of human- and machine-coded topics in free text questions",
       subtitle = "Responses to question on the 'single most important issue in the election'",
       fill = "Party vote most associated with this computer-chosen topic:")

p4 <- d %>%
  filter(prop_this_human_topic > 0.05) %>%
  ggplot(aes(x = top_topic_btm, y = issue, 
             fill = pv, alpha = prop_this_human_topic)) +
  scale_fill_manual(values = c(parties_v, Maori  = "#EF4A42", Other = "grey")) +
  geom_tile() +
  scale_alpha_continuous(guide = "none", range = c(0, 1)) +
  labs(x = "Topic identified by biterm topic modelling",
       y = "Human-coded topic",
       title = "Comparison of human- and machine-coded topics in free text questions",
       subtitle = "Responses to question on the 'single most important issue in the election'",
       fill = "Party vote most associated with this human-chosen topic:")

set.seed(123)
p2 <- plot(btm_mod, top_n = 7, labels = 1:k,
           title = "21 computer-chosen topics in response to free text questions",
           subtitle = "Responses to question on the 'single most important issue in the election'") +
  theme(text = element_text(family = "Roboto")) +
  scale_fill_manual(values = colours()[101:121])



svg_png(p2, "../img/0193-btm", w = 14, h = 10)
svg_png(p3, "../img/0193-heatmap-btm", w = 9)
svg_png(p4, "../img/0193-heatmap-human", w = 9)

# BTM topic 15 is about climate change, but also picks up a bunch of things about "chang in govt" and "change"!
topics_btm %>%
  filter(top_topic_btm == 15) %>%
  count(rmpissuex, sort = TRUE)

# Some global warming goes into BTM topic 1 along with the clean green environment
topics_btm %>%
  filter(top_topic_btm == 1) %>%
  count(rmpissuex, sort = TRUE)


# 9 is about 'rich and poor'
topics_btm %>%
  filter(top_topic_btm == 9) %>%
  count(rmpissuex, sort = TRUE)

# 17 is about 'environmental' things separate to the 'clean' and 'green'
topics_btm %>%
  filter(top_topic_btm == 17) %>%
  count(rmpissuex, sort = TRUE)

# 19 is mostly about 'tax', but via 'water tax' we get some 'water', and then some other things associated with that
topics_btm %>%
  filter(top_topic_btm == 19) %>%
  count(rmpissuex, sort = TRUE)

# Generally, the human coding is better... Good example being the mix-up between "climate change" and "change in govt".

