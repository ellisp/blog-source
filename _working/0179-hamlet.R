library(tidyverse)
library(tidytext)
library(gutenbergr)
library(SnowballC)
library(tidygraph)
library(ggraph)
library(Cairo)
CairoWin()

# TODO - a problem with lines 6380:6381 which has a stage direction across two lines.


#' Replace NA values in a vector with counting from the previous value
#' 
#' @param x a vector character
#' @examples 
#' replace_na_with_num(c(1,1,1,1, NA, NA, 1, 1, 1, NA))
replace_na_with_num <- function(x){
  # I can't think of a way to do this next operation without a loop because there is iteration
  for(i in 2:length(x)){
    if(is.na(x[i])){
      x[i] <- x[i-1] + 1
    }
  }
  return(x)
}
stopifnot(replace_na_with_num(c(1,1,1,NA, NA, 1, NA)) == c(1,1,1,2,3,1,2))


filter(gutenberg_authors, grepl("Shakespeare", author))

filter(gutenberg_metadata, grepl("Hamlet", title) & 
         author == "Shakespeare, William" &
         grepl("Public domain", rights) &
         language == "en") %>%
  select(gutenberg_id, title, gutenberg_bookshelf)

hamlet <- gutenberg_download(1524) %>%
  mutate(original_line_number = 1:n()) %>%
  select(-gutenberg_id)

hamlet %>%
  select(text) %>%
  slice(1:100) %>%
  pull(text)


# Stage directions are in square brackets eg:
# "[Enter Hamlet, Horatio, and Marcellus.]"
# "[Points to his head and shoulder.]" 
#
# Scenes begin like this:
# "Scene II. A hall in the Castle." 
#
# Speaker is in a short (3-5?) character abbreviation with no spaces followed by a dot eg
# [40] "Ham."                                              
# [41] "What hour now?"                                    
# [42] ""                                                  
# [43] "Hor."                                              
# [44] "I think it lacks of twelve." 
# [14] "King."                                             
# [15] "Do you think 'tis this?"                           
# [16] ""                                                  
# [17] "Queen."                                            
# [18] "It may be, very likely."

# Number of speeches by each character
hamlet %>%
  filter(grepl("^[A-Z][a-z]+\\.$", text)) %>%
  count(text, name= "number_speeches", sort = TRUE) 
# This causes a few problems, in particular:
# - Some one word sentences like "Farewell." and "Good."
# - Some collective terms like "Both." and "All."


# Number of times each stage direction used
hamlet %>%
  filter(grepl("^\\[.*\\]$", text)) %>%
  count(text, name = "times_used", sort = TRUE) 


# Scenes and acts. Sometimes upper case sometimes not
hamlet %>%
  filter(grepl("^Scene\\s", text, ignore.case = TRUE) | grepl("^Act\\s", text, ignore.case = TRUE))
# note this doesn't work fully because of a line that begins with "act hath three brnaches;"

#==================Processing========================
main_chars <- c("Hamlet", "Horatio", "Claudius", "Gertrude", "Ophelia", "Polonius", "Laertes", "Ghost")
# These aren't the top 8 characters - First gravedigger and Rosencrantz both have more words than Ghost -
# but seem the 8 most fully-rounded real people.

personae <- tribble(~speaker, ~speaker_abb, ~speaker_sh,
                     "Claudius, King of Denmark.", "King.", "Claudius",
                     "Hamlet, Son to the former, and Nephew to the present King.", "Ham.", "Hamlet",
                     "Polonius, Lord Chamberlain.", "Pol.", "Polonius",
                     "Horatio, Friend to Hamlet.", "Hor.",             "Horatio",                 
                     "Laertes, Son to Polonius.", "Laer.", "Laertes",
                     "Voltimand, Courtier.", "Volt.", "Voltimand",
                     "Cornelius, Courtier.", "???",               "Cornelius",                      
                     "Rosencrantz, Courtier.", "Ros.",                        "Rosencrantz",               
                     "Guildenstern, Courtier.", "Guil.",                                  "Guildenstern",
                     "Osric, Courtier.", "Osr.",                                "Osric",
                     "A Gentleman, Courtier.", "Gent.", "Gentleman",
                     "Marcellus, Officer.", "Mar.", "Marcellus",
                     "Bernardo, Officer.", "Ber.",              "Bernardo",                         
                     "Francisco, a Soldier", "Fran.",                      "Francisco",                
                     "Reynaldo, Servant to Polonius.", "Rey.","Reynaldo",
                     "Players", "Players.",                            "Players",
                     "Fortinbras, Prince of Norway.", "For.", "Fortinbras",
                     "Fortinbras, Prince of Norway.", "Fort.", "Fortinbras",
                     "A Captain.", "Capt.",                                 "Captain",              
                     "English Ambassador 1.", "1 Ambassador.", "English Ambassador",
                     "Ghost of Hamlet's Father.", "Ghost.", "Ghost",
                     "Gertrude, Queen of Denmark, and Mother of Hamlet.", "Queen.", "Gertrude",
                     "Ophelia, Daughter to Polonius.", "Oph.", "Ophelia",
                     "Prologue to The Murder of Gonzago, a play within a play", "Pro.", "Player prologue",
                      "King in The Murder of Gonzago, a play within a play", "P. King.", "Player King",
                    "Queen in The Murder of Gonzago, a play within a play", "P. Queen.", "Player Queen",
                      "Lucianus, nephew to the King in play within a play", "Luc.", "Lucianus",
                    "Danes", "Danes.", "Danes",
                    "Servant", "Servant.", "Servant",
                    "Sailor", "Sailor.", "Sailor",
                    "Messenger", "Mess.", "Messenger",
                    "First gravedigger clown", "1 Clown.", "First gravedigger",
                    "Second gravedigger clown", "2 Clown.", "Second gravedigger",
                    "First priest", "1 Priest.", "First Priest",
                    "Second priest", "2 Priest.", "Second Priest",
                    "Danish courtier lord", "Lord.", "Lord",
                    # There are 3 uses of 'All.', in each case mean different groups
                    "All present on stage", "All.", "All",
                    # only two uses of 'Both.', near the beginning:
                    "Marcellus and Bernardo together", "Both.", "Marcellus and Bernardo") %>%
  mutate(main_character = speaker_sh %in% main_chars)

not_people <- c("He.", "Say.", "Farewell.", "Perpend.", "Nothing.", "Good.", "Swear.", "No.", "Dead.", "One.")



# This chunk of code was how I identified the abbreviations that weren't in the original list of personae
# (eg "1 Clown."), and also refined the list above of not_people.
hamlet %>%
  slice(-(1:38)) %>%
  mutate(speaker_abb = case_when(
    # Most people picked up by this
    grepl("^[A-Z][a-z]+\\.$", text)         ~ text,
    # "1 Ambassador", "2 Clown" and similar are picked up by this:
    grepl("^[1-9]\\s[A-Z][a-z]+\\.$", text) ~ text,
    # P. King. etc picked up by this:
    grepl("^[A-Z]\\.\\s[A-Z][a-z]+\\.$", text) ~ text
    )) %>%
  filter(text != "") %>%
  mutate(speaker_abb = if_else(speaker_abb %in% not_people, NA_character_, speaker_abb)) %>%
  fill(speaker_abb) %>%
  filter(!is.na(speaker_abb)) %>%
  anti_join(personae, by = "speaker_abb")


hamlet_lines <- hamlet %>%
  slice(-(1:39)) %>%
  # Identify the speaker:
  mutate(speaker_abb = case_when(
    # Most people picked up by this
    grepl("^[A-Z][a-z]+\\.$", text)         ~ text,
    # "1 Ambassador", "2 Clown" and similar are picked up by this:
    grepl("^[1-9]\\s[A-Z][a-z]+\\.$", text) ~ text,
    # P. King. etc picked up by this:
    grepl("^[A-Z]\\.\\s[A-Z][a-z]+\\.$", text) ~ text
  )) %>%
  filter(text != "") %>%
  mutate(speaker_abb = if_else(speaker_abb %in% not_people, NA_character_, speaker_abb)) %>%
  fill(speaker_abb) %>%
  filter(is.na(speaker_abb) | text != speaker_abb) %>%
  left_join(personae, by = "speaker_abb") %>%
  # Identify stage directions:
  mutate(last_stage_direction = case_when(
         grepl("^\\[.*\\]$", text) ~ text
  )) %>%
  fill(last_stage_direction) %>%
  filter(is.na(last_stage_direction) | text != last_stage_direction) %>%
  mutate(last_stage_direction = gsub("[", "", last_stage_direction, fixed = TRUE),
         last_stage_direction = gsub("]", "", last_stage_direction, fixed = TRUE)) %>%
  # Identify Act:
  mutate(act = case_when(
    grepl("^A[Cc][Tt]\\s", text) ~ text
  )) %>%
  fill(act) %>%
  filter(is.na(act) | text != act) %>%
  # Identify Scene
  mutate(scene = case_when(
    grepl("^S[Cc][Ee][Nn][Ee]\\s", text) ~ text
  )) %>%
  fill(scene) %>%
  filter(is.na(scene) | text != scene) %>%
  # regularise some spelling:
  mutate(scene = gsub("Castle", "castle", scene)) %>%
  # identify the actual speeches and count the lines in each speech by a continuing speaker
  mutate(new_speaker_this_line = is.na(lag(speaker)) | speaker != lag(speaker),
         line_number_this_speech = ifelse(new_speaker_this_line, 1, NA),
         line_number_this_speech = replace_na_with_num(line_number_this_speech))

# Number of lines per scene
hamlet_lines %>%
  count(act, scene)

#filter(hamlet_lines, speaker_sh == "Lord")
#filter(hamlet_lines, original_line_number %in% 6300:6400) %>% View

# concept of stopwords seems a bit arbitrary. Why aren't "shall", "go", "us" and "one" 
# already stopwords in the snowball lexicon whereas "i", "our" are?
our_stopwords <- tibble(word = c(
  "thou", "thine", "o", "tis", "thee", "thy", "sir", "hath", "lord", "us", "one"
)) %>%
  mutate(lexicon = "made up") %>%
  rbind(get_stopwords())
  
  

hamlet_words <- hamlet_lines %>%
  tidytext::unnest_tokens(output ="word", input = "text") %>%
  left_join(our_stopwords, by = "word") %>%
  mutate(stopword = !is.na(lexicon)) %>%
  select(-lexicon) %>%
  mutate(word_stem = wordStem(word)) %>%
  # identify the actual speeches and count the words in each speech by a continuing speaker
  mutate(new_speaker_this_word = is.na(lag(speaker)) | speaker != lag(speaker),
         word_number_this_speech = ifelse(new_speaker_this_word, 1, NA),
         word_number_this_speech = replace_na_with_num(word_number_this_speech)) %>%
  mutate(word_number = 1:n()) %>%
  group_by(act) %>%
  mutate(word_number_this_act = 1:n()) %>%
  group_by(act, scene) %>%
  mutate(word_number_this_scene = 1:n()) %>%
  ungroup()

words_starting_speeches <- hamlet_words %>%
  filter(new_speaker_this_word) %>%
  select(word_number) %>%
  mutate(speech_number = 1:n())

hamlet_words <- hamlet_words %>%
  left_join(words_starting_speeches, by = "word_number") %>%
  fill(speech_number) 

# words per speaker:
char_summary <- hamlet_words %>%
  group_by(speaker_sh) %>%
  summarise(words = n(),
            speeches = sum(new_speaker_this_line),
            words_per_speech = words / speeches,
            stopwords = sum(stopword),
            prop_non_stop = 1 - stopwords / words,
            most_words_single_speech = max(word_number_this_speech)) %>%
  arrange(desc(words))

top_20_chars <- char_summary[1:20, ]$speaker_sh

hamlet_words <- hamlet_words %>%
  mutate(top_20_char = speaker_sh %in% top_20_chars,
         speaker_sh = factor(speaker_sh, levels = char_summary$speaker_sh))

# words per scene:
hamlet_words %>%
  count(act, scene) 

# most common words:
hamlet_words %>%
  count(word, sort = TRUE)

# most common words other than stopwords
hamlet_words %>%
  filter(!stopword) %>%
  count(word, sort = TRUE) %>%
  print(n = 30)

#--------------------Distinctive words-------------------

# three most distinctive word satem for each of the main cast
dist_words <- hamlet_words %>%
  filter(!stopword) %>%
  count(word_stem, speaker_sh) %>%
  group_by(speaker_sh) %>%
  mutate(prop_this_speaker = n / sum(n)) %>%
  ungroup() %>%
  mutate(prop_overall = n / sum(n),
         relative = prop_this_speaker / prop_overall) %>%
  filter(n > 2) %>%
  group_by(speaker_sh) %>%
  arrange(desc(relative), desc(n)) %>%
  slice(1:5) %>%
  filter(speaker_sh %in% top_20_chars) 
# note that "mai" is the word stem for "may", and "joi" for "joy"

dist_words %>%
  group_by(speaker_sh) %>%
  mutate(rank = 1:n()) %>%
  ggplot(aes(x = rank, y = reorder(speaker_sh, desc(speaker_sh)), label = word_stem)) +
  geom_text() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(x = "", y = "",
       title = "Most distinctive words for the top 20 characters in Hamlet")

#--------------------Distribution of length of speeches--------------------
hamlet_words %>%
  filter(main_character | speaker_sh == "First gravedigger") %>%
  group_by(speech_number) %>%
  filter(word_number_this_speech == max(word_number_this_speech)) %>%
  select(speaker_sh, speech_number, word_number_this_speech) %>%
  ggplot(aes(x = word_number_this_speech)) +
  facet_wrap(~speaker_sh,scales = "free_y") +
  geom_density(fill = "steelblue", col = NA, alpha = 0.5) +
  geom_rug() +
  scale_x_sqrt() +
  labs(x = "Number of continuous words in row",
       title = "Length of uninterrupted speeches in Shakespeare's Hamlet") +
  theme(panel.spacing = unit(2, "lines"))


#=========who interacts with whom==============

hamlet_handovers <- hamlet_words %>%
  filter(new_speaker_this_word) %>%
  mutate(previous_speaker = lag(speaker_sh)) %>%
  select(new_speaker = speaker_sh, previous_speaker, act, scene, word_number)  %>%
  mutate(words_this_speech = lead(word_number, default = max(hamlet_words$word_number)) - word_number)

d <- hamlet_handovers %>%
  filter(!is.na(previous_speaker)) %>%
  count(new_speaker, previous_speaker, sort = TRUE)  %>%
  select(from = previous_speaker, to = new_speaker, n) %>%
  mutate(from = as.character(from),
         to = as.character(to))

d1 <- d %>%
  rename(new_from = to,
         to = from) %>%
  rename(from = new_from) %>%
  rbind(d) %>%
  group_by(from, to) %>%
  summarise(n = sum(n)) %>%
  filter(from < to)
# to do = tidy up Marcellus and Bernardo here

d2 <-as_tbl_graph(d1) %>%
  activate(nodes) %>%
  left_join(char_summary, by = c("name" = "speaker_sh"))

layout <- create_layout(d2, layout = 'igraph', algorithm = 'kk') 

ggraph(layout) +
  geom_edge_arc(strength = 0, 
                aes(edge_colour = n, edge_width = n)) +
  geom_node_label(aes(label = name), colour = "white", fill = "grey10", label.size = 0) +
  geom_node_point(aes(size = words), shape = 19, colour = "steelblue", alpha = 0.5) +
  scale_edge_colour_viridis() +
  theme(panel.background = element_rect(fill = "black")) +
  scale_size_area(max_size = 22, label = comma) +
  labs(edge_colour = "Number of transitions:",
       size = "Total words spoken:")

# can't control the label for edge_size


#======================Topic modelling================
# we could do this for each individual speech; or for all speeches 
