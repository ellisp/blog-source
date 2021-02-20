library(tidyverse)
# library(rvest)
# 
# #This site has the books listed in chronological order of the story:
# url <- "https://www.biggles.nl/en/chronological.html"
# 
# 
# biggles_page <- read_html(url)
# 
# biggles_page %>%
#   html_nodes("table") %>%
#   .[[1]] %>%
#   html_table(fill = TRUE) %>%
#   select (number_and_title = X1, 
#           publication_year = X2)  %>%
#   as_tibble() %>%
#   mutate(story_sequence = as.numeric(str_extract(number_and_title, "^[0-9]*")),
#          title = gsub("^[0-9]+\\. ", "", number_and_title)) %>%
#   select(publication_year, story_sequence, title) %>%
#   write_csv("biggles-books.csv")



biggles <- read_csv("biggles-books.csv", 
                    col_types = cols(
                      publication_year = col_double(),
                      story_sequence = col_double(),
                      title = col_character(),
                      story_year = col_double(),
                      notes = col_character(),
                      story_type = col_character(),
                      detailed_story_type = col_character(),
                      book_type = col_character(),
                      main_location = col_character(),
                      main_location_broad = col_character()
                    ))

biggles %>%
  ggplot(aes(y = publication_year, x = story_year)) +
  geom_jitter()

biggles %>%
  count(publication_year) %>%
  ggplot(aes(x = publication_year, y = n)) +
  geom_col() +
  theme(panel.grid.minor = element_blank())

biggles %>%
  count(story_type, book_type) %>%
  ggplot(aes(y = book_type, fill = story_type, x = n)) +
  geom_col() +
  theme(panel.grid.minor = element_blank())

biggles %>%
  ggplot(aes(x = main_location_broad, fill = story_type)) +
  geom_bar() +
  theme(panel.grid.minor = element_blank()) +
  coord_flip() +
  theme(legend.position = "right")
