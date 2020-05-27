# approach to turn emoji into images and then use richtext at
# https://www.hvitfeldt.me/blog/real-emojis-in-ggplot2/
# approach to find emoji in a string at 
# https://gist.github.com/hrbrmstr/e89eb173ae0333f50f94fe5086fedf8b
# https://stackoverflow.com/questions/43359066/how-can-i-match-emoji-with-an-r-regex
devtools::install_github("hadley/emo")
devtools::install_github("richfitz/remoji")

pets <- "I like 🐶 🐱 🐟 🐢"



library(emo)
library(rvest)
library(stringr)
library(remoji)
library(tidyverse)
library(ggtext)
library(rvest)

unicode <- read_html("https://unicode.org/emoji/charts/full-emoji-list.html")

ut <- unicode %>%
  html_node("table") %>%
  html_table()

all_emoji <- ut[,3]

em_dict <- read_delim("https://raw.githubusercontent.com/today-is-a-good-day/emojis/master/emDict.csv", delim = ";")$Native

emoji_to_link <- function(x) {
  paste0("https://emojipedia.org/emoji/",x) %>%
    xml2::read_html() %>%
    rvest::html_nodes("tr td a") %>%
    .[1] %>%
    rvest::html_attr("href") %>%
    paste0("https://emojipedia.org/", .) %>%
    xml2::read_html() %>%
    rvest::html_node('div[class="vendor-image"] img') %>%
    rvest::html_attr("src")
}

link_to_img <- function(x, size = 12) {
  paste0("<img src='", x, "' width='", size, "'/>")
}


token_to_rt <- function(x){
  if(x %in% all_emoji){
    y <- link_to_img(emoji_to_link(x))
  } else {
    y <- x
  }
  return(y)
}

string_to_rt <- function(x){
  tokens <- str_split(x, " ", simplify = FALSE)[[1]]
  y <- lapply(tokens,  token_to_rt)
  z <- do.call(paste, y)
  return(z)
}

pets2 <- string_to_rt(pets)

ggplot() +
  theme_void() +
  annotate("richtext", x = 1, y = 1, label = pets2)


