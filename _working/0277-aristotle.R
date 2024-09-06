library(gutenbergr)
library(tidyverse)

filter(gutenberg_authors, grepl("Aristotle", author))
# 2747 and 55497 (the latter is pseudo aristotle)

ari_works <- gutenberg_metadata |>
  filter(author == "Aristotle" & language == "en") 



# ari_works 
# - lots missing here eg physics

# according to goodreads the quote is in The Philosophy of Aristotle
# https://www.goodreads.com/quotes/709859-give-me-a-child-until-he-is-7-and-i
# translated by J.L. Creed and A.E Wardman
# not availoable on kindle. But I did buy the "complete work for 67c

fulltext <- lapply(ari_works$gutenberg_id, gutenberg_download)
length(fulltext)

filter(fulltext[[1]], grepl("child", text))

lapply(fulltext, function(d){filter(d, grepl("a child", text, ignore.case = TRUE))}) |>
  bind_rows() |>
  pull(text)


lapply(fulltext, function(d){filter(d, grepl("seven[ \\.]", text, ignore.case = TRUE))}) |>
  bind_rows() |>
  pull(text)

lapply(fulltext, function(d){filter(d, grepl("show", text, ignore.case = TRUE))}) |>
  bind_rows() |>
  pull(text)

       

       