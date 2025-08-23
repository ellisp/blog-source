
library(tidyverse)
library(stylo)
blog_names <- list.files("../_posts", full.names = TRUE)
blogs <- tibble()

for(i in 1:length(blog_names)){
  blogs[i, "full"] <- paste(readLines(blog_names[i]), collapse = " ")
}


dmv <- Vectorize(delete.markup)

blogs <- blogs |> 
  mutate(no_jekyll = gsub("\\{\\% highlight R.*?\\%\\}.*?\\{\\% endhighlight \\%\\}", " ", full),
          txt = dmv(no_jekyll, markup.type = "html")) |> 
  mutate(word_count = stringi::stri_count_words(txt),
          word_count_with_tags = stringi::stri_count_words(no_jekyll))

blogs |> 
  summarise(number_blogs = n(), 
            words_with_tabs = sum(word_count_with_tags),
            total_words = sum(word_count),
            mean_words = mean(word_count),
            median_words = median(word_count),
            max_words = max(word_count),
            min_words = min(word_count))
blogs |> 
  arrange(desc(word_count)) |> 
  slice(2) |> 
  View()
  pull(txt)

