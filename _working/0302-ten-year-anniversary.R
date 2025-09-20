
library(tidyverse)
library(stylo) # for delete.markup
library(glue)
library(ggtext)

#---------------Import and process blog posts-------------
blog_names <- list.files("../_posts", full.names = TRUE)
blogs <- tibble()

for(i in 1:length(blog_names)){
  blogs[i, "full"] <- paste(readLines(blog_names[i]), collapse = " ")
  blogs[i, "filename"] <- gsub("../_posts/", "", blog_names[i], fixed = TRUE)
}


blogs <- blogs |> 
  mutate(no_jekyll = gsub("\\{\\% highlight R.*?\\%\\}.*?\\{\\% endhighlight \\%\\}", " ", full),
         txt = "")

# delete markup only works on one string at a time, seems easiest to do it in a loop:
for(i in 1:nrow(blogs)){
  blogs[i, ]$txt <- delete.markup(blogs[i, ]$no_jekyll, markup.type = "html")
}

# a few more basic stats per blog post:
blogs <- blogs |> 
  mutate(word_count = stringi::stri_count_words(txt),
          word_count_with_tags = stringi::stri_count_words(no_jekyll),
          date = as.Date(str_extract(filename, "^[0-9]*-[0-9]*-[0-9]*")),
          month = month(date),
          year = year(date))

#---------------Minimal anaylsis----------------

# Summary aggregates
blog_sum <- blogs |> 
  summarise(number_blogs = n(), 
            words_with_tabs = sum(word_count_with_tags),
            total_words = sum(word_count),
            mean_words = mean(word_count),
            median_words = median(word_count),
            max_words = max(word_count),
            min_words = min(word_count))


# Shortest blog (turns out to be one just announcing a work shiny app):
blogs |> 
  arrange(word_count) |> 
  slice(1) |> 
  pull(txt)

#------------------Graphics for use in blog-------------------------

the_caption <- "Source: https://freerangestats.info"

# Time series plot showing number of posts by month:
d1 <- blogs |> 
  group_by(year, month) |> 
  summarise(number_blogs = n()) |> 
  ungroup() |> 
  complete(year, month, fill = list(number_blogs = 0)) |> 
  # remove October, November, December in 2025 (as time of writing is September 2025):
  filter(!(year == 2025 & month %in% 10:12)) |> 
  # remove months blog did not exist:
  filter(!(year == 2015 & month %in% 1:6)) |> 
  group_by(year) |> 
  mutate(year_lab = glue("{year}: {sum(number_blogs)} posts"),
         is_zero = ifelse(number_blogs == 0, "Zero", "NotZero")) 

# model a smooth curve to the whole data set (don't want)
# to do this with geom_smooth in the plot as then it has
# break every year:
mod <- loess(number_blogs ~ I(year + month / 12), data = d1, span = 0.15)
d1$fitted <- predict(mod)

# draw time series plot of number of blogs:
p1 <- d1 |> 
  ggplot(aes(x = month, y = number_blogs)) +
  facet_wrap(~year_lab) +
  geom_line(aes(y = fitted), colour = "grey80") +
  geom_point(colour = "steelblue", size = 2.5, aes(shape = is_zero)) +
  expand_limits(y = 0) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_shape_manual(values = c("Zero" = 1, "NotZero" = 19)) +
  theme(panel.grid.minor = element_blank(),
       axis.text.x = element_text(angle = 45, hjust = 1),
       legend.position = "none") +
  labs(x = "",
       y = "Number of blog posts",
       title = "Ten years of Free Range Statistics blogging",
       subtitle = glue("{nrow(blogs)} posts and {comma(blog_sum$total_words)} words, in just over ten years."),
      caption = the_caption)

svg_png(p1, "../img/0302-counts", w = 9, h = 6)

# Connected scatter plot comparing average word count to number of posts:
p2 <- blogs |> 
  mutate(number_months = case_when(
            year == 2015 ~ 6,
            year == 2025 ~8.5,
            TRUE ~ 12
          )) |> 
  group_by(year, number_months) |> 
  summarise(avg_word_count = mean(word_count, tr = 0.1),
            number_blogs = n()) |> 
  ungroup() |> 
  mutate(blogs_per_month = number_blogs / number_months) |> 
  ggplot(aes(x = blogs_per_month, y = avg_word_count, label = year)) +
  geom_path(colour = "grey80") +
  geom_text(colour = "grey50") +
  scale_y_continuous(label = comma) +
  expand_limits(x = 4.5) +
  annotate("text", fontface = "italic", hjust = 0, colour = "darkblue",
            x = c(4, 3.4, 2.1), 
            y = c(1165, 1350, 1880),
            label = c("Time series", "Elections", "Covid") 
            )  +
  # add day jobs
  annotate("text", fontface = "italic", hjust = 0, colour = "brown",
            x = c(3.1, 2.5, 0, 1.1), 
            y = c(1130, 1675, 1420, 1330),
            label = c("NZ economics", "Consultant", "Chief Data Scientist", "Pacific") 
            )  +
  labs(x = "Blog posts per month",
       y = "Average words per blog post",
       title = "Ten years of Free Range Statistics blogging",
       subtitle = "Annotated with important (but not necessarily dominant) <span style = 'color:darkblue'>themes</span> and <span style = 'color:brown'>day-jobs</span> for different phases.",
      caption = the_caption) +
  theme(plot.subtitle = element_markdown())
    
svg_png(p2, "../img/0302-word-counts", w = 8, h = 5)
