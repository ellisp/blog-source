

# one-liner:
length(stringr::str_extract_all(paste(1:2000, collapse = ""), pattern = "1", simplify = TRUE))

# more considered approach
library(tidyverse)
library(scales)
library(testthat)
library(stringr)


count_digits <- function(data, page_number){
  length(stringr::str_extract_all(paste(data, collapse = ""), 
                                  pattern = as.character(page_number), simplify = TRUE))
}

expect_equal(sum(Vectorize(count_digits)(data = 1:2000, page_number = 1)), 1600)

#------------2500-------------
d <- expand.grid(n = 1:2500, digit = 0:9) 
d$freq <- apply(d, 1, function(x){count_digits(x[1], x[2])})

d <- d %>%
  group_by(digit) %>%
  mutate(cum_freq = cumsum(freq))


p <- d %>%
  ggplot(aes(x = n, y = cum_freq, colour = as.factor(digit))) +
  geom_line() +
  geom_text(data = filter(d, n == max(n)), aes(label = digit), hjust = 0) +
  theme(legend.position = "none") +
  scale_x_continuous("Pages in book", label = comma) +
  scale_y_continuous("Number of occurrences of digit in page number", label = comma) +
  scale_colour_brewer(palette = "Spectral") +
  ggtitle("Occurrence of digits in numbers in a simple sequence",
          "How many times does each digit appear in the page number of a book of different lengths")

CairoSVG("../img/0140-2500-results.svg", 8, 5)
print(p)
dev.off()

#------------75000-------------
d <- expand.grid(n = 1:75000, digit = 0:9) 
d$freq <- apply(d, 1, function(x){count_digits(x[1], x[2])})

d <- d %>%
  group_by(digit) %>%
  mutate(cum_freq = cumsum(freq))


p <- d %>%
  ggplot(aes(x = n, y = cum_freq, colour = as.factor(digit))) +
  geom_line() +
  geom_text(data = filter(d, n == max(n)), aes(label = digit), hjust = 0) +
  theme(legend.position = "none") +
  scale_x_continuous("Pages in book", label = comma) +
  scale_y_continuous("Number of occurrences of digit in page number", label = comma) +
  scale_colour_brewer(palette = "Spectral") +
  ggtitle("Occurrence of digits in numbers in a simple sequence",
          "How many times does each digit appear in the page number of a book of different lengths")

CairoSVG("../img/0140-75000-results.svg", 8, 5)
print(p)
dev.off()
