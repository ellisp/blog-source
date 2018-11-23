

# one-liner:
length(stringr::str_extract_all(paste(1:2000, collapse = ""), pattern = "1", simplify = TRUE))

# more considered approach
library(tidyverse)
library(scales)
library(testthat)
library(stringr)
library(gridExtra)

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

CairoPNG("../img/0140-75000-results.png", 8 * 600, 5 * 600, res = 600)
print(p)
dev.off()

#--------Benford's 'law'-----------
#' Function to draw a chart of frequency of first digits 
#' 
#' @param x a univariate numeric vector
#' @param desc description of x to use as subtitle 
#' Other parameters are self-explanatory
count_first_digit <- function(x, desc, main_title = "Frequency of first digits", 
                              seed = 123, legend.position = "none", 
                              bottom_lab = "", left_lab = "Leading digit"){
  set.seed(seed)
  # Extract the first digit from 1 to 9
  y <- as.character(x)
  y <- str_extract(y, "[1-9]")
  
  refs <- data_frame(
    digit = 1:9,
    `Benford's law` = log10(1  + (1/1:9))
  )
  
  z <- table(y)
  p <- data_frame(digit = as.numeric(names(z)),
             freq = as.numeric(z)) %>%
    mutate(freq = freq / sum(freq)) %>%
    rename(`Observed` = freq) %>%
    right_join(refs, by = "digit") %>%
    gather(variable, value, -digit)  %>%
    ggplot(aes(x = as.ordered(digit), y = value, fill = variable)) +
    geom_col(position = "dodge") +
    coord_flip() +
    scale_fill_manual("", values = c("grey85", "darkblue")) +
    ggtitle(main_title,
            desc) +
    labs(x = left_lab, y = bottom_lab) +
    theme(legend.position = legend.position)

  return(p)    

}

n <- 1000000
p1 <- count_first_digit(rnorm(n), "Standard normal distribution", legend.position = c(0.8, 0.8))
p2 <- count_first_digit(exp(rnorm(n)), "Standard log-normal distribution", "", left_lab = "")
p3 <- count_first_digit(runif(n, 0, 1), "Standard uniform distribution", "", left_lab = "")
p4 <- count_first_digit(runif(n, -95, 95), "[-95, 95] uniform distribution", "")
p5 <- count_first_digit(rgamma(n, 2), "Gamma distribution with shape 1, rate 1", "", bottom_lab = "Frequency", left_lab = "")
p6 <- count_first_digit(rcauchy(n), "Standard Cauchy distribution", "", left_lab = "")

CairoSVG("../img/0140-benford-sim.svg", 10, 6)
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)
dev.off()

