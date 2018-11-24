
#====================counting digits in page numbers===============
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


plot(density(rcauchy(100)))

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

#================Benford's 'law'============

#-----------set up---------------
#' Function to draw a chart of frequency of first digits and compare to that predicted by Benford's 'law'
#' 
#' @param x a univariate numeric vector
#' @param desc description of x to use as subtitle 
#' Other parameters are self-explanatory
#' 
#' See https://en.wikipedia.org/wiki/Benford%27s_law
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
    scale_fill_manual("", values = c("grey85", "darkblue"),
                      guide = guide_legend(reverse = TRUE)) +
    ggtitle(main_title,
            desc) +
    labs(x = left_lab, y = bottom_lab) +
    theme(legend.position = legend.position)

  return(p)    

}

#---------------with simulated distributions---------------------
n <- 1000000
p1 <- count_first_digit(rnorm(n), "Standard normal distribution", legend.position = c(0.7, 0.7),
                        main_title = "Frequency of first digits, simulated")
p2 <- count_first_digit(exp(rnorm(n)), "Standard log-normal distribution", "", left_lab = "")
p3 <- count_first_digit(runif(n, 0, 1), "Standard uniform distribution", "", left_lab = "")
p4 <- count_first_digit(runif(n, -85, 85), "[-85, 85] uniform distribution", "")
p5 <- count_first_digit(rgamma(n, 2), "Gamma distribution with shape 1, rate 1", "", 
                        bottom_lab = "Frequency", left_lab = "")
p6 <- count_first_digit(rcauchy(n), "Standard Cauchy distribution", "", left_lab = "")

CairoSVG("../img/0140-benford-sim.svg", 10.5, 6)
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)
dev.off()

#--------with some real datasets----------
p1 <- count_first_digit(AirPassengers, "Monthly Airline Passenger Numbers\n1949-1960", 
                        main_title = "Frequency of first digits, real data",
                        legend.position = c(0.7, 0.7))

p2 <- count_first_digit(BJsales, "\nBox & Jenkins Sales Data", "", left_lab = "")

p3 <- count_first_digit(CO2$uptake, "\nCarbon dioxide uptake in Grass Plants", "", left_lab = "")

p4 <- count_first_digit(EuStockMarkets, "Daily Closing Prices of Major European\nStock Indices, 1991-1998", 
                        "")

p5 <- count_first_digit(Seatbelts[ , "DriversKilled"], "\nRoad Casualties in Great Britain 1969-84", 
                        "", left_lab = "")

p6 <- count_first_digit(morley[, "Speed"], "\nSpeed of light estimated by Michelson", "", left_lab = "")

CairoSVG("../img/0140-benford-real.svg", 10.5, 6)
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)
dev.off()

CairoSVG("../img/0140-benford-population.svg", 8, 4)
count_first_digit(population$population, "National population figures")
dev.off()

convert_pngs("0140")
