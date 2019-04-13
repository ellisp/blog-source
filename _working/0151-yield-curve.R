

library(rvest)
library(tidyverse)
library(scales)
library(gganimate)
library(svglite)

# we read in the data a year at a time because although there is a page with all 29 years of data,
# it was too difficult to read in all at once ie it crashed my humble laptop:
read_a_year <- function(y){
  url_stub <- "https://www.treasury.gov/resource-center/data-chart-center/interest-rates/pages/TextView.aspx?data=yieldYear&year=XXXX"
  url <- gsub("XXXX", y, url_stub)
  
  yield_page <- read_html(url)
  yield_data <- as_tibble(html_table(yield_page, fill = TRUE)[[2]] )
  
  return(yield_data)
  
}

yields_l <- lapply(1990:2019, read_a_year)


periods <- tibble(
  period = paste(c(1,2,3,6,1,2,3,5,7,10,20,30), rep(c("mo", "yr"), c(4,8))),
  period_n = c(30, 60, 90, 180, 365.25 * c(1,2,3,5,7,10,20,30))
)

yields <- do.call("rbind", yields_l) %>%
  mutate(Date = as.Date(Date, format = c("%m/%d/%y"))) %>%
  gather(period, value, -Date) %>%
  mutate(value = suppressWarnings(as.numeric(value))) %>% 
  left_join(periods, by = "period") %>%
  group_by(Date) %>%
  mutate(ratio_5_30 = value[period == "30 yr"] / value[period == "5 yr"],
         yield_av = mean(value, na.rm = TRUE, tr = 0.2),
         yield_30 = value[period == "30 yr"],
         yield_5 = value[period == "5 yr"]) %>%
  ungroup() %>%
  filter(!is.na(value)) %>%
  arrange(Date)
  
p1 <- yields %>% 
  filter(Date < as.Date("1990-01-31")) %>%
  ggplot(aes(x = period_n, y = value)) +
  geom_path() +
  facet_wrap(~Date) +
  scale_y_continuous("Treasury yield curve rate") +
  scale_x_continuous("", breaks = periods[c(10:12), ]$period_n,
                     labels = periods[c(10:12), ]$period)

col_br <- tibble(
  lab = c(1990, 2000, 2010),
  date = as.Date(paste0(c(1990, 2000, 2010), "-03-01"))
) %>%
  mutate(date_n = as.numeric(date))

p2 <- yields %>% 
#  filter(Date < as.Date("1990-01-31")) %>%
  ggplot(aes(x = period_n, y = value, group = Date, colour = as.numeric(Date))) +
  geom_path(alpha = 0.1) +
  scale_colour_viridis_c("",
                         breaks = pull(col_br, date_n),
                         labels = pull(col_br, lab)) +
  scale_y_continuous("Treasury yield curve rate") +
  scale_x_continuous("", breaks = periods[c(10:12), ]$period_n,
                     labels = periods[c(10:12), ]$period)

print(p2)

d <- yields %>% 
  filter(Date < as.Date("1991-01-31"))

 a <- d %>% 
  ggplot(aes(x = period_n, y = value, colour = yield_av)) +
  # geom_segment(data = distinct(d, Date, yield_30, yield_5), 
  #              x = 1826, xend = 10958, aes(y = yield_5, yend = yield_30),
  #              colour = "grey50") +
  geom_path(size = 1.5) +
  scale_y_continuous("Treasury yield curve rate") +
  scale_x_continuous("", breaks = periods[c(10:12), ]$period_n,
                     labels = periods[c(10:12), ]$period) +
  scale_colour_viridis_c("Unweighted average yield", option= "A", direction = -1) +
  labs(
    subtitle = "Daily US Treasury Yield Curve Rates 1990 to 2019",
    title = 'Date: {frame_time}') +
  transition_time(Date) +
  ease_aes('linear')

res <- 150
animate(a, nframes = 100, dev = "png", start_pause = 5, end_pause = 15, rewind = TRUE,
        type = "cairo-png", antialias = "subpixel", 
        width = 8 * res, height =  6 * res, res = res)

anim_save("0151-yield-anim.gif", path = "../img/")
