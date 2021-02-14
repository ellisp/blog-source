

#=================Exploratory analysis of short positions=====================

# so some companies had moments when they were very heavily shorted - more than 16 times
most_shorted <- all_data %>%
  group_by(recent_product_name, product_code) %>%
  summarise(max_sp = max(short_position_prop),
            # earliest and latest date the firm had that position (noting not necessarily
            # continuous between these two dates)
            max_sp_date_early = min(date[short_position_prop == max_sp]),
            max_sp_date_late = min(date[short_position_prop == max_sp]),
            max_sp_days = sum(short_position_prop == max_sp)) %>%
  ungroup() %>%
  arrange(desc(max_sp)) 

# 35 firms had more than 100% of their float shorted
most_shorted %>%    filter(max_sp > 1)

# usually only for 1 day (but of course that's not surprising as I required it to exactly match)
most_shorted %>%
  filter(max_sp > 1) %>%
  arrange(desc(max_sp_days))


# 46 firms had at least sometimes 50% shorted
heavily_shorted <- most_shorted %>%   
  filter(max_sp > 0.5) %>%
  select(product_code, max_sp)

extremes <- most_shorted %>%
  filter(max_sp > 5) %>%
  rename(short_position_prop = max_sp,
         date = max_sp_date_early) %>%
  arrange(date) %>%
  select(recent_product_name, product_code, short_position_prop, date)


all_data %>%
  inner_join(heavily_shorted, by = "product_code") %>%
  ggplot(aes(x = date, y = short_position_prop, colour = product_code)) +
  geom_line() +
  geom_text_repel(data = extremes, aes(label = product_code)) +
  scale_y_sqrt(label = percent, breaks = c(0.3, 1, 5, 10, 15)) +
  theme(legend.position =  "none",
        panel.grid.minor.y = element_blank()) +
  labs(y = "Percentage of product issue\nreported as short position",
       x = "",
       title = glue("The {nrow(heavily_shorted)} most shorted firms on the ASX"),
       subtitle = "Four unusual shorting events since 2010, two of them involving multiple products.
Chart shows the short positions of all products that at least once had 50% of product issue reported as short position.",
       caption = "Source: ASIC short position reports")

extremes %>%
  rename("Most shorted as %" = short_position_prop) %>%
  kable(digits = 0) %>%
  kable_styling()



#---------------Explore stock price etc----------------------

all_stocks_data  %>%
  ggplot(aes(x = date, y = close_index, colour = ax_ticker)) +
  geom_line() +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank()) +
  scale_y_log10(label = comma_format(accuracy = 0.001)) +
  labs(x = "",
       y = "Closing price\n(as index, first observed value = 1)",
       title = "Closing stock prices of the 46 most shorted products on the ASX")


all_stocks_data %>%
  inner_join(all_data, by = c("date", "product_code")) %>%
  ggplot(aes(x = close_index, y = short_position_prop, colour = product_code)) +
  geom_path() +
  geom_smooth(aes(colour = NULL), method = "gam", colour = "black") +
  #geom_point() +
  scale_y_log10(label = percent) +
  scale_x_log10() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  labs(x = "Closing price (as index, first observed value = 1)",
       y = "Percentage of product issue\nreported as short position")


all_stocks_data %>%
  inner_join(all_data, by = c("date", "product_code")) %>%
  ggplot(aes(x = volume / 1000, y = short_position_prop, colour = product_code)) +
  geom_path() +
  geom_smooth(aes(colour = NULL), method = "gam", colour = "black") +
  scale_y_log10(label = percent) +
  scale_x_log10(label = comma_format(suffix = "k")) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  labs(x = "Volume of trades",
       y = "Percentage of product issue\nreported as short position")


#====================Combine the two=================

set.seed(126)
some_products <- sample(all_stocks_data$product_code, 13)

all_stocks_data %>%
  inner_join(all_data, by = c("date", "product_code")) %>%
  filter(product_code == some_products) %>%
  ggplot(aes(x = volume / 1000, y = short_position_prop, colour = year(date))) +
  geom_path() +
  geom_smooth(method = "gam", colour = "black") +
  facet_wrap(~glue("{product_code}: {recent_product_name}"), scales = "free") +
  scale_y_log10(label = percent) +
  scale_x_log10(label = comma_format(suffix = "k")) +
  scale_colour_viridis_c() +
  theme(legend.position = "right") +
  labs(x = "Volume of trades",
       y = "Percentage of product issue\nreported as short position",
       colour = "Year")


all_stocks_data %>%
  inner_join(all_data, by = c("date", "product_code")) %>%
  filter(product_code == some_products) %>%
  ggplot(aes(x = volume / 1000, y = close, colour = year(date))) +
  geom_path() +
  geom_smooth(method = "gam", colour = "black") +
  facet_wrap(~glue("{product_code}: {recent_product_name}"), scales = "free") +
  scale_y_log10(label = dollar) +
  scale_x_log10(label = comma_format(suffix = "k")) +
  scale_colour_viridis_c() +
  theme(legend.position = "right") +
  labs(x = "Volume of trades",
       y = "Closing price",
       colour = "Year")


all_stocks_data %>%
  inner_join(all_data, by = c("date", "product_code")) %>%
  filter(product_code == some_products) %>%
  ggplot(aes(x = close, y = short_position_prop, colour = year(date))) +
  geom_path() +
  geom_smooth(method = "gam", colour = "black") +
  facet_wrap(~glue("{product_code}: {recent_product_name}"), scales = "free") +
  scale_y_log10(label = percent) +
  scale_x_log10(label = dollar) +
  scale_colour_viridis_c() +
  theme(legend.position = "right") +
  labs(x = "Closing price",
       y = "Percentage of product issue\nreported as short position",
       colour = "Year")
