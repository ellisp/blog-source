my_plot_estimates <- function(estimates, 
                              extra_title = "", 
                              caption = "", 
                              y_max = 1500, 
                              location = " in Victoria"){
  my_theme <- my_theme +
    theme(axis.text.x = element_text(angle = 45, size = 8, hjust = 1)) 
  
  p <- estimates$plots$summary
  
  p1 <- p$patches$plots[[1]] +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    my_theme +
    theme(legend.position = "none",
          panel.grid.minor = element_blank()) +
    coord_cartesian(ylim = c(0, y_max)) +
    labs(title = glue("Estimated expected incidence{location} based on confirmations{extra_title}"),
         x = "") +
    scale_x_date(date_breaks = "1 week", date_labels = "%d %b", limits = range(p$data$date))
  
  p2 <- p$patches$plots[[2]] +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    my_theme +
    theme(legend.position = "none",
          panel.grid.minor = element_blank()) +
    coord_cartesian(ylim = c(0, y_max)) +
    labs(title = glue("Estimated infections{location} taking delay{extra_title} into account"),
         x = "") +
    scale_x_date(date_breaks = "1 week", date_labels = "%d %b", limits = range(p$data$date))
  
  todays_r <- p$data %>%
    filter(date == Sys.Date())
  st <- str_wrap(glue("Estimated R on {format(Sys.Date(), format = '%d %B')} 
             is between {format(todays_r$lower, digits = 2, nsmall = 2)} and 
             {format(todays_r$upper, digits = 2, nsmall = 2)}. The best point estimate is 
              {format(todays_r$median, digits = 2, nsmall = 2)}."), 100)
  
  
  p3 <- p$data %>% 
    ggplot(aes(x = date, y = median, fill = type)) +
    my_theme +
    geom_hline(yintercept = 1, colour = "steelblue") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
    geom_ribbon(aes(ymin = bottom, ymax = top), alpha = 0.1) +
    geom_line(aes(colour = type)) +
    theme(legend.position = "none",
          panel.grid.minor = element_blank()) +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    labs(title = glue("Effective Reproduction Number, correcting for both delay and right truncation{extra_title}"),
         subtitle = st,
         y = bquote("Estimated" ~ R[t]),
         x = "",
         caption = caption)  +
    scale_x_date(date_breaks = "1 week", date_labels = "%d %b", limits = range(p$data$date))
  
  pc <- p1 + p2 + p3 + plot_layout(ncol = 1)
  
  return(pc)
}
