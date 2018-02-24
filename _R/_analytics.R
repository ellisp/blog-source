library(tidyverse)

#check out https://developers.google.com/analytcs/devguides/reporting/core/
library(googleAnalyticsR)

## authenticate via web browser
ga_auth()

ga_id <- 106185571



pages_orig <- google_analytics_4(ga_id, 
                   date_range = c("2015-01-01", as.character(Sys.Date())), 
                   metrics = "pageViews", 
                   dimensions = c("pagePath", "pageTitle"))


pages <- pages_orig %>%
  filter(pageViews != 0) %>%
  filter(!grepl("/translate_c", pagePath)) %>%
  filter(! pagePath %in% c("/", "/index.html", "/home")) %>%
  filter(! tolower(pageTitle) %in% tolower(c("Home Page", "(not set)", "About Peter's stats stuff", 
                                             "About \"Peter's stats stuff\"", "Sorry, nothing to see here", 
                                             "Acknowledgements", "Contacts", 
                                             "Post listing by date", "Blog listing grouped by tag"))) %>%
  filter(!grepl("compliance-", pagePath)) %>%
  filter(!grepl("sharebutton", pageTitle)) %>%
  filter(!grepl("share-button", pageTitle)) %>%
  filter(!grepl("SCENE_FETCHQUEUE", pageTitle)) %>%
  group_by(pageTitle) %>%
  arrange(desc(pageViews)) %>%
  # use just the most commonly used URL for each page:
  summarise(pageViews = sum(pageViews),
            pagePath = pagePath[1]) %>%
  # strip trailing "/" from the time preceding the Jekyll upgrade:
  mutate(pagePath = gsub("/$", "", pagePath)) %>%
  ungroup() %>%
  mutate(pageViews_prop = pageViews / sum(pageViews)) %>%
  filter(pageViews > 30) 
 

#=========Image for 'most popular' page========

p <- pages %>%
  arrange(desc(pageViews)) %>%
  dplyr::slice(20:1) %>%
  mutate(pageTitle = factor(pageTitle, levels = pageTitle)) %>%
  ggplot(aes(x = pageViews_prop, y = pageTitle)) +
  geom_point(colour = "steelblue") +
  scale_x_continuous("Proportion of all page views", label = percent) +
  labs(y = "") +
  ggtitle("Most viewed pages on Peter's Stats Stuff",
          "Excluding listings, 'about', and similar administrative pages")

svg("../img/analytics-pageviews.svg", 11, 7)
print(p)
dev.off()

convert_pngs("analytics")

#==============Create the 'include' table excerpt===========


pagesnip <- pages %>%
  arrange(desc(pageViews)) %>%
  mutate(snip = paste0("<li><a href='", pagePath, "'>", pageTitle, "</a></li>")) %>%
  dplyr::slice(1:100)


writeLines(pagesnip$snip, con = "../_includes/most-popular.html")

