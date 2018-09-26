library(tidyverse)
library(lubridate)
library(stringr)
library(tabulizer)

dir.create("sl-tourism")

download.file("http://www.sltda.gov.lk/sites/default/files/monthly-international-tourist-arrivals-2018.xlsx",
              destfile = "sl-tourism/monthly-international-tourist-arrivals-2018.xlsx", mode = "wb")

mons <- as.character(month(1:12, label = TRUE))
yrs <- 15:18
base_url <- "http://www.sltda.lk/sites/default/files/Page2XXXYY.pdf"

# Download all the monthly PDFs
for(i in 1:length(mons)){
  for(j in 1:length(yrs)){
    url <- gsub("XXX", mons[i], base_url)
    url <- gsub("YY", yrs[j], url)
    fn <- paste0("sl-tourism/", str_extract(url, "Page.+pdf$"))
    try(download.file(url, destfile = fn, mode = "wb"))
  }
}

# Extract the tables from the PDFs and make each one a dataframe element of a big list:
all_tables <- list()
k <- 1
all_files <- list.files(recursive = TRUE)
for(i in 1:length(mons)){
  for(j in 1:length(yrs)){
    url <- gsub("XXX", mons[i], base_url)
    url <- gsub("YY", yrs[j], url)
    fn <- paste0("sl-tourism/", str_extract(url, "Page.+pdf$"))
    if(fn %in% all_files){
    
      tmp <- as.data.frame(extract_tables(fn)[[1]])
      names(tmp) <- c("country", "yr_minus_one", "yr", "change")
      tmp <- tmp %>%
        mutate(yr_minus_one = as.numeric(gsub(",", "", yr_minus_one)),
               yr = as.numeric(gsub(",", "", yr)),
               change = as.numeric(change)) %>%
        select(-change) %>%
        gather(year, value, - country) %>%
        mutate(year = gsub("^yr$", yrs[j], year),
               year = gsub("^yr_minus_one$", yrs[j] - 1, year),
               year = as.numeric(year) + 2000,
               month = mons[i],
               mon_num = i,
               yr_mon = year + (mon_num - 0.5) / 12)
      all_tables[[k]] <- tmp
      k <- k + 1
    }
  }
}

# simplify the list of tables into one big data frame:
arrivals <- do.call("rbind", all_tables)
summary(arrivals)
head(arrivals)             

arrivals %>%
  ggplot(aes(x = yr_mon, y = value, colour = country)) +
  geom_line() +
  theme(legend.position = "none")
