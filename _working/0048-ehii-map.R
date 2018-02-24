setwd(proj_dir)

library(cshapes)      # for historic country boundaries.  Load this early as it calls plyr which clashes with dplyr
library(ggplot2)
library(scales)
library(RColorBrewer) # for brewer.pal(...)
library(openxlsx)
library(countrycode)  # for ISO country codes
library(ggthemes)     # for theme_map
library(dplyr)
library(tidyr)        # for reshaping the UTIP EHII data



# if we didn't already download it some previous day, download the UTIP EHII data:
if(!file.exists("ehii.xlsx")){
   download.file("http://utip.lbj.utexas.edu/data/EHII-UPDATED-10-30-2013.xlsx",
              destfile = "ehii.xlsx", mode = "wb")
}

ehii <- read.xlsx("ehii.xlsx")[ , -1] # don't need the first column

ehii_tidy <- ehii %>%
   gather(Year, Gini, -Country, -Code) %>%
   mutate(Year = as.numeric(Year)) %>%
   mutate(iso3c = ifelse(Code == "GER", "DEU", Code),
          iso3c = ifelse(Code == "YUG", "SCG", iso3c)) %>%
   left_join(countrycode_data[ , c("iso3c", "iso2c")], by = "iso3c")
# Germany after unification had the wrong code; change to DEU
# YUG has data 1994 to 1998 and had dubious status. Recode as SCG (Serbia and Montenegro)


# set the global historical limits of the Gini coefficients
limits = c(min(ehii_tidy$Gini, na.rm = TRUE) - 5, 
           max(ehii_tidy$Gini, na.rm = TRUE) + 5)



#=======loop drawing the individual frames of the animation starts here=========
setwd("_output/0048")

for(i in min(ehii_tidy$Year + 2):max(ehii_tidy$Year - 2)){

   # thanks to cshapes project for historical countries: http://nils.weidmann.ws/projects/cshapes.html
   
   
   # Download a shapefile of country boundaries.
   # This was first set up to use 1988 boundaries for some maps, 1995 for others;
   # but it turns out that if we use 1995 boundaries all the time the only
   # old country we miss out on is East Germany, and we gain PNG and Eritrea
   # Also note that Hong Kong, Macau and Puerto Rico have data that can't be
   # shown with formal country boundaries in any cshapes maps.  I've left this
   # so it is easy to change to use 1988 boundaries sometimes, but at the moment
   # it will only do this when the year is earlier than 1960 (never, with current
   # inequality data from UTIP)
   if (i < 1960){
      world <- cshp(date = as.Date(paste0(1988, "-06-01")))      
   } else {
      world <- cshp(date = as.Date(paste0(1995, "-06-01")))      
   }

   # Some manual tweaks to the shapefile to make an ISO3 country code variable
   # compatible with that in the UTIP inequality data.
   world@data$iso3c <- ifelse(world@data$CNTRY_NAME == "Pakistan",
                              "PAK", as.character(world@data$ISO1AL3))
   world@data$iso3c <- ifelse(world@data$CNTRY_NAME == "Libya", "LBY", world@data$iso3c)
   world@data$iso3c <- ifelse(grepl("zech", world@data$CNTRY_NAME), "CZE", world@data$iso3c)
   world@data$iso3c <- ifelse(grepl("Angola", world@data$CNTRY_NAME), "AGO", world@data$iso3c)
   world@data$iso3c <- ifelse(grepl("Bangladesh", world@data$CNTRY_NAME), "BGD", world@data$iso3c)
   
   # convert the shapefile into a data frame for use with ggplot2,
   # and join it to the ISO3 country codes for later use:
   world_map <- fortify(world) %>%
      left_join(data_frame(id = rownames(world@data), iso3c = world@data$iso3c)) 
   
   # set the five year window for which we want the moving average
   years <- (i - 2):(i + 2)
   
   # filter the data to those five years and take the average
   # of any years of data we have for each country in those 
   # years:
   the_data <- ehii_tidy %>%
      filter(Year %in% years) %>%
      group_by(iso3c) %>%
      dplyr::summarise(Gini = mean(Gini, na.rm = TRUE)) %>%
      filter(!is.na(Gini))
   
   # these next messages and printing are checks to see which
   # if any countries we are missing out on drawing due to
   # data mismatches:
      all_gini_countries <- unique(the_data$iso3c)
      all_map_countries <- unique(world_map$iso3c)
      
      missed1 <- sum(!all_map_countries %in% all_gini_countries)
      missed2 <- sum(!all_gini_countries %in% all_map_countries)
      missed3 <- sum(is.na(world@data$iso3c))
      message(paste(missed1, "countries in map but no Gini", i))
      message(paste(missed2, "countries have a Gini but no map in ", i))
      message(paste(missed3, "countries have no iso3c code in", i))
      
      # print the countries that had data which wasn't mapped
      print(all_gini_countries[!all_gini_countries %in% all_map_countries])
   
      # Puerto Rico, Macau and Hong Kong often are missing from the map
      # DDR has data 1970 to 1988 and won't be shown
      # ERI has data from 1963 despite not really being a country until 1993
      #     If we use 1995 maps it will show up but we have to drop DDR.
      # PNG has data pre its independence in 1975; need a map from later than
      #     1975 for its borders to show up.

   # Back to the main routine needed for drawing the map.         
   # Merge the Gini coefficient data with the world map
   the_data <- right_join(the_data, world_map, by = "iso3c")
   
   # Draw the map
   p <- ggplot(the_data) +
      aes(x = long, y = lat, group = group, fill = Gini) +
      geom_polygon(colour = "grey60", size = 0.3) +
      theme_map(base_size = 23, base_family = "myfont") +
      coord_equal() +
      scale_fill_gradientn(colours = brewer.pal(9, "RdYlBu")[9:1], 
                           limits = limits) +
      theme(legend.position = c(0.1, 0.1)) +
      annotate("text", x = 0, y = -48, label = paste0(i - 2, " to ", i + 2),
               size = 12, colour = "grey30") +
      annotate("text", x = -137, y = -50, label = "Red = more inequality", colour = "darkred") +
      annotate("text", x = -137, y = -54, label = "Blue = less inequality", colour = "blue") +
      labs(fill = "Mean Gini\ncoefficient",
           caption = "Map by http://ellisp.github.io/\nData from University of Texas Inequality Project 'Estimated Household Income Inequality'") +
      ggtitle("Changing inequality estimates over time",
              subtitle = "Full animation shows 1963 to 2008, five years at once") +
      theme(legend.title.align = 0.5)
      
   # save the frame as an image
   png(paste0(i, ".png"), 1800, 950, res = 100)
      print(p)
   dev.off()
       
}

# Combine all the frames into a single animated GIF, using ImageMagick
# note since upgrade to v7 ImageMagick uses magick not convert, which makes
# things much easier for Windows users (no conflict with Windows convert, which
# does something completely different)
system('magick -loop 0 -delay 80 *.png "0048-ehii.gif"')

# go back to eh root directory
setwd(proj_dir)

# move the asset over to where needed for the blog
file.copy("_output/0048/0048-ehii.gif", "../img/0048-ehii.gif", overwrite = TRUE)
