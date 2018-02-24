
# hmm, see http://www.politifact.com/punditfact/statements/2017/feb/10/sean-hannity/no-9th-circuit-isnt-most-overturned-court-country-/ for more up to date source
# and http://www.scotusblog.com/reference/stat-pack/

library(tidyverse)
library(scales)
library(stringr)
library(tabulizer)
library(forcats)
library(grid)

#-----------------download file and extract the tables---------------------
thefile <- tempfile()

download.file("http://www.americanbar.org/content/dam/aba/migrated/intelprop/magazine/LandslideJan2010_Hofer.authcheckdam.pdf",
              destfile = thefile, mode = "wb")

tabs <- extract_tables(thefile)

#----------------------prepare data---------------
total_cases <- as.data.frame(tabs[[1]][ -(1:2), ])
names(total_cases) <- c("Court", 1999:2008, "Total")

total_cases <- total_cases %>%
   gather(Year, Cases, -Court) %>%
   mutate(Cases = as.numeric(gsub(",", "", Cases)),
          Court = str_trim(Court))

svg("../img/0081-cases-time.svg", 8, 6)
total_cases %>%
   filter(Year != "Total") %>%
   mutate(Year = as.numeric(Year)) %>%
   ggplot(aes(x = Year, y = Cases)) +
   geom_point() +
   geom_line() +
   facet_wrap(~Court, scales = "free_y", ncol = 3) +
   scale_y_continuous("Total appeals terminated\n", label = comma)
dev.off()

scotus <- as.data.frame(tabs[[3]][-(1:3), 1:5])
scotus <- cbind(scotus[ , -5], str_split(scotus[ , 5], " ", simplify = TRUE)[ , 1:2])
names(scotus) <- c("Court", "Reversed", "Vacated", "Affirmed", "Reversed + Vacated", "Appealed")
scotus <- scotus %>%
   mutate(Court = str_trim(Court)) %>%
   gather(Result, Number, - Court) %>%
   mutate(Number = as.numeric(as.character(Number))) %>%
   spread(Result, Number)
   
scotus

combined <- total_cases %>%
   filter(Year == "Total" & Court != "Annual Totals") %>%
   select(Court, Cases) %>%
   left_join(scotus, by = "Court") %>%
   mutate(rv_prop_cases = `Reversed + Vacated` / Cases,
          rv_prop_appealed = `Reversed + Vacated` / Appealed) %>%
   arrange(desc(rv_prop_cases)) %>%
   mutate(Court = factor(Court, levels = Court))
   
svg("../img/0081-total-ratios.svg", 10, 6)   
combined %>%
   select(Court, Cases, rv_prop_cases, rv_prop_appealed, Appealed) %>%
   gather(Denominator, Proportion, -Court, -Cases, -Appealed) %>%
   mutate(Denominator = ifelse(grepl("appealed", Denominator), "Percentage of total appeals", "Percentage of total cases"),
          Denominator_value = ifelse(grepl("total cases", Denominator), Cases, Appealed)) %>%
   ggplot(aes(x = Proportion, y = Court, size = Cases, label = Denominator_value)) +
   # force x axis to go to zero by drawing an invisible line:
   geom_vline(xintercept = 0, alpha = 0) +
   facet_wrap(~Denominator, scales = "free_x") +
   geom_text(colour = "steelblue") +
   scale_x_continuous(label = percent) +
   scale_size_area("Total original cases:", label = comma) +
   theme(legend.position = "none") +
   ggtitle("Federal courts of appeals cases reversed or vacated by the Supreme Court 1999 - 2008", 
           "From the 9th Circuit, 80% of 175 cases that went to SCOTUS were overturned,
but that was only 0.12% of the 114,199 total cases originally decided by that circuit in the period.") +
   labs(caption = "Data from http://www.americanbar.org/content/dam/aba/migrated/intelprop/magazine/LandslideJan2010_Hofer.authcheckdam.pdf
        Analysis at http://ellisp.github.io",
        y = "Appeals circuit\n\n",
        x = "Horizontal position indicates the proportion of cases overturned by the Supreme Court.
The printed number is the denominator - total cases that could have been overturned at that stage.
The size is proportionate to the total cases decided by each circuit.") 

grid.text(0.75, 0.26, 
          label = "But only a tiny percentage of the number\nof cases originally decided by the circuit judges.",
          gp = gpar(cex = 0.75, family = "myfont", col = "grey50"))

grid.text(0.35, 0.3, 
          label = "A high proportion of the small number of\ncases that are taken to the Supreme Court\ndo end up reversed or vacated.",
          gp = gpar(cex = 0.75, family = "myfont", col = "grey50"))
dev.off()


#--------------barchart version------------
svg("../img/0081-bars.svg", 10, 6) 
combined %>%
   mutate(`Not considered by SCOTUS` = Cases - Appealed) %>%
   select(Court, `Not considered by SCOTUS`, Affirmed, `Reversed + Vacated`) %>%
   gather(Result, Number, -Court) %>%
   mutate(Result = ifelse(Result == "Reversed + Vacated", "Overturned", Result),
          Result = factor(Result, levels = c("Overturned", "Affirmed", "Not considered by SCOTUS"))) %>%
   ggplot(aes(x = Court, weight = Number, fill = Result)) +
   geom_bar(position = "stack") +
   coord_flip() +
   scale_y_continuous(label = comma) +
   scale_fill_discrete(guide = guide_legend(reverse = TRUE)) +
   ggtitle("Federal courts of appeals cases reversed or vacated by the Supreme Court 1999 - 2008") +
   labs(caption = "Data from http://www.americanbar.org/content/dam/aba/migrated/intelprop/magazine/LandslideJan2010_Hofer.authcheckdam.pdf
        Analysis at http://ellisp.github.io",
        x = "Appeals circuit", y = "Number of appeal cases")
dev.off()   
   
#==============inference===========
model_data <- combined %>%
   mutate(Court = relevel(factor(Court), ref = "Eighth Circuit"))

#----------------proportion of original------------
model1 <- glm(rv_prop_cases ~ Court, family = "binomial", weights = Cases, data = model_data)
anova(model1, test = "Chi")
res <- confint(model1)[-1, ]

res_df <- as.data.frame(res) %>%
   mutate(Court = gsub("Court", "", rownames(res)),
          center = (`2.5 %` + `97.5 %`) / 2) %>%
   mutate(Court = fct_reorder(Court, center))

svg("../img/0081-model-results.svg", 10, 6)
ggplot(res_df, aes(y = Court, yend = Court, x = `2.5 %`, xend = `97.5 %`)) +
   geom_segment() +
   geom_point(aes(x = center)) +
   labs(x = "95% confidence interval for increase in logarithm of odds of a completed case 
being appealed successfully by Supreme Court, relative to the Eighth Circuit",
         y = "",
        caption = "Data from http://www.americanbar.org/content/dam/aba/migrated/intelprop/magazine/LandslideJan2010_Hofer.authcheckdam.pdf
        Analysis at http://ellisp.github.io")
dev.off()


#============save PNG versions=========
setwd("../img")
files <- list.files()
files <- files[grepl("^0081.+svg$", files)]
for(i in files){
   output <- gsub("svg$", "png", i)
   cmd <- paste0('\"C:\\Program Files\\ImageMagick-7.0.2-Q16\\magick\"', " ", i, " ", output)
   system(cmd)
   
}
setwd("../_working")