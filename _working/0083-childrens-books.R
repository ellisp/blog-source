# great gore vidal article http://www.nybooks.com/articles/1964/12/03/the-writing-of-e-nesbit/

library(tidyverse)
library(xlsx)
library(cluster)
library(ape)
library(dendextend) 

download.file("http://ellisp.github.io/data/childrens-books.xlsx", 
              destfile = "tmp.xlsx", mode = "wb")
books_orig <- read.xlsx("tmp.xlsx", sheetIndex = 1, 
                        check.names = FALSE, stringsAsFactors = FALSE)
unlink("tmp.xlsx")

books_thin <- books_orig %>%
   gather(Trope, Present, -Author, -Title) %>%
   filter(!is.na(Present)) %>%
   as_tibble() %>%
   mutate(Present = 1)

books_wide <- books_thin %>%
   select(-Author) %>%
   spread(Trope, Present, fill = 0)

books_mat <- books_wide %>%
   select(-Title) %>%
   as.matrix()
rownames(books_mat) <- books_wide$Title


#---------cluster analysis-------
books_d <- daisy(books_mat)

cld <- as.dendrogram(diana(books_d))


# Three versions of the dendrogram:

# need to use hang.dendrogram to keep the hanging raggedness
svg("../img/0083-dendro1.svg", 10, 10)
par(family = "myfont")
par(mar = c(5,4,4,7), font.main = 1)
par(bg = "grey99")
plot(hang.dendrogram(cld), horiz = TRUE, yaxt = "n", type = "triangle",
     xlim = c(4.5, -1), 
     edgePar = list(col = "steelblue"),
     main = "Selected childrens books or series, clustered by thematic elements")
dev.off()

svg("../img/0083-dendro2.svg", 10, 8)
par(family = "myfont")
par(mar = c(1,1,1,1))
par(bg = "grey99")
plot(as.phylo(cld), type = "unrooted", cex = 0.6) # maybe
dev.off()

svg("../img/0083-dendro3.svg", 10, 10)
par(family = "myfont")
par(bg = "grey99")
plot(as.phylo(cld), type = "fan", cex = 0.6) # maybe
dev.off()

#-----------principle components-------------
books_pc <- prcomp(books_mat, scale = TRUE)

svg("../img/0083-pc.svg", 10, 10)
par(family = "myfont")
par(bg = "grey99")
biplot(books_pc, choices = 1:2, col = c("darkblue",  "grey75"), pc.biplot = TRUE)
dev.off()

convert_pngs("0083")
