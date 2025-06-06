convert_pngs <- function(pattern, directory = "../img"){
    if(!is.na(as.numeric(pattern))){
      pattern <- paste0("^", pattern)
    }
  
   origdir <- setwd(directory)
   files <- list.files(recursive = TRUE, full.names = TRUE, pattern = pattern)
   files <- files[grepl("\\.svg$", files)]
   for(i in files){
      output <- gsub("svg$", "png", i)
      cmd <- paste0('\"C:\\Program Files\\ImageMagick-7.0.2-Q16\\magick\"', " -density 300 ", i, " ", output)
      system(cmd)
      
   }
   setwd(origdir)
   beepr::beep(4)
}

convert_election_pngs <- function(files = c("gam-final-chances-bar.svg",
                                            "gam-vote-predictions.svg",
                                            "gam-vote-predictions-density.svg",
                                            "gam-final-chances-histogram.svg")){
   setwd("../img")
   for(i in files){
      output <- gsub("svg$", "png", i)
      cmd <- paste0('\"C:\\Program Files\\ImageMagick-7.0.2-Q16\\magick\"', " ", i, " ", output)
      system(cmd)
      
   }
   setwd("../_working")
   beepr::beep(4)
}


rmd2jekyll <- function(filename){
   # adapted from https://github.com/nicolewhite/nicolewhite.github.io/blob/master/_posts/2015-02-07-r-blogging-with-rmarkdown-knitr-jekyll.md 
   #filename = "explore_cars.rmd"
   filename_full <- paste0("_drafts/", filename)
   
   require(knitr)
   
   # Check that it's a .Rmd file.
   if(!grepl(".rmd", tolower(filename))) {
      stop("You must specify a .Rmd file.")
   }
   
   # Knit and place in _posts.
   dir = paste0("_posts/", Sys.Date(), "-")
   output = paste0(dir, sub('.rmd', '.html', tolower(filename)))
   knit2html(filename_full, output, fragment.only = TRUE)
   
   # cleam up the .md file created as a sideproduct
   unlink(gsub(".rmd", ".md", tolower(filename)))
   
    # Copy .png files to the images directory.
    fromdir = "{{ site.url }}/img/posts"
    todir = "img/posts"
    
    pics = list.files(fromdir, ".png")
    pics = sapply(pics, function(x) paste(fromdir, x, sep="/"))
    file.copy(pics, todir)   
   
   unlink("{{ site.url }}", recursive = TRUE)
   
   
   firstcontent <- readLines(filename_full)
   l <- which(substr(firstcontent, 1, 3) == '---')
   content <- readLines(output)
   content <- c(firstcontent[l[1]:l[2]], content)
   
   write.table(content, output, row.names = FALSE, quote = FALSE, col.names=FALSE)
 
}



