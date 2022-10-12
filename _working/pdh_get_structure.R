

library(xml2)
library(tidyverse)


get_pdh_codelists <- function(dataflow = NULL,
                              version = "1.0",
                              str_url = NULL){
  
  if(is.null(str_url)){
    str_url <- paste0("https://stats-nsi-stable.pacificdata.org/rest/dataflow/SPC/",
                      dataflow,
                      "/",
                      version,
                      "?references=all")
  } 
  
  str_raw <- read_xml(str_url)
  
  if(length(str_raw) != 2) {
    stop("Expected sructure to be a list of two elements")
  }
  
  st <- str_raw |>
    xml_child(2) |>
    as_list() 
  
  codelists <- st$Codelists
  
  all_codelists <- list()
  
  # What are these codelists:
  codelist_names_en <- as.character(sapply(codelists, function(x) x[1]$Name))
  codelist_names_fr <- as.character(sapply(codelists, function(x) x[2]$Name))
  
  for(i in 1:length(codelists)){
    this_cl <- codelists[[i]]
    
    # knock out the elements that are one element long, these are just the English and French names
    for(j in length(this_cl):1){
      if(length(this_cl[[j]]) == 1){
        this_cl[[j]] <- NULL
      }
    }
    
    all_codelists[[i]] <- tibble::tibble(
      id = sapply(this_cl, function(x){attr(x, "id")}),
      name = as.character(sapply(this_cl, function(x){x$Name})),
      category_en = codelist_names_en[i],
      category_fr = codelist_names_fr[i]
    )
    
  }

  return(dplyr::bind_rows(all_codelists))
}

eg1 <- get_pdh_codelists(dataflow = "DF_EMPRATES", version = "1.0")
eg2 <- get_pdh_codelists("DF_FOOD_NUTRIENTS")
eg3 <- get_pdh_codelists("DF_KEYFACTS")  

count(eg1, category_en)
count(eg2, category_en)
count(eg3, category_en)

filter(eg3, category_en == "Codelist for KEYFACTS indicators")
