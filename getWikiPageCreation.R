##########################################
#
# Title: Create a dataset with dates of page creation
# Author: Guedes-Santos, J
#
# Ultimate version: 2019-03-02
#
##########################################

rm(list = ls())

library(tidyverse)
library(rvest)
library(data.table)


# Read PA Dataset
pa_original <- read_csv("./data/BrazilianProtectedAreas_2018-12-10_reviewed.csv")

# Create PA dataset with date of page creation
pa_dataset <- data.table(codcnuc = character(), pa_name = character(), pa_start_views = character(), pa_end_views = character(), page_creation = character())

# ---- English ----

# Load PA CSV ENG Files list
pa_csv_files <- list.files("./data/eng/")

# Read all CSV ENG names and get page creation from Wikipedia
for (i in 1:length(pa_csv_files)) {
  print(paste0(i, " in ", length(pa_csv_files)))

  # Split Filename

  pa_filename <- pa_csv_files[i]
   
  pa_properties <- unlist(strsplit(pa_filename, "_"))
  
  pa_cod_cnuc <- pa_properties[1]
  pa_start_views <- pa_properties[3]
  pa_end_views <- unlist(strsplit(pa_properties[4], "[.]"))[1]

  # Get PA Name from PA Original Dataset
  pa_name <- pa_original %>%
    filter(pa_original$codCnuc == pa_cod_cnuc) %>%
    select(enNameWiki)
  
  pa_name <- as.character(pa_name[1,1])
    
  # Get date of page creation 
  url_pa <- paste0("https://en.wikipedia.org/w/index.php?title=", gsub(" ", "_", pa_name) ,"&action=info")
  
  print(pa_name)
  print(url_pa)
  
  pa_page_content <- url_pa %>%
    read_html() %>%
    html_node("body #content #bodyContent #mw-content-text .mw-page-info #mw-pageinfo-firsttime") %>%
    xml_contents() %>%
    html_text()
  
  page_creation <- pa_page_content[2]
  
  # Add information to the dataset
  new_row <- data.table(codcnuc = pa_cod_cnuc, pa_name = pa_name, pa_start_views = pa_start_views, pa_end_views = pa_end_views, page_creation = page_creation)
  
  pa_dataset <- rbind(pa_dataset, new_row)
  
}

# ---- Portuguese ----

# Load PA CSV PT Files list
pa_csv_files <- list.files("./data/pt/")


# Read all CSV PT names and get page creation from Wikipedia
for (i in 1:length(pa_csv_files)) {
  print(paste0(i, "in", length(pa_csv_files)))
  
  # Split Filename
  
  pa_filename <- pa_csv_files[i]
  
  pa_properties <- unlist(strsplit(pa_filename, "_"))
  
  pa_cod_cnuc <- pa_properties[1]
  pa_start_views <- pa_properties[3]
  pa_end_views <- unlist(strsplit(pa_properties[4], "[.]"))[1]

  # Get PA Name from PA Original Dataset
  pa_name <- pa_original %>%
    filter(pa_original$codCnuc == pa_cod_cnuc) %>%
    select(ptNameWiki)
  
  pa_name <- as.character(pa_name[1,1])
  
    
  # Get date of page creation 
  url_pa <- paste0("https://pt.wikipedia.org/w/index.php?title=", gsub(" ", "_", pa_name) ,"&action=info")
  
  print(pa_name)
  print(url_pa)
  
  pa_page_content <- url_pa %>%
    read_html() %>%
    html_node("body #content #bodyContent #mw-content-text .mw-page-info #mw-pageinfo-firsttime") %>%
    xml_contents() %>%
    html_text()
  
  page_creation <- pa_page_content[2]
  
  # Add information to the dataset
  new_row <- data.table(codcnuc = pa_cod_cnuc, pa_name = pa_name, pa_start_views = pa_start_views, pa_end_views = pa_end_views, page_creation = page_creation)
  
  pa_dataset <- rbind(pa_dataset, new_row)
  
}

# ---- Test area ----
