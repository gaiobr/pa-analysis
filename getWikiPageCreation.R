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

# ---- Create PA dataset with date of page creation ----
pa_dataset_eng <- data.table(codcnuc = character(), pa_name = character(), pa_start_views = character(), pa_end_views = character(), page_creation = character())

pa_dataset_pt <- data.table(codcnuc = character(), pa_name = character(), pa_start_views = character(), pa_end_views = character(), page_creation = character())

# ---- Read CSV Eng Files and extract information from filenames ----
pa_eng_files <- list.files("./data/eng/")

pa_info_eng <- data.table(
  codcnuc = character(), 
  pa_start_views = character(), 
  pa_end_views = character()
)

for (i in 1:length(pa_eng_files)) {
  # Split Filename
  
  pa_filename <- pa_eng_files[i]
  
  pa_properties <- unlist(strsplit(pa_filename, "_"))
  
  pa_cod_cnuc <- pa_properties[1]
  pa_start_views <- pa_properties[3]
  pa_end_views <- unlist(strsplit(pa_properties[4], "[.]"))[1]
  
  # Add information to the dataset
  new_row <- data.table(
    codcnuc = pa_cod_cnuc, 
    pa_start_views = pa_start_views, 
    pa_end_views = pa_end_views
  )
  
  pa_info_eng <- rbind(pa_info_eng, new_row)
}

# ---- Read CSV Pt Files and extract information from filenames ----
pa_pt_files <- list.files("./data/pt/")

pa_info_pt <- data.table(
  codcnuc = character(), 
  pa_start_views = character(), 
  pa_end_views = character()
)

for (i in 1:length(pa_pt_files)) {
  # Split Filename
  
  pa_filename <- pa_pt_files[i]
  
  pa_properties <- unlist(strsplit(pa_filename, "_"))
  
  pa_cod_cnuc <- pa_properties[1]
  pa_start_views <- pa_properties[3]
  pa_end_views <- unlist(strsplit(pa_properties[4], "[.]"))[1]
  
  # Add information to the dataset
  new_row <- data.table(
    codcnuc = pa_cod_cnuc, 
    pa_start_views = pa_start_views, 
    pa_end_views = pa_end_views
  )
  
  pa_info_pt <- rbind(pa_info_pt, new_row)
}


# ----ENG PAGE CREATION: Read all CSV ENG names and get page creation from Wikipedia ----
for (i in 1:length(pa_eng_files)) {
  print(paste0(i, " in ", length(pa_eng_files)))

  # Split Filename

  pa_filename <- pa_eng_files[i]
   
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
  
  # Split character variable and standardize date
  page_creation <- unlist(strsplit(pa_page_content[2], " "))
  
  page_creation_day <- page_creation[2]
  page_creation_month <- page_creation[3]
  page_creation_year <- page_creation[4]
  
  # Set locale to work with dates
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
  
  page_creation <- paste(page_creation_day, page_creation_month, page_creation_year)
  page_creation <- as.Date(page_creation, tryFormats = c("%d %B %Y"))
  page_creation <- as.character(page_creation)

  
  # Add information to the dataset
  new_row <- data.table(codcnuc = pa_cod_cnuc, pa_name = pa_name, pa_start_views = pa_start_views, pa_end_views = pa_end_views, page_creation = page_creation)
  
  pa_dataset_eng <- rbind(pa_dataset_eng, new_row)
  
}



# ---- PT PAGE CREATION: Read all CSV ENG names and verify if exists a PT version page ----
# Then, get info about page creation of the PT version from Wikipedia
for (i in 1:length(pa_info_eng$codcnuc)) {
  print(paste(i, "in", length(pa_info_eng$codcnuc)))
  
  pa_cod_cnuc <- pa_info_eng$codcnuc[i]
  pa_start_views <- pa_info_eng$pa_start_views[i]
  pa_end_views <- pa_info_eng$pa_end_views[i]
  
  # Get PA Eng Name from PA Original Dataset
  pa_name <- pa_original %>%
    filter(pa_original$codCnuc == pa_cod_cnuc) %>%
    select(ptNameWiki)
  
  if (!is.na(pa_name[1,1])) {
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
    
    # Split character variable and standardize date
    page_creation <- unlist(strsplit(pa_page_content[2], " de "))
    
    page_creation_day <- page_creation[2]
    page_creation_month <- page_creation[3]
    page_creation_year <- page_creation[4]
    
    # Set locale to work with dates
    Sys.setlocale("LC_TIME", "pt_BR.UTF-8")
    
    page_creation <- paste(page_creation_day, page_creation_month, page_creation_year)
    page_creation <- as.Date(page_creation, tryFormats = c("%d %B %Y"))
    page_creation <- as.character(page_creation)
    
    
    # Add information to the dataset
    new_row <- data.table(codcnuc = pa_cod_cnuc, pa_name = pa_name, pa_start_views = pa_start_views, pa_end_views = pa_end_views, page_creation = page_creation)
    
    pa_dataset_pt <- rbind(pa_dataset_pt, new_row)

  }
}

# ---- PT PAGE CREATION: Read all CSV PT names and get page creation from Wikipedia ----
for (i in 1:length(pa_info_pt$codcnuc)) {
  print(paste(i, "in", length(pa_info_pt$codcnuc)))
  
  pa_cod_cnuc <- pa_info_pt$codcnuc[i]
  pa_start_views <- pa_info_pt$pa_start_views[i]
  pa_end_views <- pa_info_pt$pa_end_views[i]
  
  # Get PA Eng Name from PA Original Dataset
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
  
  # Split character variable and standardize date
  page_creation <- unlist(strsplit(pa_page_content[2], " de "))
  
  page_creation_day <- page_creation[2]
  page_creation_month <- page_creation[3]
  page_creation_year <- page_creation[4]
  
  # Set locale to work with dates
  Sys.setlocale("LC_TIME", "pt_BR.UTF-8")
  
  page_creation <- paste(page_creation_day, page_creation_month, page_creation_year)
  page_creation <- as.Date(page_creation, tryFormats = c("%d %B %Y"))
  page_creation <- as.character(page_creation)
  
  
  # Add information to the dataset
  new_row <- data.table(codcnuc = pa_cod_cnuc, pa_name = pa_name, pa_start_views = pa_start_views, pa_end_views = pa_end_views, page_creation = page_creation)
  
  pa_dataset_pt <- rbind(pa_dataset_pt, new_row)
  
}



# Save PA Dataset
today <- Sys.Date()

write_csv(pa_dataset_eng, paste0("./data/BPA_WikipediaPages_eng_", today, ".csv"))
write_csv(pa_dataset_pt, paste0("./data/BPA_WikipediaPages_pt_", today, ".csv"))


