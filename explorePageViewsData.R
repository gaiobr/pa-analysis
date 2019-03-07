# ---- Head ----$$$$$$$$$$$$$$$
#
# Explore pageviews data
# Created in 2019-03-05
#
# Guedes-Santos, J
#
# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Clean environment
rm(list = ls())

# Load packages
library(tidyverse)
library(data.table)
library(lubridate)

# Load datasets
cnuc_data <- read_csv("./data/BrazilianProtectedAreas_2019-03-07_reviewed.csv")
page_creation_data_eng <- read_csv("./data/BPA_WikipediaPages_eng_2019-03-07.csv")

# Create a Mean Eng dataset
mean_eng_pageviews <- data.table(
  cod_cnuc = character(),
  mean = numeric(),
  na_rows = integer(),
  total_days = integer()
)

# Load list of CSV files
files <- list.files("./data/eng/")

# ---- MEAN ENG PAGEVIEWS: Calculate mean values for all ENG pageviews ----
for (i in 1:length(files)) {
  pa_pageviews <- read_csv(paste0("./data/eng/",files[i]), col_types = cols()) # col_types = cols() supress output information when reading CSV

  # Extract CNUC code from CSV filename
  cod_cnuc_pageview <- substr(files[i], 0, 12)
  
  # Extract page creation 
  page_creation <- page_creation_data_eng %>%
    filter(codcnuc == cod_cnuc_pageview) %>%
    select(page_creation)
  
  # Rearrange CSV Pageviews Dataset
  pa_pageviews <- pa_pageviews %>%
    filter(Language == "en") %>%
    select(4:ncol(pa_pageviews)) %>%
    t()
  
  pa_pageviews <- as.data.frame(pa_pageviews)
  
  pa_pageviews <- cbind(rownames(pa_pageviews), pa_pageviews)
  colnames(pa_pageviews) <- c("Dates", "Views")

  print(cod_cnuc_pageview)

  # Define start row based in the date of page creation (after 2015-07-01, row will be greater than 1)
  if (page_creation$page_creation >= as.Date("2015-07-01")) {
    row_num <- which(as.Date(pa_pageviews$Dates) == page_creation$page_creation)
  } else {
    row_num <- which(as.Date(pa_pageviews$Dates) == as.Date("2015-07-01"))
  }
  
  print(nrow(pa_pageviews))

  # Select a subset when necessary
  pa_pageviews <- pa_pageviews %>%
    slice(row_num:nrow(pa_pageviews))

  # Calculate mean
  pa_mean <- mean(as.integer(pa_pageviews$Views), na.rm = TRUE)
  print(pa_mean)
  
  # Count how many rows contains data filled with NA
  na_rows <- pa_pageviews %>%
    filter(is.na(Views)) %>%
    count()

  na_rows <- as.integer(na_rows)
  
  # Add information to the dataset
  new_row <- data.table(cod_cnuc = cod_cnuc_pageview, mean = pa_mean, na_rows = na_rows, total_days = nrow(pa_pageviews))
  
  mean_eng_pageviews <- rbind(mean_eng_pageviews, new_row)  
}


# ---- MEAN MONTH ENG PAGEVIEWS: Calculate mean values by month to ENG pageviews ----
# Create a primary table with a row for each month starting from 2015-07
month_mean_eng_pageviews <- data.table(
  month = seq(
    as.Date("2015-07-01"), 
    as.Date("2019-02-01"), 
    by = "month"
    )
)


for (i in 1:length(files)) {
  # Read CSV
  pa_pageviews <- read_csv(paste0("./data/eng/",files[i]), col_types = cols()) # col_types = cols() supress output information when reading CSV
  
  # Extract CNUC code from CSV filename
  cod_cnuc_pageview <- substr(files[i], 0, 12)
  
  # Extract page creation 
  page_creation <- page_creation_data_eng %>%
    filter(codcnuc == cod_cnuc_pageview) %>%
    select(page_creation)
  
  
  # Transform data
  # Rearrange CSV Pageviews Dataset
  pa_pageviews <- pa_pageviews %>%
    filter(Language == "en") %>%
    select(4:ncol(pa_pageviews)) %>%
    t()
  
  pa_pageviews <- as.data.frame(pa_pageviews)
  
  pa_pageviews <- cbind(rownames(pa_pageviews), pa_pageviews)
  colnames(pa_pageviews) <- c("Dates", "Views")
  
  # Define start row based in the date of page creation (after 2015-07-01, row will be greater than 1)
  if (page_creation$page_creation >= as.Date("2015-07-01")) {
    row_num <- which(as.Date(pa_pageviews$Dates) == page_creation$page_creation)
    print("Depois#############################")
    print(cod_cnuc_pageview)
    print(row_num)
    print(page_creation$page_creation)
  } else {
    row_num <- which(as.Date(pa_pageviews$Dates) == as.Date("2015-07-01"))
    print("Antes$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
    print(cod_cnuc_pageview)
    print(row_num)
    print(page_creation$page_creation)
  }
  
  # Select a subset when necessary
  pa_pageviews <- pa_pageviews %>%
    slice(row_num:nrow(pa_pageviews))
  
  # Filter by month and calculate means
  month_eng_means <- pa_pageviews %>%
    arrange(as.Date(Dates)) %>%
    mutate(Month_Year = substr(Dates, 1,7)) %>%
    group_by(Month_Year) %>%
    summarise(mean(as.numeric(Views, na.rm = TRUE))) 
  
  colnames(month_eng_means) <- c("month", cod_cnuc_pageview)
  month_eng_means$month <- as.Date(paste0(month_eng_means$month, "-01"))
  
  # Populate data table
  month_mean_eng_pageviews <- merge(x = month_mean_eng_pageviews, y = month_eng_means, by = "month", all = TRUE)
  
}



# ---- MEAN PT PAGEVIEWS: Calculate mean values for all PT pageviews ----
files_pt <- list.files("./data/pt/")
page_creation_data_pt <- read_csv("./data/BPA_WikipediaPages_pt_2019-03-07.csv")

# Create a Mean Pt dataset
mean_pt_pageviews <- data.table(
  cod_cnuc = character(),
  mean = numeric(),
  na_rows = integer(),
  total_days = integer()
)

for (i in 1:length(files)) {
  pa_pageviews <- read_csv(paste0("./data/eng/",files[i]), col_types = cols()) # col_types = cols() supress output information when reading CSV
  
  # Extract CNUC code from CSV filename
  cod_cnuc_pageview <- substr(files[i], 0, 12)
  
  # Extract page creation 
  page_creation <- page_creation_data_pt %>%
    filter(codcnuc == cod_cnuc_pageview) %>%
    select(page_creation)
  
  # Test if pt language exists in CSV Eng File
  pt_verify <- pa_pageviews %>%
    subset(pa_pageviews$Language == "pt")
  
  if (length(pt_verify$Language) > 0) {
    # Rearrange CSV Pageviews Dataset
    pa_pageviews <- pa_pageviews %>%
      filter(Language == "pt") %>%
      select(4:ncol(pa_pageviews)) %>%
      t()
    
    pa_pageviews <- as.data.frame(pa_pageviews)
    
    pa_pageviews <- cbind(rownames(pa_pageviews), pa_pageviews)
    colnames(pa_pageviews) <- c("Dates", "Views")
    
    print(cod_cnuc_pageview)

    # Define start row based in the date of page creation (after 2015-07-01, row will be greater than 1)
    if (page_creation$page_creation >= as.Date("2015-07-01")) {
      row_num <- which(as.Date(pa_pageviews$Dates) == page_creation$page_creation)
    } else {
      row_num <- which(as.Date(pa_pageviews$Dates) == as.Date("2015-07-01"))
    }
    
    print(nrow(pa_pageviews))
    
    # Select a subset when necessary
    pa_pageviews <- pa_pageviews %>%
      slice(row_num:nrow(pa_pageviews))
    
    # Calculate mean
    pa_mean <- mean(as.integer(pa_pageviews$Views), na.rm = TRUE)
    print(pa_mean)
    
    # Count how many rows contains data filled with NA
    na_rows <- pa_pageviews %>%
      filter(is.na(Views)) %>%
      count()
    
    na_rows <- as.integer(na_rows)
    
    # Add information to the dataset
    new_row <- data.table(cod_cnuc = cod_cnuc_pageview, mean = pa_mean, na_rows = na_rows, total_days = nrow(pa_pageviews))
    
    mean_pt_pageviews <- rbind(mean_pt_pageviews, new_row)  
  }
  

  
}


# ---- MEAN MONTH PT PAGEVIEWS: Calculate mean values by month to ENG pageviews ----
# Create a primary table with a row for each month starting from 2015-07
month_mean_pt_pageviews <- data.table(
  month = seq(
    as.Date("2015-07-01"), 
    as.Date("2019-02-01"), 
    by = "month"
  )
)


for (i in 1:length(files)) {
  # Read CSV
  pa_pageviews <- read_csv(paste0("./data/eng/",files[i]), col_types = cols()) # col_types = cols() supress output information when reading CSV
  
  # Extract CNUC code from CSV filename
  cod_cnuc_pageview <- substr(files[i], 0, 12)
  
  # Extract page creation 
  page_creation <- page_creation_data_pt %>%
    filter(codcnuc == cod_cnuc_pageview) %>%
    select(page_creation)
  
  # Test if pt language exists in CSV Eng File
  pt_verify <- pa_pageviews %>%
    subset(pa_pageviews$Language == "pt")
  
  if (length(pt_verify$Language) > 0) {
   
     # Transform data
    # Rearrange CSV Pageviews Dataset
    pa_pageviews <- pa_pageviews %>%
      filter(Language == "pt") %>%
      select(4:ncol(pa_pageviews)) %>%
      t()
    
    pa_pageviews <- as.data.frame(pa_pageviews)
    
    pa_pageviews <- cbind(rownames(pa_pageviews), pa_pageviews)
    colnames(pa_pageviews) <- c("Dates", "Views")
    
    # Define start row based in the date of page creation (after 2015-07-01, row will be greater than 1)
    if (page_creation$page_creation >= as.Date("2015-07-01")) {
      row_num <- which(as.Date(pa_pageviews$Dates) == page_creation$page_creation)
      print("Depois#############################")
      print(cod_cnuc_pageview)
      print(row_num)
      print(page_creation$page_creation)
    } else {
      row_num <- which(as.Date(pa_pageviews$Dates) == as.Date("2015-07-01"))
      print("Antes$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
      print(cod_cnuc_pageview)
      print(row_num)
      print(page_creation$page_creation)
    }
    
    # Select a subset when necessary
    pa_pageviews <- pa_pageviews %>%
      slice(row_num:nrow(pa_pageviews))
    
    # Filter by month and calculate means
    month_pt_means <- pa_pageviews %>%
      arrange(as.Date(Dates)) %>%
      mutate(Month_Year = substr(Dates, 1,7)) %>%
      group_by(Month_Year) %>%
      summarise(mean(as.numeric(Views, na.rm = TRUE))) 
    
    colnames(month_pt_means) <- c("month", cod_cnuc_pageview)
    month_pt_means$month <- as.Date(paste0(month_pt_means$month, "-01"))
    
    # Populate data table
    month_mean_pt_pageviews <- merge(x = month_mean_pt_pageviews, y = month_pt_means, by = "month", all = TRUE)
    
  }
  
  
}

# ---- IMPORTANT TO VERIFY ----
# Some PAs that have PT data, but is not listed in the CNUC Dataset
# It is important to make a comparison between CNUC Name and WikiNAme (engs and portuguese)
# For example: Floresta Nacional de Brasília (0000.00.0086) está vinculada a uma PA que não é a correta
for (i in 1:length(files)) {
  pa_pageviews <- read_csv(paste0("./data/eng/",files[i]), col_types = cols()) # col_types = cols() supress output information when reading CSV
  
  # Extract CNUC code from CSV filename
  cod_cnuc_pageview <- substr(files[i], 0, 12)
  
  # Extract page creation 
  page_creation <- page_creation_data_pt %>%
    filter(codcnuc == cod_cnuc_pageview) %>%
    select(page_creation)
  
  print(cod_cnuc_pageview)
  print(page_creation$page_creation)
  
  # Test if pt language exists in CSV Eng File
  pt_verify <- pa_pageviews %>%
    subset(pa_pageviews$Language == "pt")
  
  if (length(pt_verify$Language) > 0) {
    verify_pgcreation <- page_creation_data_pt %>%
      subset(page_creation_data_pt$codcnuc == cod_cnuc_pageview)
    
    if (length(verify_pgcreation$page_creation) < 1) {
      print(cod_cnuc_pageview)
    }
    
  }

}

