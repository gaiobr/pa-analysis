#####################################
#
# Explore pageviews data
# Created in 2019-03-05
#
# Guedes-Santos, J
#
#####################################

# Clean environment
rm(list = ls())

# Load packages
library(tidyverse)
library(data.table)

# Load datasets
cnuc_data <- read_csv("./data/BrazilianProtectedAreas_2018-12-10_reviewed.csv")
page_creation_data_eng <- read_csv("./data/BPA_WikipediaPages_eng_2019-03-05.csv")

# Create a Mean dataset
mean_pageviews <- data.table(
  cod_cnuc = character(),
  mean = numeric(),
  na_rows = integer(),
  total_days = integer()
)

# Load list of CSV files
files <- list.files("./data/eng/")

# Calculate mean values to pageviews
for (i in 1:length(files)) {
  pa_pageviews <- read_csv(paste0("./data/eng/",files[i]))

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

  print("Linhas")
  print(nrow(pa_pageviews))
    
  str(pa_pageviews)
  summary(pa_pageviews)
  pa_mean <- mean(as.integer(pa_pageviews$Views), na.rm = TRUE)
  print(pa_mean)
  
  # Count how many rows contains data filled with NA
  na_rows <- pa_pageviews %>%
    filter(is.na(Views)) %>%
    count()

  na_rows <- as.integer(na_rows)
  
  # Add information to the dataset
  new_row <- data.table(cod_cnuc = cod_cnuc_pageview, mean = pa_mean, na_rows = na_rows, total_days = nrow(pa_pageviews))
  
  mean_pageviews <- rbind(mean_pageviews, new_row)  
}

