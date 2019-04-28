

rm(list = ls())

library(tidyverse)

pa_dir <- "./data/eng/"

length(list.files(pa_dir))

pa_files <- list.files(pa_dir)

wiki_languages <- vector("character", length = 0)

# Read all CSVs and get languages
for (i in 1:length(list.files(pa_dir))) {
  pa_data <- read_csv(paste0(pa_dir, pa_files[i]))
  
  # Verify if has any different language in CSV 
  if (length(pa_data$Language[!pa_data$Language %in% wiki_languages]) > 0) {
    # Add every new language in CSV to var 'wiki_languages' 
    for (i in 1:length(pa_data$Language[!pa_data$Language %in% wiki_languages])) {
      wiki_languages[length(wiki_languages) + 1] <- pa_data$Language[!pa_data$Language %in% wiki_languages]
    }
  }
  
}
