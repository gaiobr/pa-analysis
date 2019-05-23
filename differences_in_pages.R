### Get differences about Wikipedia Pages
### - Pages with different Wikidata Id in each language (Pt and Eng)
### - Pages with Wikidata Id only in Portuguese
### - Pages with Wikidata Id only in English

rm(list = ls())

# Import packages
library(tidyverse)

# Import Protected Areas Dataset with Wikipedia Pages: Names, IDs Wikidata and URLs
PA_DB <- read.csv("./data/BrazilianProtectedAreas_2019-05-13.csv")

# Display Structure of the Dataset
str(PA_DB)

# List column names
colnames(PA_DB)

# Get Pages that have different Wikidata IDs between them
diff_wiki_pages_ids <- PA_DB %>%
  subset(as.character(idPtWikiData) != as.character(idEnWikiData))

# Get PT Pages that do not exist in English Version
pt_wiki_pages_only <- PA_DB %>%
  filter(!is.na(idPtWikiData)) %>%
  filter (is.na(idEnWikiData))

# Get Eng Pages that do not exist in Portuguese Version
eng_wiki_pages_only <- PA_DB %>%
  filter(!is.na(idEnWikiData)) %>%
  filter (is.na(idPtWikiData))
