###############################################
## Tidy Dataset
## Verify difference between Language Lists
## Author: Guedes-Santos, J
###############################################

# Load packages
library(tidyverse)

# Clean R memory
rm(list=ls())

# Define dataset
pa_names <- read_csv("./data/BrazilianProtectedAreas_2019-03-07_reviewed.csv")

# View dataset
head(pa_names)

# Verify pages by Name: in ptNameWiki without page in enNameWiki
pt_names <- pa_names %>%
  subset(is.na(enNameWiki)) %>%
  subset(!is.na(ptNameWiki))

# Verify pages by Name: in enNameWiki without page in ptNameWiki
en_names <- pa_names %>%
  subset(is.na(ptNameWiki)) %>%
  subset(!is.na(enNameWiki))

# Verify pages by ID WikiData: in idPtWikiData with different id in enNameWiki
dif_id <- pa_names %>%
  subset(idPtWikiData != idEnWikiData)
