###############################################
## Tidy Dataset
## Verify difference between Language Lists
## Author: Guedes-Santos, J
###############################################

# Load packages
library(tidyverse)

# Define dataset
pa_names <- read_csv("./data/BrazilianProtectedAreas_2018-12-10_reviewed.csv")

# View dataset
str(pa_names)
summary(pa_names)

# Verify pages by Name: in ptNameWiki without page in enNameWiki
pt_names <- pa_names %>%
  subset(is.na(enNameWiki)) %>%
  subset(!is.na(ptNameWiki))

# Verify pages by Name: in enNameWiki without page in ptnNameWiki
en_names <- pa_names %>%
  subset(is.na(ptNameWiki)) %>%
  subset(!is.na(enNameWiki))

# Verify pages by ID WikiData: in idPtWikiData with different id in enNameWiki
dif_id <- pa_names %>%
  subset(idPtWikiData != idEnWikiData)
