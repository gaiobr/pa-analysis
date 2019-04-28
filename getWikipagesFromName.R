## Reference: https://data.metinyazici.org/2017/10/working-with-web-data-in-r.html

###########
# 1. Comparar Id Wikidata En e Pt para verificar se são iguais
# 2. Se não for, destacar em uma nova tabela para verificação manual

# Clean R Memory
rm(list = ls())

# Load required packages
library(tidyverse)
library(httr)
library(jsonlite)
library(readODS)
library(WikipediR)
library(WikidataR)

# This configuration is necessary because the following error:
## Error in curl::curl_fetch_memory(url, handle = handle) : 
##    Error in the HTTP2 framing layer
### Possible implementation to solve this error temporarily: https://groups.google.com/forum/#!topic/manipulatr/-C-z8l1MvTE
httr::set_config(config(http_version = 2)) # set the HTTP version to 1.1 (none, 1.0, 1.1, 2)

CNUC <- read_csv("./data/BPA_CNUC_2018-11-06.csv")

CNUC_DB <- data.frame("codCnuc" = NA,	"nomeUC" = NA,	"duplicate" = NA,	"cat" = NA,	"esfera" = NA, "idEnWikiData" = NA,	"enNameWiki" = NA,	"enWikiPage" = NA, "idPtWikiData" = NA,	"ptNameWiki" = NA,	"ptWikiPage" = NA,	"lastVerification" = NA)

# Create a idWikiData column with NA values
CNUC_DB$codCnuc <- NA
CNUC_DB$nomeUC <- NA
CNUC_DB$idPtWikiData <- NA
CNUC_DB$idEnWikiData <- NA

# Portuguese pages
for (i in 1:nrow(CNUC)) {
  
  pageCNUC <- CNUC$nomeUC[i]
  CNUC_DB[i,1] <- CNUC$codCnuc[i]
  CNUC_DB[i,2] <- CNUC$nomeUC[i]
  CNUC_DB[i,4] <- CNUC$`Categoria de Manejo`[i]
  CNUC_DB[i,5] <- CNUC$`Esfera Administrativa`[i]
  
  # Define URL to get info
  url <- modify_url(
    "https://pt.wikipedia.org/w/api.php?",
    query = list(
      action = "query",
      format = "json",
      list = "search",
      srsort = "just_match",
      srsearch = pageCNUC
    )
  )
  
  # Load URL info
  getWikipages <- GET(url)
  
  ## http_type(getWikipages) # Returns content type
  ## writeLines(content(get_http, as = "text")) # Show content
  ## content(get_http, as = "text") # Another way to show content
  jsonWikipages <- fromJSON(content(getWikipages, as = "text"))
  
  # Show first page found
  print("------------------------------------------------------------")
  print(paste('Item:', i))
  print(paste('codCNUC:', CNUC$codCnuc[i]))
  print(paste('Página CNUC: ', pageCNUC))
  if (jsonWikipages$query$searchinfo$totalhits > 0) {
    ptIdWiki <- jsonWikipages$query$search$pageid[1]
    ptNameWiki <- jsonWikipages$query$search$title[1]
    ptWikidata <- find_item(ptNameWiki)
    
    ##  if (!is.null(ptNameWiki)) {
    if (length(ptWikidata) > 0) {    
      ptWikipedia <- page_info("pt", "wikipedia", page = ptNameWiki)
      
      idPtWikidata <- ptWikidata[[1]]$id
      
      CNUC_DB$ptNameWiki[i] <- ptNameWiki
      CNUC_DB$ptWikiPage[i] <- ptWikipedia$query$pages[[1]]$fullurl
      CNUC_DB$lastVerification[i] <- Sys.Date()
      CNUC_DB$idPtWikiData[i] <- idPtWikidata
      
      print(paste('ID1: ', idPtWikidata))
    }
  }
  
}

## English Pages
for (i in 1:nrow(CNUC)) {
  
  pageCNUC <- CNUC$nomeUC[i]
  CNUC_DB[i,1] <- CNUC$codCnuc[i]
  CNUC_DB[i,2] <- CNUC$nomeUC[i]
  CNUC_DB[i,4] <- CNUC$`Categoria de Manejo`[i]
  CNUC_DB[i,5] <- CNUC$`Esfera Administrativa`[i]
  
  # Define URL to get info
  url <- modify_url(
    "https://en.wikipedia.org/w/api.php?",
    query = list(
      action = "query",
      format = "json",
      list = "search",
      srsort = "just_match",
      srsearch = pageCNUC
    )
  )
  
  # Load URL info
  getWikipages <- GET(url)
  
  ## http_type(getWikipages) # Returns content type
  ## writeLines(content(get_http, as = "text")) # Show content
  ## content(get_http, as = "text") # Another way to show content
  jsonWikipages <- fromJSON(content(getWikipages, as = "text"))
  
  # Show first page found
  print("------------------------------------------------------------")
  print(paste('Item:', i))
  print(paste('codCNUC:', CNUC$codCnuc[i]))
  print(paste('Página CNUC: ', pageCNUC))
  if (jsonWikipages$query$searchinfo$totalhits > 0) {
    enIdWiki <- jsonWikipages$query$search$pageid[1]
    enNameWiki <- jsonWikipages$query$search$title[1]
    enWikidata <- find_item(enNameWiki)
    
    ##  if (!is.null(ptNameWiki)) {
    if (length(enWikidata) > 0) {    
      enWikipedia <- page_info("en", "wikipedia", page = enNameWiki)
      
      idEnWikidata <- enWikidata[[1]]$id
      
      CNUC_DB$enNameWiki[i] <- enNameWiki
      CNUC_DB$enWikiPage[i] <- enWikipedia$query$pages[[1]]$fullurl
      CNUC_DB$lastVerification[i] <- Sys.Date()
      CNUC_DB$idEnWikiData[i] <- idEnWikidata
      
      print(paste('ID1: ', idEnWikidata))
      print(paste('Página Eng: ', enNameWiki))
    }
  }
  
}


## Gravar no arquivo CSV
today <- Sys.Date()

write_csv(CNUC_DB, paste0("./data/BrazilianProtectedAreas_", today, ".csv"))
