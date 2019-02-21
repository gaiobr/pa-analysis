# Getting Wikipedia Langviews Data

# Clean R memory
rm(list=ls())


# We use RSelenium to connect with a Docker server
# We need to have a Docker installed and started in the machine
# It is necessary to automate browsing process and to download the data
library(RSelenium)
# library(wdman)

# ---- Brazilian Protected Areas Database ----
### This Database is based in the ICMBio Database added with some Wikipedia information
PA_file <- "./data/BrazilianProtectedAreas_2018-12-10_reviewed.csv"

PA_DB <- read.csv(PA_file)

PA_DB$enWikiPageDown <- NA # Column to verify if a page was verified and download was succeeded


# ---- Period of pageviews we want to get ----
views_start <- "2015-07-01"
#views_end <- "2018-07-31"
views_end <- as.character(Sys.Date()-1) # Yesterday - Because Wikipedia only releases a closed day to download

# ---- Configuring RSelenium ----
#### Very important to define download directory before running the script
# This variable will pass additional settings to Chrome browser (we can choose another browser, but we need to see how to configure it)
# Mint Caetes
############################### 
# Estou tendo algum problema em configurar um diretório que não seja em /home/gaio/test - verificar se não é questão de permissão de escrita
###############################
#PA_directory <- paste("/home/seluser/Data/Raw_Data/Wikipedia/",PA_cat,"/", sep="")
#eCaps <- list(chromeOptions = list(prefs = list(
#  "download.default_directory" = PA_directory
#)))
eCaps <- list(chromeOptions = list(prefs = list(
  "download.default_directory" = "/home/seluser/Downloads"
)))

# Here we connect with the Docker
remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "firefox", extraCapabilities = eCaps)
remDr$open()

# Running loop to extract ENG data
for(i in 1:nrow(PA_DB)){
#for(i in 1:5) { #only for test
  # This first line defines the link
  #### Very important to define view dates
  # If statement to control errors from non-existent pages
  if (is.na(PA_DB[i,7]) == FALSE) {
    print(paste(PA_DB[i,7]," - ",PA_DB[i,2]))
    
    enWikiPage <- gsub("^.*?wiki/","", PA_DB$enWikiPage[i])
    
    codCnuc <- PA_DB$codCnuc
    url<-paste("https://tools.wmflabs.org/langviews/?project=en.wikipedia.org&platform=all-access&agent=user&start=",views_start,"&end=",views_end,"&sort=views&direction=1&view=list&page=", sep="")
    url2<-paste(url,enWikiPage,sep="")
    
    # Automatized data download - adjust Sys.sleep values for speed
    remDr$navigate(url2)
    Sys.sleep(20) #20 early
    webElem <- remDr$findElement("class", "download-btn-group")
    webElem$clickElement()
    Sys.sleep(2)
    webElem <- remDr$findElement("class", "download-csv")
    webElem$clickElement()
    Sys.sleep(10) #10 early
    
    # Assigns a name to the PA_Name variable that will be used to rename the CSV file 
    if (is.na(PA_DB$enNameWiki[i]) == FALSE) {
      PA_Name <- PA_DB$enNameWiki[i]
    } 

    #Automatically renames files according to PA_DB line number - adjust accordingly if necessary
    #file.rename(paste("/home/seluser/Data/Raw_Data/Wikipedia/",PA_cat,"/","langviews-20150701-20180731.csv",sep=""),paste("/home/seluser/Data/Raw_Data/Wikipedia/",PA_cat,"/",i,"_",PA_DB$enNameWiki[i],"_20150701_20180731.csv",sep=""))
    # Define standard filename from Wikipedia download
    filename <- paste("/home/gaio/test/","langviews-",gsub("-",'',views_start),"-",gsub("-",'',views_end),".csv",sep="")
    # Rename Wikipedia filename to a custom filename
    if (file.exists(filename)) {
      newFilename <- paste("/home/gaio/test/",codCnuc[i],"_",gsub("/","-",PA_DB$enNameWiki[i]),"_",views_start,"_",views_end,".csv",sep="")
      file.rename(filename, newFilename)
      print(paste("Arquivo",newFilename,"baixado com sucesso!"))
      PA_DB$enWikiPageDown[i] <- TRUE
    }  else {
      print(paste("Não foi possível baixar o arquivo referente à página",PA_DB$enNameWiki[i]))
      PA_DB$enWikiPageDown[i] <- FALSE
    }
    print(paste(i," out of ",nrow(PA_DB),sep=""))
  } 
}



rD[["server"]]$stop()
