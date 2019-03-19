# ---- Head ------------------- 
# Explore mean pageviews data
# Created in 2019-03-05
#
# Guedes-Santos, J
#
#______________________________

# Clean memory
rm(list = ls())


# Load packages
library(tidyverse)

# Read CSV files
pa_dataset <- read_csv('./data/BrazilianProtectedAreas_2019-03-07_reviewed.csv')
eng_means <- read_csv('./data/BPA_Wiki_Eng_2019-03-10.csv')
eng_month_means <- read_csv('./data/BPA_Wiki_Eng_Month_2019-03-10.csv')
pt_means <- read_csv('./data/BPA_Wiki_Pt_2019-03-10.csv')
pt_month_means <- read_csv('./data/BPA_Wiki_Pt_Month_2019-03-10.csv')

# Define functions
# Return Brazilian Protected Area Original Name
pa_name <- function(dataset, cod_cnuc) {
  # This function is based in PA_Dataset: BrazilianProtectedAreas_[date]>_reviewed.csv
  name <- dataset %>%
    filter(codCnuc == cod_cnuc) %>%
    select(nomeUC)
  return(name$nomeUC)
}

# Subset datas
# Define an initial PA Code - Only for tests
#cod_cnuc <- "0000.00.0001"

# Join eng and pt means in a unique dataset
pa_means <- eng_means %>%
  inner_join(pt_means, eng_means, by = "cod_cnuc", suffix = c(".eng", ".pt"))

# Join eng and pt month means in a unique dataset
pa_month_means <- eng_month_means %>%
  select(one_of(colnames(pt_month_means))) %>%
  inner_join(pt_month_means, by = c("month"), suffix = c(".eng", ".pt")) 

# Rename col names because an error with dots in col names
for (i in 2:((ncol(pa_month_means)))) {
  names(pa_month_means)[i] <- paste0("cnuc_", gsub('[.]', '_', names(pa_month_means)[i]))
  print(paste(i, names(pa_month_means)[i]))
}

colnames(pa_month_means)

# --------------------------------
# Get category and government level
pa_governance <- pa_dataset %>%
  select(codCnuc, cat, esfera)

# Join category and government level
pa_means <- pa_means %>%
  inner_join(pa_governance, by = c("cod_cnuc" = "codCnuc"))


for (i in 2:((ncol(pa_month_means)-1)/2)) {
  cod_cnuc <- gsub("_", ".", substr(names(pa_month_means)[i], 6, 17))
  pt_colname <- gsub("_eng", "_pt", names(pa_month_means)[i])
  
  print(paste(i, cod_cnuc, names(pa_month_means)[i]))
  #print(pa_name(pa_dataset, cod_cnuc))

  # Plot a simple graph compare eng and pt pageviews
  ggplot(data = pa_month_means) +
    geom_point(mapping = aes_string(x = "month", y = names(pa_month_means)[i]), color = "red") +
    geom_point(mapping = aes_string(x = "month", y = pt_colname), color = "blue") +
    geom_line(mapping = aes_string(x = "month", y = names(pa_month_means)[i]), color = "red") +
    geom_line(mapping = aes_string(x = "month", y = pt_colname), color = "blue") +
    xlab('Time') +
    ylab("Monthly averages of page views") +
    ggtitle(pa_name(pa_dataset, cod_cnuc))
  
  ggsave(paste0("./figures/", cod_cnuc, ".png"))
}


# Plot all PAs - not working yet
pa_month_Melted <- reshape2::melt(pa_month_means, id.var='month')
head(pa_month_Melted)

ggplot(pa_month_Melted, aes(x = month, y = value, col = variable)) + 
  geom_line()

fahrenheit_to_kelvin <- function(temp_F) {
  temp_K <- ((temp_F - 32) * (5 / 9)) + 273.15
  return(temp_K)
}

# Get 10 most visited PAs in eng
pa_means %>%
  top_n(10, mean.eng) %>%
  arrange(desc(mean.eng))

# Get 10 most visited PAs in pt
pa_means %>%
  top_n(10, mean.pt) %>%
  arrange(desc(mean.pt))
