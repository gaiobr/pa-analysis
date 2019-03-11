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

# Subset datas

# Define an initial PA Code
cod_cnuc <- "0000.00.0001"

# Return Brazilian Protected Area Original Name
pa_dataset %>%
  filter(codCnuc == cod_cnuc) %>%
  select(nomeUC)

# Join eng and pt means in a unique dataset
pa_means <- eng_means %>%
  inner_join(eng_means, pt_means, by = "cod_cnuc", suffix = c(".eng", ".pt"))

# Join eng and pt month means in a unique dataset
pa_month_means <- eng_month_means %>%
  inner_join(pt_month_means, by = c("month"), suffix = c(".eng", ".pt"))


# Get category and government level
pa_governance <- pa_dataset %>%
  select(codCnuc, cat, esfera)

# Join category and government level
pa_means <- pa_means %>%
  inner_join(pa_governance, by = c("cod_cnuc" = "codCnuc"))


# Plot a simple graph compare eng and pt pageviews
ggplot(data = pa_month_means) +
  geom_point(mapping = aes(x = month, y = `0000.00.0001.pt`), color = "red") +
  geom_point(mapping = aes(x = month, y = `0000.00.0001.eng`), color = "blue") +
  geom_line(mapping = aes(x = month, y = `0000.00.0001.pt`), color = "red") +
  geom_line(mapping = aes(x = month, y = `0000.00.0001.eng`), color = "blue") +
  xlab('Time') +
  ylab("Monthly averages of page views") +
  ggtitle(pa_dataset$nomeUC)

# Plot all PAs - not working yet
pa_month_Melted <- reshape2::melt(pa_month_means, id.var='month')
head(pa_month_Melted)

ggplot(pa_month_Melted, aes(x = month, y = value, col = variable)) + 
  geom_line()

