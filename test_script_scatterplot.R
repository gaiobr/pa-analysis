# ---- Head ------------------- 
# Project: Protected Areas Social Interest
#
# Plot scatterplot and boxplot for means
#
# Created in 2019-05-30
#
# Guedes-Santos, J
#
#______________________________

# Clean environment
rm(list = ls())

# ---- Import functions -----
source("./functions.R")


# ---- Load libraries ----
library(tidyverse)
library(ggplot2)
library(scales)
library(reshape2)
library(ggExtra)
library(ggpubr)


# ---- Load datasets ----------
#PA Dataset (CNUC)
pa_dataset <- read_csv("./data/BrazilianProtectedAreas_2019-07-14.csv")
#PA data from Correia et al paper (Ecological Indicators - Correia, R.A. et al. 2019)
#pa_correia <- read.table("/media/gaio/Argos/Dropbox/Pesquisa/Doutorado/Qualificação II/Análises Ricardo/PA_data_030416.csv",sep=";",dec=".", header=T, quote = "\"")
#pa_correia <- read.table("/home/gaio/Dropbox/Pesquisa/Doutorado/Qualificação II/Análises Ricardo/PA_data_030416.csv",sep=";",dec=".", header=T, quote = "\"")
#pa_correia <- read.table("Z:/Dropbox/Pesquisa/Doutorado/Qualificação II/Análises Ricardo/PA_data_030416.csv",sep=";",dec=".", header=T, quote = "\"")
pa_correia <-read.table("F:/GAIO/data/ricardo/EI/PA_data_030416.csv",sep=";",dec=".", header=T, quote = "\"")
#PA Dataset Merge from CNUC PA Dataset and Correia PA Dataset 
eng_means <- read_csv('./data/BPA_Wiki_Eng_2019-06-25.csv')
eng_month_means <- read_csv('./data/BPA_Wiki_Eng_Month_2019-06-21.csv')
pt_means <- read_csv('./data/BPA_Wiki_Pt_2019-06-25.csv')
pt_month_means <- read_csv('./data/BPA_Wiki_Pt_Month_2019-06-21.csv')

# ---- Transform datasets ----
pa_merge <- inner_join(pa_correia,
                       pa_dataset,
                       by = c("id" = "codCnuc")
)

#PAs on CNUC PA Dataset that there are not in Correia PA Dataset
pa_excluded <- anti_join(pa_dataset,
                         pa_correia,
                         by = c("codCnuc" = "id"))

#PAs on Correia PA Dataset that there are not in CNUC PA Dataset
pa_excluded2 <- anti_join(pa_correia,
                          pa_dataset,
                          by = c("id" = "codCnuc")
)

#PAs with means of pageviews
pa_means <- read_csv("./data/BPA_Wiki_Means_2019-06-21.csv")

#Merge means and variables
pa_means_merge <- left_join (x = pa_merge,
                             y = pa_means,
                             by = c("id" = "cod_cnuc"))

# ---- Scatterplot: PT vs ENG Pageviews ----
# Only PAs with pages in both languages
names(pa_means)
(
  p <- ggplot(pa_means,
              aes(x = mean.eng, y = mean.pt)) +
    geom_point(color = "cadetblue4",
               alpha = 0.3,
               size = 3) +
    geom_smooth(method = "lm") +
    labs(
      x = "Page views of \nEnglish Articles",
      y = "Page views of \nPortuguese Articles") +
    scale_y_log10(labels = comma, limits=c(NA,1000), breaks = c(1, 10, 100, 1000)) + 
    scale_x_log10(labels = comma, limits=c(NA,1000), breaks = c(1, 10, 100, 1000)) +
    theme(axis.text.x = element_text(size = 10, 
                                     angle = 45, 
                                     hjust = 1),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 14),
          panel.background = element_rect(fill = "white",
                                          colour = "lightgrey",
                                          size = 0.2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "#ecf0f1"), 
          panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                          colour = "#ecf0f1")
    )
)

(p2 <- ggMarginal(p, type="boxplot", size = 12, fill="white")) # Add a boxplot in margins to Lang Pageviews

today <- Sys.Date()

svg(paste0("./figures/PA_Pageviews_Scatterplot", today,".svg"))
p2
dev.off()  
png(paste0("./figures/PA_Pageviews_Scatterplot", today,".png"))
p2
dev.off()  


# Correlation tests -------------------------------------------------------
shapiro.test(pa_means$mean.pt)
shapiro.test(pa_means$mean.eng)
ggqqplot(pa_means$mean.pt, ylab = 'Portuguese')
ggqqplot(pa_means$mean.eng, ylab = 'English')

# Pearson
cor(pa_means$mean.eng, pa_means$mean.pt, method = 'pearson')
cor.test(pa_means$mean.eng, pa_means$mean.pt, method = 'pearson', use = 'complete.obs')

# Spearman
cor(pa_means$mean.eng, pa_means$mean.pt, method = 'spearman')
cor.test(pa_means$mean.eng, pa_means$mean.pt, method = 'spearman')

# Kendall
cor(pa_means$mean.eng, pa_means$mean.pt, method = 'kendall')
cor.test(pa_means$mean.eng, pa_means$mean.pt, method = 'kendall')
