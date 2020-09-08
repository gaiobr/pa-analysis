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

# ---- Boxplot: Pageviews by Categories ----
pa_means_melted <- melt(data = pa_means_merge, id.vars = c("id", "cat.y", "esfera.x"), measure.vars = c("mean.eng", "mean.pt"))
head(pa_means_melted)

# Rename category values

# Portuguese version
# pa_means_melted$cat.y[pa_means_melted$cat.y == "Área de Proteção Ambiental"] <- "APA"
# pa_means_melted$cat.y[pa_means_melted$cat.y == "Área de Relevante Interesse Ecológico"] <- "ARIE"
# pa_means_melted$cat.y[pa_means_melted$cat.y == "Estação Ecológica"] <- "ESEC"
# pa_means_melted$cat.y[pa_means_melted$cat.y == "Floresta"] <- "FLO"
# pa_means_melted$cat.y[pa_means_melted$cat.y == "Monumento Natural"] <- "MONAT"
# pa_means_melted$cat.y[pa_means_melted$cat.y == "Parque"] <- "PAR"
# pa_means_melted$cat.y[pa_means_melted$cat.y == "Refúgio de Vida Silvestre"] <- "RVS"
# pa_means_melted$cat.y[pa_means_melted$cat.y == "Reserva Biológica"] <- "REBIO"
# pa_means_melted$cat.y[pa_means_melted$cat.y == "Reserva de Desenvolvimento Sustentável"] <- "RDS"
# pa_means_melted$cat.y[pa_means_melted$cat.y == "Reserva Extrativista"] <- "RESEX"
# pa_means_melted$cat.y[pa_means_melted$cat.y == "Reserva Particular do Patrimônio Natural"] <- "RPPN"

# Base
# Park (PAR) 
# Biological Reserve (BR) 
# Ecological Station (ES) 
# Wildlife Refuge (WR) 
# Natural Monument (NM) 
# Forest (NF) 
# Sustainable Development Reserve (SDR) 
# Extractive Reserve (ER) 
# Environmental Protection Area (EPA) 
# Area of Relevant Ecological Interest (AREI)
# Private Natural Heritage Reserve (PNHR)

# English version
pa_means_melted$cat.y[pa_means_melted$cat.y == "Área de Proteção Ambiental"] <- "EPA"
pa_means_melted$cat.y[pa_means_melted$cat.y == "Área de Relevante Interesse Ecológico"] <- "AREI"
pa_means_melted$cat.y[pa_means_melted$cat.y == "Estação Ecológica"] <- "ES"
pa_means_melted$cat.y[pa_means_melted$cat.y == "Floresta"] <- "FOR"
pa_means_melted$cat.y[pa_means_melted$cat.y == "Monumento Natural"] <- "NM"
pa_means_melted$cat.y[pa_means_melted$cat.y == "Parque"] <- "PAR"
pa_means_melted$cat.y[pa_means_melted$cat.y == "Refúgio de Vida Silvestre"] <- "WR"
pa_means_melted$cat.y[pa_means_melted$cat.y == "Reserva Biológica"] <- "BR"
pa_means_melted$cat.y[pa_means_melted$cat.y == "Reserva de Desenvolvimento Sustentável"] <- "SDR"
pa_means_melted$cat.y[pa_means_melted$cat.y == "Reserva Extrativista"] <- "ER"
pa_means_melted$cat.y[pa_means_melted$cat.y == "Reserva Particular do Patrimônio Natural"] <- "PNHR"

table(pa_means_melted$cat.y)



(
  p_c <- ggplot(data = subset(pa_means_melted, !is.na(cat.y) & cat.y != "Reserva Particular do Patrimônio Natural"),
                aes(x = cat.y, y = value, color = variable)) +
    geom_boxplot(outlier.size = 0.3) +
    coord_trans(y = "log10") + # Transforms axes without changing values
    scale_color_discrete(name = "Languages", labels = c("English", "Portuguese")) +
    #  scale_x_discrete(labels = c("State", "Federal", "Municipality")) +
    scale_y_continuous(breaks=c(1, 10, 100, 1000)) + # Manually controls tick marks in y axis
    labs(
      x = "Categories",
      y = "Mean Page Views") +
    theme(plot.title = element_text(size = 14,
                                    hjust = 0),
          axis.text.x = element_text(size = 12, 
                                     angle = 45, 
                                     hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)) +
    theme(
      panel.background = element_rect(fill = "white",
                                      colour = "lightgrey",
                                      size = 0.2, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "#ecf0f1"), 
      panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                      colour = "#ecf0f1")
    )
)

today = Sys.Date()
svg(paste0("./figures/PA_Boxplot_All_Categories_Pageviews_", today,".svg"), width = 1000, height = 500)
p_c
dev.off()  
png(paste0("./figures/PA_Boxplot_All_Categories_Pageviews_", today,".png"), width = 1000, height = 500)
p_c
dev.off()  

# Print Boxplots together
p <- ggarrange(p_b, p_g, p_c,
               ncol = 1,
               nrow = 3,
               heights = c(1,1,1))

today = Sys.Date()
#svg(paste0("./figures/PA_Categories_Pageviews_Boxplot", today,".svg"), width = 1000, height = 500)
png(paste0("./figures/PA_All_Pageviews_Boxplot", today,".png"), width = 1000, height = 1200)
p
dev.off()  