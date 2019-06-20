# ---- Head ------------------- 
# Project: Protected Areas Social Interest
#
# Explore Data
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

# ---- Load datasets ----------
#PA Dataset (CNUC)
pa_dataset <- read_csv("./data/BrazilianProtectedAreas_2019-05-13.csv")
#PA data from Correia et al paper (Ecological Indicators - Correia, R.A. et al. 2019)
pa_correia <- read.table("/media/gaio/Argos/Dropbox/Pesquisa/Doutorado/Qualificação II/Análises Ricardo/PA_data_030416.csv",sep=";",dec=".", header=T, quote = "\"")
#pa_correia <- read.table("/home/gaio/Dropbox/Pesquisa/Doutorado/Qualificação II/Análises Ricardo/PA_data_030416.csv",sep=";",dec=".", header=T, quote = "\"")
#pa_correia <- read.table("Z:/Dropbox/Pesquisa/Doutorado/Qualificação II/Análises Ricardo/PA_data_030416.csv",sep=";",dec=".", header=T, quote = "\"")
#PA Dataset Merge from CNUC PA Dataset and Correia PA Dataset 
eng_means <- read_csv('./data/BPA_Wiki_Eng_2019-05-29.csv')
eng_month_means <- read_csv('./data/BPA_Wiki_Eng_Month_2019-05-29.csv')
pt_means <- read_csv('./data/BPA_Wiki_Pt_2019-05-29.csv')
pt_month_means <- read_csv('./data/BPA_Wiki_Pt_Month_2019-05-29.csv')

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
pa_means <- read_csv("./data/BPA_Wiki_Means_2019-06-20.csv")

#Merge means and variables
pa_means_merge <- left_join (x = pa_merge,
                             y = pa_means,
                             by = c("id" = "cod_cnuc"))

#Merge PT and ENG datasets adding LANG column
pt_means$lang <- "pt" 
eng_means$lang <- "eng" 
pa_means_vertical <- rbind(pt_means, eng_means)

# ---- _____ Explore Data ----------

# ---- Boxplot: Pageviews by Language ----
# All PAs with Pages #
names(pa_means_vertical)
p <- ggplot(pa_means_vertical, 
            aes(x = lang, y = mean, color = lang)) +
  geom_boxplot(outlier.color = "red",
               outlier.size = 1) +
  #geom_jitter(shape = 1, position = position_jitter(0.2)) +
  stat_summary(fun.y = mean, # Add a mean point
               geom = "point") +
  coord_trans(y = "log10") + # Transforms axes without changing values
  scale_color_discrete(name = "Language", labels = c("English", "Portuguese")) +
  scale_x_discrete(labels = c("English", "Portuguese")) +
  scale_y_continuous(breaks=c(1, 2, 10, 50, 100, 200, 300)) + # Manually controls tick marks in y axis
  labs(title = "Brazilian Protected Areas on Wikipedia\nPageviews",
       x = "Languages",
       y = "PA Pageviews \n (Means)") +
  theme(plot.title = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5))

p
  # scale_y_log10() # Scale Y axys into a Log scale: base 10
svg("PA_Pageviews_Boxplot.svg")
p
dev.off()  

png("PA_Pageviews_Boxplot.png")
p
dev.off()

# ---- Scatterplot: PT vs ENG Pageviews ----
# Only PAs with pages in both languages
names(pa_means)
p <- ggplot(pa_means,
            aes(x = mean.eng, y = mean.pt)) +
  scale_y_log10(labels = comma) + # Forces R to plot in long format instead abbreviated
  scale_x_log10() +
#  coord_trans(x = "log10", y = "log10") + # Transforms axes without changing values
#  scale_y_continuous(labels = comma) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Brazilian Protected Areas on Wikipedia\nPageviews",
       x = "English PA \nPageviews",
       y = "Portuguese PA \nPageviews") +
  theme(plot.title = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5))
  
p
svg("PA_Pageviews_Scatterplot.svg")
p
dev.off()  

png("PA_Pageviews_Scatterplot.png")
p
dev.off()


# ---- Boxplot: Pageviews by Biomes ----
pa_means_melted <- melt(data = pa_means_merge, id.vars = c("id", "bioma"), measure.vars = c("mean.eng", "mean.pt"))
names(pa_means_melted)

p <- ggplot(data = subset(pa_means_melted, !is.na(bioma)), # Subset to remove NA biome values
            aes(x = bioma, y = value, color = variable)) +
  geom_boxplot(outlier.size = 0.3) +
  coord_trans(y = "log10") + # Transforms axes without changing values
  scale_color_discrete(name = "Languages", labels = c("English", "Portuguese")) +
  scale_x_discrete() +
  scale_y_continuous(breaks=c(1, 2, 10, 50, 100, 200, 300)) + # Manually controls tick marks in y axis
  labs(title = "Brazilian Protected Areas on Wikipedia\nPageviews vs Biomes",
     x = "Biomes",
     y = "PA Pageviews \n (Means)") +
  theme(plot.title = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10))

p
svg("PA_Biomes_Pageviews_Boxplot.svg", width = 1000, height = 500)
p
dev.off()  

png("PA_Biomes_Pageviews_Boxplot.png", width = 1000, height = 500)
p
dev.off()

# ---- Boxplot: Pageviews by Level of Government ----
pa_means_melted <- melt(data = pa_means_merge, id.vars = c("id", "govern"), measure.vars = c("mean.eng", "mean.pt"))
names(pa_means_melted)

p <- ggplot(data = pa_means_melted,
            aes(x = govern, y = value, color = variable)) +
  geom_boxplot(outlier.size = 0.3) +
  coord_trans(y = "log10") + # Transforms axes without changing values
  scale_color_discrete(name = "Languages", labels = c("English", "Portuguese")) +
  scale_x_discrete(labels = c("State", "Federal", "Municipality")) +
  scale_y_continuous(breaks=c(1, 2, 10, 50, 100, 200, 300)) + # Manually controls tick marks in y axis
  labs(title = "Brazilian Protected Areas on Wikipedia\nPageviews vs Levels of Government",
       x = "Levels of Government",
       y = "PA Pageviews \n (Means)") +
  theme(plot.title = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10))

p
svg("PA_Government_Pageviews_Boxplot.svg", width = 1000, height = 500)
p
dev.off()  

png("PA_Government_Pageviews_Boxplot.png", width = 1000, height = 500)
p
dev.off()

# ---- Boxplot: Pageviews by Categories ----
pa_means_melted <- melt(data = pa_means_merge, id.vars = c("id", "cat2"), measure.vars = c("mean.eng", "mean.pt"))
names(pa_means_melted)

p <- ggplot(data = pa_means_melted,
            aes(x = cat2, y = value, color = variable)) +
  geom_boxplot(outlier.size = 0.3) +
  coord_trans(y = "log10") + # Transforms axes without changing values
  scale_color_discrete(name = "Languages", labels = c("English", "Portuguese")) +
#  scale_x_discrete(labels = c("State", "Federal", "Municipality")) +
  scale_y_continuous(breaks=c(1, 2, 10, 50, 100, 200, 300)) + # Manually controls tick marks in y axis
  labs(title = "Brazilian Protected Areas on Wikipedia\nPageviews vs Categories",
       x = "Categories",
       y = "PA Pageviews \n (Means)") +
  theme(plot.title = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10))

p
svg("PA_Categories_Pageviews_Boxplot.svg", width = 1000, height = 500)
p
dev.off()  

png("PA_Categories_Pageviews_Boxplot.png", width = 1000, height = 500)
p
dev.off()

# ---- _____ End Explore Data ----


# ---- Number of pageviews ---- 
# Protected Areas with mean pageviews greater than 10
pt_pv <- pt_means %>%
  filter(pt_means$mean >= 20)
print("PT Pageviews >= 20")
print(paste("Quantidade:", count(pt_pv)))
pa_list_names(pa_dataset, pt_pv$cod_cnuc)

eng_pv <- eng_means %>%
  filter(eng_means$mean >= 10)
print("ENG Pageviews >= 10")
print(paste("Quantidade:", count(eng_10)))
pa_list_names(pa_dataset, eng_10$cod_cnuc)


#______________________________
# ### CNUC Dataset ###

# Number of PT and ENG Wikipages
n_pt_pages <- length(na.omit(pa_dataset$idPtWikiData))
n_pt_pages
n_eng_pages <- length(na.omit(pa_dataset$idEnWikiData))
n_eng_pages

# Number of Wikipages in both languages
pa_both <- pa_dataset %>%
  filter(!is.na(pa_dataset$idPtWikiData))
pa_both <- pa_both  %>%
  filter(!is.na(pa_both$idEnWikiData))
n_pa_both <- nrow(pa_both)

# Number of PT and ENG Wikipages without a pair
pt_unpair <- pa_dataset %>%
  filter(is.na(pa_dataset$idEnWikiData))
pt_unpair <- pt_unpair %>%
  filter(!is.na(pt_unpair$idPtWikiData))
n_pt_unpair <- nrow(pt_unpair)

eng_unpair <- pa_dataset %>%
  filter(is.na(pa_dataset$idPtWikiData))
eng_unpair <- eng_unpair %>%
  filter(!is.na(eng_unpair$idEnWikiData))
n_eng_unpair <- nrow(eng_unpair)

#______________________________
# ### Merge Dataset ###

# Number of PT and ENG Wikipages
n_pt_mpages <- length(na.omit(pa_merge$idPtWikiData))
n_pt_mpages
n_eng_mpages <- length(na.omit(pa_merge$idEnWikiData))
n_eng_mpages

# Number of Wikipages in both languages
pa_mboth <- pa_merge %>%
  filter(!is.na(pa_merge$idPtWikiData))
pa_mboth <- pa_mboth  %>%
  filter(!is.na(pa_mboth$idEnWikiData))
n_pa_mboth <- nrow(pa_mboth)
n_pa_mboth

# Number of PT and ENG Wikipages without a pair
pt_munpair <- pa_merge %>%
  filter(is.na(pa_merge$idEnWikiData))
pt_munpair <- pt_munpair %>%
  filter(!is.na(pt_munpair$idPtWikiData))
n_pt_munpair <- nrow(pt_munpair)
n_pt_munpair

eng_munpair <- pa_merge %>%
  filter(is.na(pa_merge$idPtWikiData))
eng_munpair <- eng_munpair %>%
  filter(!is.na(eng_munpair$idEnWikiData))
n_eng_munpair <- nrow(eng_munpair)
n_eng_munpair

#______________________________
# ### Biomes ###
# Number of biomes on PA Merge dataset
biomes <- unique(pa_merge$bioma)
biomes

# Number of biomes on PA Wikipages in both languages
pa_biomes <- table(pa_mboth$bioma)
pa_biomes

#______________________________
# ### Categories ###
categories <- unique(pa_mboth$cat.y)
categories

pa_categories <- table(pa_mboth$cat2)
pa_categories

#______________________________
# ### Governance ###
governance <- unique(pa_mboth$esfera)
governance

pa_governance <- table(pa_mboth$esfera)
pa_governance

table(pa_mboth$year)

table(pa_mboth$paddd)

table(pa_mboth$paddd_type)

table(pa_mboth$paddd_type.1)

table(pa_mboth$paddd_status)

table(pa_mboth$paddd_enacted)

table(pa_mboth$paddd_events)

#______________________________
# ---- Most visualized PAs ----


pa_pt_descending <- pt_means[with(pt_means, order(-mean)),]
head(pa_pt_descending)  

pa_eng_descending <- eng_means[with(eng_means, order(-mean)),]
head(pa_eng_descending)  

for (i in 1:10) {
  print(pa_name(pa_dataset, pa_pt_descending$cod_cnuc[i]))
  print(pa_pt_descending$cod_cnuc[i])
  print(pa_pt_descending$mean[i])
}

for (i in 1:10) {
  print(pa_name(pa_dataset, pa_eng_descending$cod_cnuc[i]))
  print(pa_eng_descending$mean[i])
}

# Number of PAs with pageviews greater than 10
sum(pt_means$mean > 10)
sum(eng_means$mean > 10)

# Number of PAs with pageviews less than 10
sum(pt_means$mean < 1)
sum(eng_means$mean > 1)


## Plot pageviews by PA ##
library(stats)
library(scales)
library(ggplot2)
library(Hmisc)
pa_month_analysis <- read_csv("./data/pt/0000.00.0037_Área de Relevante Interesse Ecológico Ilhas Queimada Grande e Queimada Pequena_2015-07-01_2019-05-21.csv")
# Rearrange CSV Pageviews Dataset
pa_month_analysis <- pa_month_analysis %>%
  filter(Language == "en") %>%
  select(4:ncol(pa_month_analysis)) %>%
  t()

pa_month_analysis <- as.data.frame(pa_month_analysis)

pa_month_analysis <- cbind(rownames(pa_month_analysis), pa_month_analysis)
colnames(pa_month_analysis) <- c("Dates", "Views")

summary(pa_month_analysis)

pa_month_analysis$Dates <- as.Date(pa_month_analysis$Dates) 
str(pa_month_analysis)
pa_month_analysis$Month <- as.Date(cut(pa_month_analysis$Dates, breaks = "month"))
pa_month_analysis$Week <- as.Date(cut(pa_month_analysis$Dates, breaks = "week", start.on.monday = FALSE))
ggplot(data = pa_month_analysis, 
       aes(Month, Views)) +
  stat_summary(fun.y = sum, geom = "line") + 
  scale_x_date(
         labels = date_format("%Y-%m"),
         breaks = "1 month"
       )

ggplot(data = pa_month_analysis, 
       aes(Month, Views)) +
  stat_summary(fun.data = mean_cl_normal) + 
  geom_smooth(method = 'lm', formula = y ~ x)

fit <- lm(as.Date(Dates)~Views, data = pa_month_analysis)
abline(fit, col="red")
lines(pa_month_analysis$Views, fitted(fit), col="blue")
fit

## END DELETE ##

# ---- Tests ----------

#______________________________
# ### Normality tests ###
hist(pa_means_merge$mean.pt)
qqnorm(pa_means_merge$mean.pt)
qqline(pa_means_merge$mean.pt)

shapiro.test(pa_means_merge$mean.pt)

hist(pa_means_merge$mean.eng)
qqnorm(pa_means_merge$mean.eng)
qqline(pa_means_merge$mean.eng)

shapiro.test(pa_means_merge$mean.eng)

#______________________________
# ### Kruskal-Wallis ###
# Categories
kruskal.test(mean.eng ~ cat.y, data = pa_means_merge)
kruskal.test(mean.pt ~ cat.x, data = pa_means_merge)

# Biomes
kruskal.test(mean.eng ~ bioma, data = pa_means_merge)
kruskal.test(mean.pt ~ bioma, data = pa_means_merge)

#______________________________
# ### ANOVA ###
dplyr::sample_n(pa_means, 10)
levels(pa_means[ ,3])
lapply(pa_means, levels)

group_by(pa_means, esfera) %>%
  summarise(
    count = n(),
    mean = mean(mean.eng, na.rm = TRUE),
    sd = sd(mean.eng, na.rm = TRUE)
  )

# Box plots
# ++++++++++++++++++++
# Plot means by government levels and color by gov levels
library(ggpubr)
ggboxplot(pa_means, x = "esfera", y = "mean.eng", 
          color = "esfera", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Federal", "Estadual", "Municipal"),
          ylab = "Protected Area Interest (English)", xlab = "Level of Government",
          yscale = "log10",
          add = "mean_sd", error.plot = "errorbar",
          add.params = list(binwidth = 0.1, dotsize = 0.3)
)

ggboxplot(pa_means, x = "esfera", y = "mean.pt", 
          color = "esfera", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Federal", "Estadual", "Municipal"),
          ylab = "Protected Area Interest (Portuguese)", xlab = "Level of Government",
          yscale = "log10",
          add = "mean_sd", error.plot = "errorbar",
          add.params = list(binwidth = 0.1, dotsize = 0.3)
)

# Plot means by biomes and color by biomes
ggboxplot(pa_means_merge, x = "bioma", y = "mean.pt", 
          color = "bioma",
          ylab = "Protected Area Interest (Portuguese)", xlab = "Biomes",
          yscale = "log10",
          add = "mean_sd", error.plot = "errorbar",
          add.params = list(binwidth = 0.1, dotsize = 0.3)
)

ggboxplot(pa_means_merge, x = "bioma", y = "mean.eng", 
          color = "bioma",
          ylab = "Protected Area Interest (English)", xlab = "Biomes",
          yscale = "log10",
          add = "mean_sd", error.plot = "errorbar",
          add.params = list(binwidth = 0.1, dotsize = 0.3)
)

# Plot means by biomes and color by biomes
ggboxplot(pa_means_merge, x = "cat2", y = "mean.pt", 
          color = "cat2",
          ylab = "Protected Area Interest (Portuguese)", xlab = "Categories",
          yscale = "log10",
          add = "mean_sd", error.plot = "errorbar",
          add.params = list(binwidth = 0.1, dotsize = 0.3)
)

ggboxplot(pa_means_merge, x = "cat2", y = "mean.eng", 
          color = "cat2",
          ylab = "Protected Area Interest (English)", xlab = "Categories",
          yscale = "log10",
          add = "mean_sd", error.plot = "errorbar",
          add.params = list(binwidth = 0.1, dotsize = 0.3)
)

tmp <- pa_means_merge %>%
  filter(pa_means_merge$cat2 == "PARNA")

ggboxplot(tmp, x = "year", y = "mean.eng", 
          color = "year",
          ylab = "Protected Area Interest (English)", xlab = "Year",
          yscale = "log10",
          add = "mean_sd", error.plot = "errorbar",
          add.params = list(binwidth = 0.1, dotsize = 0.3)
)




ggbarplot(pa_means, x = "esfera", y = "mean.pt", 
          add = "mean_se", error.plot = "errorbar",
          order = c("Federal", "Estadual", "Municipal"),
          ylab = "Médias Português", xlab = "Esfera de Governo"
)

boxplot(mean.eng ~ esfera, data = pa_means,
        log = "y", col = "yellow")


boxplot(mean.pt ~ esfera, data = pa_means,
        log = "y", col = "yellow")

boxplot(mean.eng ~ bioma, data = pa_means_merge,
        log = "y", col = "yellow")

boxplot(mean.pt ~ bioma, data = pa_means_merge,
        log = "y", col = "yellow")

boxplot(mean.eng ~ cat2, data = pa_means_merge,
        log = "y", col = "yellow")

boxplot(mean.pt ~ cat2, data = pa_means_merge,
        log = "y", col = "yellow")

# Mean plots
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(pa_means, x = "esfera", y = "mean.eng", 
       add = c("mean_se", "jitter"), 
       ylab = "Médias Inglês", xlab = "Esfera de Governo")



# Box plot
boxplot(mean.eng ~ esfera, data = pa_means,
        xlab = "Esfera de Governo", ylab = "Médias Inglês",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))

# plotmeans
library("gplots")
plotmeans(mean.eng ~ esfera, data = pa_means, frame = FALSE,
          xlab = "Esfera de Governo", ylab = "Médias Inglês",
          main="Mean Plot with 95% CI") 

summary(pa_means)

names(pa_means_merge)
# Compute the analysis of variance
reseng.aov <- aov(mean.eng ~ bioma, data = pa_means_merge)
respt.aov <- aov(mean.pt ~ bioma, data = pa_means_merge)
# Summary of the analysis
summary(reseng.aov)
summary(respt.aov)

TukeyHSD(reseng.aov)

table(pa_means_merge$esfera.y)

pt_means
date_tmp <- "2015-07-01"
date_tmp <- as.Date(date_tmp)
date_tmp + pt_means$total_days[1]

