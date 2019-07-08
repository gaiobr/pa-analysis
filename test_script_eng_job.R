####Clear R memory
rm(list=ls())

####Set working directory
setwd(choose.dir())

####Load data
#Pa data from EI paper
pa_df<-read.table("C:/Users/LACOS21/Documents/tmp_gaio/tmp/data/PA_data_030416.csv",sep=";",dec=".", header=T, quote = "\"")
#pa_df<-read.table("Z:/Dropbox/Pesquisa/Doutorado/Qualificação II/Análises Ricardo/PA_data_030416_raw.csv",sep=";",dec=".", header=T, quote = "\"")
#pa_df<-read.table("./data/PA_data_030416.csv",sep=";",dec=".", header=T, quote = "\"")
#pa_df<-read.table("/home/gaio/Dropbox/Pesquisa/Doutorado/Qualificação II/Análises Ricardo/PA_data_030416.csv",sep=";",dec=".", header=T, quote = "\"")
print(summary(pa_df))

#Wiki data with PT and EN
wiki_all_df<-read.table("./data/BPA_Wiki_Means_2019-06-21.csv",sep=",",dec=".", header=T)
#Uncomment below to consider only Federal PAs
#wiki_all_df <- wiki_all_df %>% filter(wiki_all_df$esfera == "Federal")
#EndTests
summary(wiki_all_df)


#Wiki data PT only
wiki_pt_df<-read.table("./data/BPA_Wiki_Pt_2019-06-21.csv",sep=",",dec=".", header=T)
summary(wiki_pt_df)

#Wiki data EN only
wiki_en_df<-read.table("./data/BPA_Wiki_Eng_2019-06-21.csv",sep=",",dec=".", header=T)
summary(wiki_en_df)


####Load packages for analysis
library(tidyverse)
library(ggplot2)
library(ggExtra)
library(ggpubr)
library(pscl)
library(MuMIn)
library(scales)


####Merge Wiki_all and PA_EI datasets
merge_pa_df<-inner_join(pa_df,
                     wiki_all_df,
                     by = c("id" = "cod_cnuc"))
head(merge_pa_df)





#Since views are similar for both PT and EN, lets model the data just with PT views
merge_model_df<-left_join(x = pa_df,
                          y = wiki_all_df,
                          by = c("id" = "cod_cnuc"))
head(merge_model_df)

#Let's use the same variables as used for the EI paper
#  Z standardization
merge_model_df$area_km2_mod<-scale(log10(merge_model_df$area_km2+1))
merge_model_df$pop_50k_mod<-scale(log10(merge_model_df$pop_50k+1))
merge_model_df$alt_r_mod<-scale(log10(merge_model_df$alt_r+1))
merge_model_df$acc_50k_mod<-scale(log10(merge_model_df$acc_50k+1))
merge_model_df$tot_div_mod<-scale(log10(merge_model_df$tot_div+1))
merge_model_df$Min_dist_mod<-scale(log10(merge_model_df$Min_dist+1))
merge_model_df$year_mod<-scale(2019-merge_model_df$year)

#Lets define the response variable
merge_model_df$mean.pt<-ifelse(is.na(merge_model_df$mean.pt),0,ceiling(merge_model_df$mean.pt))
merge_model_df$mean.eng<-ifelse(is.na(merge_model_df$mean.eng),0,ceiling(merge_model_df$mean.eng))

#let's create a data.frame only with the modelling data
names(merge_model_df)
model_df<-merge_model_df[,c(2,5,6,7,8,9:10,14,22,27,31,34,39:45)]
summary(model_df)
model_df<-na.exclude(model_df)
summary(model_df)
model_df$bioma <- relevel(model_df$bioma, "Caatinga")
model_df$govern <- relevel(model_df$govern, "federal")

write_csv(model_df, "model_df_hurdle_eng.csv")

#And now let's try in EN
mod1_eng<-hurdle(mean.eng ~ area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+bir_div+year_mod+bioma+group+govern|
                   area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+bir_div+year_mod+bioma+group+govern,
             data = model_df,
             dist = "poisson",
             na.action = "na.fail")
mod2_eng<-hurdle(mean.eng ~ area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+bir_div+year_mod+bioma+group+govern|
                   area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+bir_div+year_mod+bioma+group+govern,
             data = model_df,
             dist = "negbin",
             na.action = "na.fail")
mod3_eng<-hurdle(mean.eng ~ area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+bir_div+year_mod+bioma+group+govern|
                   area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+bir_div+year_mod+bioma+group+govern,
             data = model_df,
             dist = "geometric",
             na.action = "na.fail")
AICc(mod1_eng,mod2_eng,mod3_eng)

#Negative binomial seems to be the best choice based on AICc - Let's keep it and run all model combinations
dd_views_eng <- dredge(mod2_eng,evaluate=TRUE, rank = "AICc", REML = F)
dd_views_eng
gmod_views_eng <- get.models(dd_views_eng,subset = delta < 4)
gmod_views_eng
mod_avg_eng<-model.avg(gmod_views_eng)
summary(mod_avg_eng)

out <- capture.output(summary(mod_avg_eng))
cat("Hurdle Model ENG", out, file='Hurdle_Model_ENG.txt', sep = '\n', append = FALSE)

out2 <- capture.output(confint(mod_avg_eng))
cat("Confint Hurdle Model ENG Avg Model", out2, file='Confint_Hurdle_Avg_Model_ENG.txt', sep = '\n', append = FALSE)

model <- read.table("./data/Hurdle Average Confit ENG_count.csv", sep=",", dec=".", header=T)
model$var <- factor(model$var, levels=unique(as.character(model$var)) )
names(model)
p <- ggplot(model, aes(x = var, y = estimate, ymin = inferior, ymax = superior)) +
  geom_pointrange() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype="dotted") +
  xlab('Variables') +
  ylab('Estimate') +
  labs(
    title = "English",
    subtitle = "Count"
  )
p

model <- read.table("./data/Hurdle Average Confit ENG_zeros.csv", sep=",", dec=".", header=T)
model$var <- factor(model$var, levels=unique(as.character(model$var)) )
names(model)
p2 <- ggplot(model, aes(x = var, y = estimate, ymin = inferior, ymax = superior)) +
  geom_pointrange() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype="dotted") +
  xlab('Variables') +
  ylab('Estimate') +
  labs(
    title = "English",
    subtitle = "Zero"
  )
p2

