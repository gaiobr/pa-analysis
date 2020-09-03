####Clear R memory
rm(list=ls())

####Set working directory
setwd(choose.dir())

####Load data
#Pa data from EI paper
# Lacos
pa_df<-read.table("C:/Users/LACOS21/Documents/tmp_gaio/tmp/data/PA_data_030416.csv",sep=";",dec=".", header=T, quote = "\"")
#pa_df<-read.table("Z:/Dropbox/Pesquisa/Doutorado/Qualificação II/Análises Ricardo/PA_data_030416_raw.csv",sep=";",dec=".", header=T, quote = "\"")
#pa_df<-read.table("./data/PA_data_030416.csv",sep=";",dec=".", header=T, quote = "\"")
# Mint
#pa_df<-read.table("/media/gaio/Argos/Dropbox/Pesquisa/Doutorado/Qualificação II/Análises Ricardo/PA_data_030416.csv",sep=";",dec=".", header=T, quote = "\"")
print(summary(pa_df))

#Wiki data with PT and EN
wiki_all_df<-read.table("./data/BPA_Wiki_Means_2019-06-21.csv",sep=",",dec=".", header=T)
#wiki_all_df<-read.table("C:/Users/LACOS21/Documents/tmp_gaio/tmp/data/BPA_Wiki_Means_2019-06-21.csv",sep=",",dec=".", header=T)
#Uncomment below to consider only Federal PAs
#wiki_all_df <- wiki_all_df %>% filter(wiki_all_df$esfera == "Federal")
#EndTests
print(summary(wiki_all_df))


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
library(relaimpo)

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

write_csv(model_df, "model_df_hurdle_pt.csv")

#And now let's try in PT
mod1_pt<-hurdle(mean.pt ~ area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+bir_div+year_mod+bioma+group+govern|
                   area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+bir_div+year_mod+bioma+group+govern,
             data = model_df,
             dist = "poisson",
             na.action = "na.fail")
mod2_pt<-hurdle(mean.pt ~ area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+bir_div+year_mod+bioma+group+govern|
                   area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+bir_div+year_mod+bioma+group+govern,
             data = model_df,
             dist = "negbin",
             na.action = "na.fail")
mod3_pt<-hurdle(mean.pt ~ area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+bir_div+year_mod+bioma+group+govern|
                   area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+bir_div+year_mod+bioma+group+govern,
             data = model_df,
             dist = "geometric",
             na.action = "na.fail")
AICc(mod1_pt,mod2_pt,mod3_pt)

#Negative binomial seems to be the best choice based on AICc - Let's keep it and run all model combinations
dd_views_pt <- dredge(mod2_pt,evaluate=TRUE, rank = "AICc", REML = F)
#dd_views_pt <- readRDS("hurdle_model_ENG_1.1.rds")
dd_views_pt
gmod_views_pt <- get.models(dd_views_pt,subset = delta < 4)
gmod_views_pt
mod_avg_pt<-model.avg(gmod_views_pt)
summary(mod_avg_pt)

saveRDS(dd_views_pt, "hurdle_model_PT_1.rds")

out <- capture.output(summary(mod_avg_pt))
cat("Hurdle Model PT", out, file='Hurdle_Model_PT.txt', sep = '\n', append = FALSE)

out2 <- capture.output(confint(mod_avg_pt))
cat("Confint Hurdle Model PT Avg Model", out2, file='Confint_Hurdle_Avg_Model_PT.txt', sep = '\n', append = FALSE)

###calc.relimp(dd_views_pt, type = c("lmg"), rela = TRUE) 

library(ggplot2)

model <- read.table("./data/Hurdle Average Confit PT_count.csv", sep=",", dec=".", header=T)
model$var <- factor(model$var, levels=unique(as.character(model$var)) )
names(model)
p_pt_count <- ggplot(model, aes(x = var, y = estimate, ymin = inferior, ymax = superior, color = estimate)) +
  geom_pointrange() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype="dotted") +
  xlab('Model Parameters') +
  ylab('Estimate') +
  labs(
    title = "Portuguese Protected Area Page views",
    subtitle = "Count Model"
  ) +
  theme(legend.position = "none")
p_pt_count

svg(paste0("./figures/PA_Pt_Count_Model_", Sys.Date(),".svg"))
#png(paste0("./figures/PA_Eng_Zero_Model", today,".png"))
p_pt_count
dev.off()  


model <- read.table("./data/Hurdle Average Confit PT_zeros.csv", sep=",", dec=".", header=T)
model2 <- read.table("./data/Hurdle Average Confit ENG_zeros.csv", sep=",", dec=".", header = T)
model$var <- factor(model$var, levels=unique(as.character(model$var)) )
names(model)
p_pt_zero <- ggplot(model, aes(x = var, y = estimate, ymin = inferior, ymax = superior, color = estimate)) +
  geom_pointrange() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype="dotted") +
  xlab('Model Parameters') +
  ylab('Estimate') +
  labs(
    title = "Portuguese Protected Area Page views",
    subtitle = "Zero Model"
  ) +
  theme(legend.position = "none")
p_pt_zero

svg(paste0("./figures/PA_Pt_Zero_Model_", Sys.Date(),".svg"))
#png(paste0("./figures/PA_Eng_Zero_Model", today,".png"))
p_pt_zero
dev.off()  
