####Clear R memory
rm(list=ls())


####Set working directory
setwd(choose.dir())


####Load data
#Pa data from EI paper
pa_df<-read.table("/media/gaio/Argos/Dropbox/Pesquisa/Doutorado/Qualificação II/Análises Ricardo/PA_data_030416.csv",sep=";",dec=".", header=T, quote = "\"")
#pa_df<-read.table("/home/gaio/Dropbox/Pesquisa/Doutorado/Qualificação II/Análises Ricardo/PA_data_030416.csv",sep=";",dec=".", header=T, quote = "\"")
summary(pa_df)

#Wiki data with PT and EN
wiki_all_df<-read.table("./data/BPA_Wiki_Means_2019-05-24.csv",sep=",",dec=".", header=T)
summary(wiki_all_df)

#Wiki data PT only
wiki_pt_df<-read.table("./data/BPA_Wiki_Pt_2019-05-23.csv",sep=",",dec=".", header=T)
summary(wiki_pt_df)

#Wiki data EN only
wiki_en_df<-read.table("./data/BPA_Wiki_Eng_2019-05-23.csv",sep=",",dec=".", header=T)
summary(wiki_en_df)


####Load packages for analysis
library(tidyverse)
library(ggplot2)
library(ggExtra)
library(ggpubr)
library(pscl)
library(MuMIn)


####Merge Wiki_all and PA_EI datasets
merge_pa_df<-inner_join(pa_df,
                     wiki_all_df,
                     by = c("id" = "cod_cnuc"))
head(merge_pa_df)

####Merge PT and EN langviews
# Was this used?
merge_lang_df<-rbind(wiki_pt_df,wiki_en_df)
head(merge_lang_df)
dups<-merge_lang_df[duplicated(merge_lang_df$cod_cnuc),1]
merge_lang_df$dups<-NA

for(i in 1:nrow(merge_lang_df)){
  merge_lang_df$dups[i]<-ifelse(merge_lang_df[i,1] %in% dups,1,0)
  print(i)
}

merge_lang_df$dups<-as.factor(merge_lang_df$dups)
merge_lang_df$lang<-as.factor(merge_lang_df$lang)
summary(merge_lang_df)

####Exploratory plots
#Histogram of average vies per language for PT and EN
p1_pt<-ggplot(wiki_pt_df,
           aes(x = mean))+
       geom_histogram(fill = "red")+
       scale_x_continuous(trans = "log10",
                          name = "Mean views per page")+
       scale_y_continuous(name = "Number of PAs",
                          limits = c(0,40))+
       theme_classic()+
       theme(axis.text = element_text(size = 12,
                                      colour = "black"),
             axis.title = element_text(size = 14))+
       annotate("text",
                x = 0.25,
                y = 40,
                label = "a)",
                size = 5,
                colour = "black")
p1_pt

p1_en<-ggplot(wiki_en_df,
              aes(x = mean))+
  geom_histogram(fill = "blue")+
  scale_x_continuous(trans = "log10",
                     name = "Mean views per page")+
  scale_y_continuous(name = "Number of PAs",
                     limits = c(0,40))+
  theme_classic()+
  theme(axis.text = element_text(size = 12,
                                 colour = "black"),
        axis.title = element_text(size = 14))+
  annotate("text",
           x = 0.25,
           y = 40,
           label = "b)",
           size = 5,
           colour = "black")
p1_en

plot1<-ggarrange(p1_pt, p1_en,
                 ncol = 2,
                 nrow = 1,
                 heights = c(0.85,0.85,1.3))
plot1

#Plot scatter combining data only for PAs with version in both languages
p2<-ggplot(merge_pa_df, aes(x=mean.pt, y=mean.eng))+
    geom_point(colour = "grey")+
    theme_classic()+
    theme(axis.text = element_text(size = 12,
                                 colour = "black"),
          axis.title = element_text(size = 14))+
    scale_x_continuous(trans = "log10",
                       name = "Mean views per page in Portuguese")+
    scale_y_continuous(trans = "log10",
                       name = "Mean views per page in English")+
    geom_hline(yintercept = median(merge_pa_df$mean.eng),
               linetype = "dashed")+
    geom_vline(xintercept = median(merge_pa_df$mean.pt),
               linetype = "dashed")
p2
p2<-ggMarginal(p2, type="boxplot", size = 8, fill="gray")

ggsave("Fig1.tiff",
       p2,
       units = "cm",
       height = 10,
       width = 15,
       dpi = 150)

####Statistical analysis
#Correlation between views in PT and EN with raw values
cor.test(merge_pa_df$mean.pt,merge_pa_df$mean.eng)

#Correlation between views in PT and EN with log values
cor.test(log10(merge_pa_df$mean.pt),log10(merge_pa_df$mean.eng))


#Since views are similar for both PT and EN, lets model the data just with PT views
merge_model_df<-left_join(x = pa_df,
                          y = wiki_all_df,
                          by = c("id" = "cod_cnuc"))
head(merge_model_df)

#Let's use the same variables as used for the EI paper
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
model_df<-merge_model_df[,c(2,5,7,9:10,14,27,31,34,39:45)]
summary(model_df)
model_df<-na.exclude(model_df)
cor(model_df[,c(10:16)])

#First lets make a test of the best distribution
mod1_pt<-hurdle(mean.pt ~ area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+tot_div_mod+year_mod|
               area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+tot_div_mod+year_mod,
             data = model_df,
             dist = "poisson",
             na.action = "na.fail")
mod2_pt<-hurdle(mean.pt ~ area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+tot_div_mod+year_mod|
               area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+tot_div_mod+year_mod,
             data = model_df,
             dist = "negbin",
             na.action = "na.fail")
mod3_pt<-hurdle(mean.pt ~ area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+tot_div_mod+year_mod|
               area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+tot_div_mod+year_mod,
             data = model_df,
             dist = "geometric",
             na.action = "na.fail")
AICc(mod1_pt,mod2_pt,mod3_pt)

#Negative binomial seems to be the best choice based on AICc - Let's keep it and run all model combinations
dd_views_pt <- dredge(mod2_pt,evaluate=TRUE, rank = "AICc", REML = F)
dd_views_pt
gmod_views_pt <- get.models(dd_views_pt,subset = delta < 4)
gmod_views_pt
mod_avg_pt<-model.avg(gmod_views_pt)
summary(mod_avg_pt)

#And now let's try in EN
mod1_eng<-hurdle(mean.eng ~ area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+tot_div_mod+year_mod|
               area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+tot_div_mod+year_mod,
             data = model_df,
             dist = "poisson",
             na.action = "na.fail")
mod2_eng<-hurdle(mean.eng ~ area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+tot_div_mod+year_mod|
               area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+tot_div_mod+year_mod,
             data = model_df,
             dist = "negbin",
             na.action = "na.fail")
mod3_eng<-hurdle(mean.eng ~ area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+tot_div_mod+year_mod|
               area_km2_mod+pop_50k_mod+alt_r_mod+acc_50k_mod+tot_div_mod+year_mod,
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


out <- capture.output(summary(mod_avg_pt))
cat("Hurdle Model PT", out, file='Hurdle_Model_PT.txt', sep = '\n', append = FALSE)

out <- capture.output(summary(mod_avg_eng))
cat("Hurdle Model ENG", out, file='Hurdle_Model_ENG.txt', sep = '\n', append = FALSE)


