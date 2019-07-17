# ---- Head ------------------- 
# Project: Protected Areas Social Interest
#
# Merge Data in Working Dataset
#
# Created in 2019-06-29
#
# Guedes-Santos, J
#
#______________________________

rm(list = ls())

# ---- Load libraries ----
library(tidyverse)

# ---- Load datasets ----
#PA Dataset (CNUC)
pa_dataset <- read_csv("./data/BrazilianProtectedAreas_2019-05-13.csv")
#Correia et al. 2018 Dataset
rc_dataset <- read.table("/media/gaio/Argos/Dropbox/Pesquisa/Doutorado/Qualificação II/Análises Ricardo/PA_data_030416_raw.csv", sep=";", dec=".", header=T, quote = "\"", encoding = "latin1")

#Rename columns
names(pa_dataset)
pa_dataset <- pa_dataset %>%
  rename(
    cod_cnuc = codCnuc,
    pa_name_cnuc = nomeUC,
    category = cat,
    governance_lvl = esfera,
    id_en_wkdata = idEnWikiData,
    en_wkname = enNameWiki,
    en_wkpage = enWikiPage,
    id_pt_wkdata = idPtWikiData,
    pt_wkname = ptNameWiki,
    pt_wkpage = ptWikiPage
  )

names(rc_dataset)
rc_dataset <- rc_dataset %>%
  rename(
    category = cat,
    category_abv = cat2,
    protect_group = group,
    governance_lvl = govern,
    year_establish = year,
    cod_cnuc = id,
    access_50k = acc_50k,
    sp_diversity = tot_div,
    altitude_r = alt_r,
    biome = bioma
    
  )

#Select only columns of interest
pa_dataset <- pa_dataset %>%
  select(
    cod_cnuc, pa_name_cnuc,
    category, governance_lvl,
    id_en_wkdata, en_wkname, en_wkpage,
    id_pt_wkdata, pt_wkname, pt_wkpage
  )

rc_dataset <- rc_dataset %>%
  select(
    biome,
    access_50k,
    pop_50k,
    year_establish,
    sp_diversity,
    altitude_r,
    area_km2,
    protect_group,
    category_abv,
    cod_cnuc
  )

#Merge and maintain all rows
# rc_dataset has less rows than pa_dataset
# columns as altitude, protect_group and others has NA values for most of PAs in pa_dataset
pa_dataset <- left_join(pa_dataset,
                         rc_dataset,
                         by = c("cod_cnuc" = "cod_cnuc"))

#Merge only with existing rows in both datasets
pa_rc_dataset <- inner_join(pa_dataset,
                            rc_dataset,
                            by = c("cod_cnuc" = "cod_cnuc"))

# Next steps
# 1. Log variables

rppn <- pa_rc_dataset %>%
  subset(category == "Reserva Particular do Patrimônio Natural")
rppn

# Get only rows there are in pa_dataset and not in rc_dataset
pa_diff <- anti_join(pa_dataset, rc_dataset, by = "cod_cnuc")

# Get only rows there are in rc_dataset and not in pa_dataset
rc_diff <- anti_join(rc_dataset, pa_dataset, by = "cod_cnuc")

write_csv(rc_diff, "./data/rc_diff.csv")


#Merge only names to dataset ENG Means
eng_means <- read_csv("./data/BPA_Wiki_Eng_2019-06-25.csv")
eng_means_names <- left_join(eng_means,
                             pa_dataset[, c("cod_cnuc", "pa_name_cnuc", "en_wkname")],
                             by = c("cod_cnuc" = "cod_cnuc"))
write_csv(eng_means_names, "./data/ENG_MEANS.csv")

#Merge only names to dataset PT Means
pt_means <- read_csv("./data/BPA_Wiki_Pt_2019-06-25.csv")
pt_means_names <- left_join(pt_means,
                             pa_dataset[, c("cod_cnuc", "pa_name_cnuc", "pt_wkname")],
                             by = c("cod_cnuc" = "cod_cnuc"))
write_csv(pt_means_names, "./data/PT_MEANS.csv")
