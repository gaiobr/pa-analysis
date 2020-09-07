# PLOT GRAPH --------------------------------------------------------------
####Clear R memory
rm(list=ls())

####Set working directory
setwd('F:/GAIO/github/pa-analysis/')

library(ggplot2)
library(tidyverse)

today = Sys.Date()
#biome = 'Amazonia'
biome = 'Mata-Atlantica'

# COUNT MODEL -------------------------------------------------------------

df_pt <- read.table(paste0("./data/Hurdle Average Confit PT_", biome ,"_2020-09-07.csv"), sep=",", dec=".", header=T) %>% 
  arrange(desc(order)) %>% filter(type == 'count')
df_eng <- read.table(paste0("./data/Hurdle Average Confit ENG_", biome ,"_2020-09-07.csv"), sep=",", dec=".", header=T) %>% 
  arrange(desc(order)) %>% filter(type == 'count')

df_pt$var <- factor(df_pt$var, levels=unique(as.character(df_pt$var)) )
df_eng$var <- factor(df_eng$var, levels=unique(as.character(df_eng$var)) )

df_pt$lang <- 'pt'
df_eng$lang <- 'en'

model <- rbind(df_pt, df_eng)

(
  p_model <- ggplot(model, aes(var, estimate)) +
  geom_pointrange(
    aes(ymin = inferior, ymax = superior, color = lang),
    position = position_dodge(0.5)
    ) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype="dotted") +
  xlab('Model Parameters') +
  ylab('Estimate') +
  labs(
    title = "Count Model",
    subtitle = "Portuguese and English"
  ) +
  theme(legend.position = "right") +
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


svg(paste0("./figures/PA_PT_ENG_Count_Model_", biome, '_', today,".svg"))
p_model
dev.off()
png(paste0("./figures/PA_PT_ENG_Count_Model_", biome, '_', today,".png"))
p_model
dev.off()


# ZERO MODEL --------------------------------------------------------------

# The values are exactly the same for English and Portuguese, so we need only one of them

df_pt <- read.table(paste0("./data/Hurdle Average Confit PT_", biome ,"_2020-09-07.csv"), sep=",", dec=".", header=T) %>% 
  arrange(desc(order)) %>% filter(type == 'zero')
# df_eng <- read.table(paste0("./data/Hurdle Average Confit ENG_", biome ,"_2020-09-07.csv"), sep=",", dec=".", header=T) %>% 
#   arrange(desc(order)) %>% filter(type == 'zero')

df_pt$var <- factor(df_pt$var, levels=unique(as.character(df_pt$var)) )
#df_eng$var <- factor(df_eng$var, levels=unique(as.character(df_eng$var)) )

df_pt$lang <- 'pt'
#df_eng$lang <- 'en'

model <- df_pt
#model <- rbind(df_pt, df_eng)

(
  p_model <- ggplot(model, aes(var, estimate)) +
  geom_pointrange(
    aes(ymin = inferior, ymax = superior, color = lang)
  ) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype="dotted") +
  xlab('Model Parameters') +
  ylab('Estimate') +
  labs(
    title = "Zero Model",
    subtitle = "Portuguese and English"
  ) +
  theme(legend.position = "none") +
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

svg(paste0("./figures/PA_PT_ENG_Zero_Model_", biome, '_', today,".svg"))
p_model
dev.off()
png(paste0("./figures/PA_PT_ENG_Zero_Model_", biome, '_', today,".png"))
p_model
dev.off()
