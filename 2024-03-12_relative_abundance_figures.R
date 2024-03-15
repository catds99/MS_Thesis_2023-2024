#2024-03-12
#relative abundance figures

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)
library(ggpubr)
theme_set(theme_pubr())




# import data

rel_data = read_excel("data/2024-03-11_relative_abundance.xlsx", sheet = "Sheet2")

glimpse(rel_data)

group_colors = c("Cyanobacteria" = "aquamarine3", "Cryptophytes" = "lightpink3", "Diatoms" = "lightgoldenrod", "Dinoflagellates" = "darkgoldenrod", "Green Algae" = "chartreuse4", "Haptophytes" = "chocolate4")

b1_relative_abundance = ggplot(rel_data, aes(fill = Group, y = Bioassay_1, x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = group_colors) +
  ylab("Percent of Community") + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 30)

ggsave(b1_biomass, filename = "figures/b1_community_biomass.png",
       device = "png", height = 12, width = 15)







rel_data_2 = read_excel("data/2024-03-11_relative_abundance.xlsx", sheet = "Sheet4")

glimpse(rel_data_2)

rel_data_2$Group = factor(rel_data_2$Group,
                          levels = c("Cyanobacteria", "Cryptophytes", "Diatoms", "Dinoflagellates", "Green_Algae", "dh"),
                          labels = c("Cyanobacteria", "Cryptophytes", "Diatoms", "Dinoflagellates", "Green Algae", "Diatoms/Haptophytes"))

group_colors_2 = c("Cyanobacteria" = "aquamarine3", "Cryptophytes" = "lightpink3", "Diatoms/Haptophytes" = "lightgoldenrod", "Dinoflagellates" = "darkgoldenrod", "Green Algae" = "chartreuse4")

b2_percent_comp = ggplot(rel_data_2, aes(fill = Group, y = Bioassay_2, x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = group_colors_2) +
  ylab("Percent of Community") + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 30)

ggsave(b2_percent_comp, filename = "figures/b2_percent_comp.png",
       device = "png", height = 12, width = 15)




b3_percent_comp = ggplot(rel_data_2, aes(fill = Group, y = Bioassay_3, x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = group_colors_2) +
  ylab("Percent of Community") + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 30)

ggsave(b3_percent_comp, filename = "figures/b3_percent_comp.png",
       device = "png", height = 12, width = 15)



b4_percent_comp = ggplot(rel_data_2, aes(fill = Group, y = Bioassay_4, x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = group_colors_2) +
  ylab("Percent of Community") + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 30)

ggsave(b4_percent_comp, filename = "figures/b4_percent_comp.png",
       device = "png", height = 12, width = 15)
