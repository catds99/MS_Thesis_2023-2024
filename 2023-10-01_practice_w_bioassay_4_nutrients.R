#2023-10-04
#practice with bioassay 2=4 nutrients

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)

install.packages("ggpubr")
library(ggpubr)

#import data - normal

bioassay_4_nut_normal = read_excel("data/bioassay_4_nutrients.xlsx", sheet = "Normal")

summary(bioassay_4_nut_normal)
glimpse(bioassay_4_nut_normal)

#import data - acid preserved

bioassay_4_nut_ap = read_excel("data/bioassay_4_nutrients.xlsx", sheet = "Acid_Preserved")

summary(bioassay_4_nut_ap)
glimpse(bioassay_4_nut_ap)

#find average per treatment - normal

bioassay_4_nut_normal_avg = bioassay_4_nut_normal %>%
  group_by(Sample_ID) %>%
  summarise(mean_NO3 = mean(NO3), mean_NO2 = mean(NO2), mean_NH3 = mean(NH3), mean_PO4 = mean(PO4)) 

bioassay_4_nut_normal_avg

#find average per treatment - acid preserved

bioassay_4_nut_ap_avg = bioassay_4_nut_ap %>%
  group_by(Sample_ID) %>%
  summarise(mean_NO3 = mean(NO3), mean_NO2 = mean(NO2), mean_NH3 = mean(NH3), mean_PO4 = mean(PO4)) 

bioassay_4_nut_ap_avg

#bar graph:

bioassay_4_NO2 = ggplot() +
  geom_boxplot(aes(x = fct_relevel(Sample_ID, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                   y = NO2),
               fill = "darkgrey",
               data = bioassay_4_nut_normal) +
  xlab("Treatment") +
  ylab(expression(paste("Concentration of NO2 (\u03BCM)"))) +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic() 
ggsave(bioassay_4_NO2, filename = "nutrient_figures/bioassay_4_NO2.png",
       device = "png", height = 7, width = 11)

bioassay_4_NH4_normal = ggplot() +
  geom_boxplot(aes(x = fct_relevel(Sample_ID, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                   y = NH3),
               fill = "darkgrey",
               data = bioassay_4_nut_normal) +
  xlab("Treatment (non-acid preserved samples)") +
  ylab(expression(paste("Concentration of NH4 (\u03BCM)"))) +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic() 
ggsave(bioassay_4_NH4_normal, filename = "nutrient_figures/bioassay_4_NH4_normal.png",
       device = "png", height = 7, width = 11)

bioassay_4_NH4_ap = ggplot() +
  geom_boxplot(aes(x = fct_relevel(Sample_ID, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                   y = NH3),
               fill = "darkgrey",
               data = bioassay_4_nut_ap) +
  xlab("Treatment (acid preserved samples)") +
  ylab(expression(paste("Concentration of NH4 (\u03BCM)"))) +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic() 
ggsave(bioassay_4_NH4_ap, filename = "nutrient_figures/bioassay_4_NH4_ap.png",
       device = "png", height = 7, width = 11)

bioassay_4_NO3 = ggplot() +
  geom_boxplot(aes(x = fct_relevel(Sample_ID, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                   y = NO3),
               fill = "darkgrey",
               data = bioassay_4_nut_normal) +
  xlab("Treatment") +
  ylab(expression(paste("Concentration of NO3 (\u03BCM)"))) +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic() 
ggsave(bioassay_4_NO3, filename = "nutrient_figures/bioassay_4_NO3.png",
       device = "png", height = 7, width = 11)

bioassay_4_PO4 = ggplot() +
  geom_boxplot(aes(x = fct_relevel(Sample_ID, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                   y = PO4),
               fill = "darkgrey",
               data = bioassay_4_nut_normal) +
  xlab("Treatment") +
  ylab(expression(paste("Concentration of PO4 (\u03BCM)"))) +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic() 
ggsave(bioassay_4_PO4, filename = "nutrient_figures/bioassay_4_PO4.png",
       device = "png", height = 7, width = 11)


