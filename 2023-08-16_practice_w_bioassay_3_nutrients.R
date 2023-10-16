#2023-08-16
#practice with bioassay 3 nutrients

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)

install.packages("ggpubr")
library(ggpubr)

#import data

bioassay_3_nut = read_excel("data/bioassay_3_nutrients.xlsx")

summary(bioassay_3_nut)
glimpse(bioassay_3_nut)

#find average per treatment

bioassay_3_nut_avg = bioassay_3_nut %>%
  group_by(Sample_ID) %>%
  summarise(mean_NO3 = mean(NO3), mean_NO2 = mean(NO2), mean_NH3 = mean(NH3), mean_PO4 = mean(PO4)) 

bioassay_3_nut_avg

#bar graph:

bioassay_3_NO3 = ggplot() +
  geom_boxplot(aes(x = fct_relevel(Sample_ID, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                   y = NO3),
               fill = "darkgrey",
               data = bioassay_3_nut) +
  xlab("Treatment") +
  ylab(expression(paste("Concentration of NO3 (\u03BCM)"))) +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic() 
ggsave(bioassay_3_NO3, filename = "nutrient_figures/bioassay_3_NO3.png",
       device = "png", height = 7, width = 11)

bioassay_3_NO2 = ggplot() +
  geom_boxplot(aes(x = fct_relevel(Sample_ID, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                   y = NO2),
               fill = "darkgrey",
               data = bioassay_3_nut) +
  xlab("Treatment") +
  ylab(expression(paste("Concentration of NO2 (\u03BCM)"))) +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic() 
ggsave(bioassay_3_NO2, filename = "nutrient_figures/bioassay_3_NO2.png",
       device = "png", height = 7, width = 11)

bioassay_3_NH4 = ggplot() +
  geom_boxplot(aes(x = fct_relevel(Sample_ID, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                   y = NH3),
               fill = "darkgrey",
               data = bioassay_3_nut) +
  xlab("Treatment") +
  ylab(expression(paste("Concentration of NH4 (\u03BCM)"))) +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic() 
ggsave(bioassay_3_NH4, filename = "nutrient_figures/bioassay_3_NH4.png",
       device = "png", height = 7, width = 11)

bioassay_3_PO4 = ggplot() +
  geom_boxplot(aes(x = fct_relevel(Sample_ID, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                   y = PO4),
               fill = "darkgrey",
               data = bioassay_3_nut) +
  xlab("Treatment") +
  ylab(expression(paste("Concentration of PO4 (\u03BCM)"))) +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic() 
ggsave(bioassay_3_PO4, filename = "nutrient_figures/bioassay_3_PO4.png",
       device = "png", height = 7, width = 11)