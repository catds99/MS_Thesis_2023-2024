#2023-11-14
#Stacked barplots using chemtax data

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)

#########################Bioassay 1

#import data

bioassay_1 = read_excel("data/Chemtax_data.xlsx", sheet = "Bioassay_1_percent_avg")

summary(bioassay_1)
glimpse(bioassay_1)

bioassay_1_avg = bioassay_1 %>%
  group_by(Treatment) %>%
  summarise(mean_percent = mean(Percent))

bioassay_1_avg

Bioassay_1_community_plot_percent = ggplot(data = bioassay_1, 
       aes(fill = Group, 
           y = Percent, 
           x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +  
  geom_bar(position="stack", stat="identity") + 
  ylab("Percentage (%)") + xlab("Treatment") + labs(fill ='Phytoplankton Taxa') +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic()
ggsave(Bioassay_1_community_plot_percent, filename = "figures/Bioassay_1_community_plot_percent.png",
       device = "png", height = 7, width = 11)


###Bioassay 2

bioassay_2 = read_excel("data/Chemtax_data.xlsx", sheet = "Bioassay_2_percent_avg")

summary(bioassay_2)
glimpse(bioassay_2)

Bioassay_2_community_plot_percent = ggplot(data = bioassay_2, 
                                           aes(fill = Group, 
                                               y = Percent, 
                                               x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +  
  geom_bar(position="stack", stat="identity") + 
  ylab("Percentage (%)") + xlab("Treatment") + labs(fill ='Phytoplankton Taxa') +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic()
ggsave(Bioassay_2_community_plot_percent, filename = "figures/Bioassay_2_community_plot_percent.png",
       device = "png", height = 7, width = 11)

###Bioassay 3

bioassay_3 = read_excel("data/Chemtax_data.xlsx", sheet = "Bioassay_3_percent_avg")

summary(bioassay_3)
glimpse(bioassay_3)


Bioassay_3_community_plot_percent = ggplot(data = bioassay_3, 
                                           aes(fill = Group, 
                                               y = Percent, 
                                               x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +  
  geom_bar(position="stack", stat="identity") + 
  ylab("Percentage (%)") + xlab("Treatment") + labs(fill ='Phytoplankton Taxa') +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic()
ggsave(Bioassay_3_community_plot_percent, filename = "figures/Bioassay_3_community_plot_percent.png",
       device = "png", height = 7, width = 11)

###Bioassay 4

bioassay_4 = read_excel("data/Chemtax_data.xlsx", sheet = "Bioassay_4_percent_avg")

summary(bioassay_4)
glimpse(bioassay_4)

Bioassay_4_community_plot_percent = ggplot(data = bioassay_4, 
                                           aes(fill = Group, 
                                               y = Percent, 
                                               x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +  
  geom_bar(position="stack", stat="identity") + 
  ylab("Percentage (%)") + xlab("Treatment") + labs(fill ='Phytoplankton Taxa') +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic()
ggsave(Bioassay_4_community_plot_percent, filename = "figures/Bioassay_4_community_plot_percent.png",
       device = "png", height = 7, width = 11)


