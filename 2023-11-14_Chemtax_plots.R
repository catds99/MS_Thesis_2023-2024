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

bioassay_1 = read_excel("data/Chemtax_data.xlsx", sheet = "Bioassay_1_conc_avg")

summary(bioassay_1)
glimpse(bioassay_1)

#figure

Bioassay_1_community_plot = ggplot(bioassay_1, aes(fill=Group, 
                                                   y=Concentration, 
                                                   x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_bar(position="stack", stat="identity") + 
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill ='Group') +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic()
Bioassay_1_community_plot
ggsave(Bioassay_1_community_plot, filename = "figures/Bioassay_1_community_plot.png",
       device = "png", height = 7, width = 11)


#########################Bioassay 2

#import data

bioassay_2 = read_excel("data/Chemtax_data.xlsx", sheet = "Bioassay_2_conc_avg")

summary(bioassay_2)
glimpse(bioassay_2)

#figure

Bioassay_2_community_plot = ggplot(bioassay_2, aes(fill=Group, 
                                                   y=Concentration, 
                                                   x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_bar(position="stack", stat="identity") + 
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill ='Group') +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic()
Bioassay_2_community_plot
ggsave(Bioassay_2_community_plot, filename = "figures/Bioassay_2_community_plot.png",
       device = "png", height = 7, width = 11)

#########################Bioassay 3

#import data

bioassay_3 = read_excel("data/Chemtax_data.xlsx", sheet = "Bioassay_3_conc_avg")

summary(bioassay_3)
glimpse(bioassay_3)

#figure

Bioassay_3_community_plot = ggplot(bioassay_3, aes(fill=Group, 
                                                   y=Concentration, 
                                                   x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_bar(position="stack", stat="identity") + 
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill ='Group') +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic()
Bioassay_3_community_plot
ggsave(Bioassay_3_community_plot, filename = "figures/Bioassay_3_community_plot.png",
       device = "png", height = 7, width = 11)

#########################Bioassay 4

#import data

bioassay_4 = read_excel("data/Chemtax_data.xlsx", sheet = "Bioassay_4_conc_avg")

summary(bioassay_4)
glimpse(bioassay_4)

#figure

Bioassay_4_community_plot = ggplot(bioassay_4, aes(fill=Group, 
                                                   y=Concentration, 
                                                   x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_bar(position="stack", stat="identity") + 
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill ='Group') +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic()
Bioassay_4_community_plot
ggsave(Bioassay_4_community_plot, filename = "figures/Bioassay_4_community_plot.png",
       device = "png", height = 7, width = 11)


