#2023-11-14
#Stacked barplots using PhytoClass data

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)

#########################Bioassay 1

#import data

pc_bioassay_1 = read_excel("data/PhytoClass_data.xlsx", sheet = "Bioassay_1")

summary(pc_bioassay_1)
glimpse(pc_bioassay_1)

#figure

B1_community_plot = ggplot(pc_bioassay_1, aes(fill=Group, 
                                                   y=Concentration, 
                                                   x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_bar(position="stack", stat="identity") + 
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill ='Group') +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic()
ggsave(B1_community_plot, filename = "figures/PhytoClass/B1_community_plot.png",
       device = "png", height = 7, width = 11)

#########################Bioassay 2

#import data

pc_bioassay_2 = read_excel("data/PhytoClass_data.xlsx", sheet = "Bioassay_2")

summary(pc_bioassay_2)
glimpse(pc_bioassay_2)

#figure

B2_community_plot = ggplot(pc_bioassay_2, aes(fill=Group, 
                                              y=Concentration, 
                                              x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_bar(position="stack", stat="identity") + 
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill ='Group') +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic()
ggsave(B2_community_plot, filename = "figures/PhytoClass/B2_community_plot.png",
       device = "png", height = 7, width = 11)

#########################Bioassay 3

#import data

pc_bioassay_3 = read_excel("data/PhytoClass_data.xlsx", sheet = "Bioassay_3")

summary(pc_bioassay_3)
glimpse(pc_bioassay_3)

#figure

B3_community_plot = ggplot(pc_bioassay_3, aes(fill=Group, 
                                              y=Concentration, 
                                              x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_bar(position="stack", stat="identity") + 
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill ='Group') +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic()
ggsave(B3_community_plot, filename = "figures/PhytoClass/B3_community_plot.png",
       device = "png", height = 7, width = 11)

#########################Bioassay 4

#import data

pc_bioassay_4 = read_excel("data/PhytoClass_data.xlsx", sheet = "Bioassay_4")

summary(pc_bioassay_4)
glimpse(pc_bioassay_4)

#figure

B4_community_plot = ggplot(pc_bioassay_4, aes(fill=Group, 
                                              y=Concentration, 
                                              x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_bar(position="stack", stat="identity") + 
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill ='Group') +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic()
ggsave(B4_community_plot, filename = "figures/PhytoClass/B4_community_plot.png",
       device = "png", height = 7, width = 11)

#all biossays:

All_Chl_a_bargraph = ggplot(data = all_bioassay, 
                            aes(x = fct_relevel(Treatment, "T_0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=Chl_a)) +
  geom_bar(stat='identity', fill="darkgrey") +
  xlab("Treatment") +
  ylab(expression(paste("Chl a \u03BCg", l^-1))) +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN+LP", "DIN_HP" = "DIN+HP")) +
  facet_wrap(~factor(Bioassay, levels=c('May', 'June', 'July', 'September')),  ncol=2, labeller=variable_labeller) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0)) + 
  ylim(0, 60)
ggsave(All_Chl_a_bargraph, filename = "figures/All_Chl_a_bargraph.png",
       device = "png", height = 7, width = 11)
