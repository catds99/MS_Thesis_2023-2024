#2023-11-14
#Stacked barplots using PhytoClass data

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)
library(ggpubr)
theme_set(theme_pubr())

#######import percent change community data

pc_percent_change = read_excel("data/PhytoClass_data.xlsx", sheet = "percent_change")

summary(pc_percent_change)
glimpse(pc_percent_change)

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
  theme_classic(base_size = 14)
ggsave(B1_community_plot, filename = "figures/PhytoClass/B1_community_plot.png",
       device = "png", height = 7, width = 11)

#figure for percent change

pc_percent_change_b1 = read_excel("data/PhytoClass_data.xlsx", sheet = "Bioassay_1_percent_change")

summary(pc_percent_change_b1)
glimpse(pc_percent_change_b1)

B1_community_percent_change_plot_2 = ggplot(pc_percent_change_b1, aes(fill=Group, 
                                 y=Percent_Change, 
                                 x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept=c(1.5, 2.5, 3.5, 4.5), linetype="solid", color = "black", linewidth = 0.5) +
  ylab("Percent Change") + 
  xlab("Treatment") + 
  labs(fill ='Group') +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14)
ggsave(B1_community_percent_change_plot_2, filename = "figures/PhytoClass/B1_community_percent_change_plot_2.png",
       device = "png", height = 7, width = 11)

B1_community_percent_change_plot = ggplot(pc_percent_change_b1, aes(fill=Group, 
                          y=Percent_Change, 
                          x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_bar(position="stack", stat="identity") + 
  geom_hline(yintercept=0, linetype="dashed", color = "black", linewidth = 1) +
  ylab("Percent Change") + 
  xlab("Treatment") + 
  labs(fill ='Group') +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14)
ggsave(B1_community_percent_change_plot, filename = "figures/PhytoClass/B1_community_percent_change_plot.png",
       device = "png", height = 7, width = 11)

#combined figure:

b1_community_comp = ggarrange(B1_community_plot, B1_community_percent_change_plot_2,
                    labels = c("A", "B"),
                    nrow = 2)

ggsave(b1_community_comp, filename = "figures/PhytoClass/b1_community_comp.png",
       device = "png", height = 10, width = 11)

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
  theme_classic(base_size = 14)
ggsave(B2_community_plot, filename = "figures/PhytoClass/B2_community_plot.png",
       device = "png", height = 7, width = 11)



#figure for percent change

pc_percent_change_b2 = read_excel("data/PhytoClass_data.xlsx", sheet = "Bioassay_2_percent_change")

summary(pc_percent_change_b2)
glimpse(pc_percent_change_b2)

B2_community_percent_change_plot = ggplot(pc_percent_change_b2, aes(fill=Group, 
                                                                    y=Percent_Change, 
                                                                    x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_bar(position="stack", stat="identity") + 
  geom_hline(yintercept=0, linetype="dashed", color = "black", linewidth = 1) +
  ylab("Percent Change") + 
  xlab("Treatment") + 
  labs(fill ='Group') +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14)
ggsave(B2_community_percent_change_plot, filename = "figures/PhytoClass/B2_community_percent_change_plot.png",
       device = "png", height = 7, width = 11)

B2_community_percent_change_plot_2 = ggplot(pc_percent_change_b2, aes(fill=Group, 
                                                                      y=Percent_Change, 
                                                                      x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept=c(1.5, 2.5, 3.5, 4.5), linetype="solid", color = "black", linewidth = 0.5) +
  ylab("Percent Change") + 
  xlab("Treatment") + 
  labs(fill ='Group') +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14)
ggsave(B2_community_percent_change_plot_2, filename = "figures/PhytoClass/B2_community_percent_change_plot_2.png",
       device = "png", height = 7, width = 11)

#combined figure:

b2_community_comp = ggarrange(B2_community_plot, B2_community_percent_change_plot_2,
                              labels = c("A", "B"),
                              nrow = 2)

ggsave(b2_community_comp, filename = "figures/PhytoClass/b2_community_comp.png",
       device = "png", height = 10, width = 11)

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
  theme_classic(base_size = 14)
ggsave(B3_community_plot, filename = "figures/PhytoClass/B3_community_plot.png",
       device = "png", height = 7, width = 11)

#figure for percent change

pc_percent_change_b3 = read_excel("data/PhytoClass_data.xlsx", sheet = "Bioassay_3_percent_change")

summary(pc_percent_change_b3)
glimpse(pc_percent_change_b3)

B3_community_percent_change_plot = ggplot(pc_percent_change_b3, aes(fill=Group, 
                                                                    y=Percent_Change, 
                                                                    x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_bar(position="stack", stat="identity") + 
  geom_hline(yintercept=0, linetype="dashed", color = "black", linewidth = 1) +
  ylab("Percent Change") + 
  xlab("Treatment") + 
  labs(fill ='Group') +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14)
ggsave(B3_community_percent_change_plot, filename = "figures/PhytoClass/B3_community_percent_change_plot.png",
       device = "png", height = 7, width = 11)

B3_community_percent_change_plot_2 = ggplot(pc_percent_change_b3, aes(fill=Group, 
                                                                      y=Percent_Change, 
                                                                      x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept=c(1.5, 2.5, 3.5, 4.5), linetype="solid", color = "black", linewidth = 0.5) +
  ylab("Percent Change") + 
  xlab("Treatment") + 
  labs(fill ='Group') +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14)
ggsave(B3_community_percent_change_plot_2, filename = "figures/PhytoClass/B3_community_percent_change_plot_2.png",
       device = "png", height = 7, width = 11)

#combined figure:

b3_community_comp = ggarrange(B3_community_plot, B3_community_percent_change_plot_2,
                              labels = c("A", "B"),
                              nrow = 2)

ggsave(b3_community_comp, filename = "figures/PhytoClass/b3_community_comp.png",
       device = "png", height = 10, width = 11)

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
  theme_classic(base_size = 14)
ggsave(B4_community_plot, filename = "figures/PhytoClass/B4_community_plot.png",
       device = "png", height = 7, width = 11)

#figure for percent change

pc_percent_change_b4 = read_excel("data/PhytoClass_data.xlsx", sheet = "Bioassay_4_percent_change")

summary(pc_percent_change_b4)
glimpse(pc_percent_change_b4)

B4_community_percent_change_plot = ggplot(pc_percent_change_b4, aes(fill=Group, 
                                                                    y=Percent_Change, 
                                                                    x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_bar(position="stack", stat="identity") + 
  geom_hline(yintercept=0, linetype="dashed", color = "black", linewidth = 1) +
  ylab("Percent Change") + 
  xlab("Treatment") + 
  labs(fill ='Group') +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14)
ggsave(B4_community_percent_change_plot, filename = "figures/PhytoClass/B4_community_percent_change_plot.png",
       device = "png", height = 7, width = 11)

B4_community_percent_change_plot_2 = ggplot(pc_percent_change_b4, aes(fill=Group, 
                                                                      y=Percent_Change, 
                                                                      x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept=c(1.5, 2.5, 3.5, 4.5), linetype="solid", color = "black", linewidth = 0.5) +
  ylab("Percent Change") + 
  xlab("Treatment") + 
  labs(fill ='Group') +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14)
ggsave(B4_community_percent_change_plot_2, filename = "figures/PhytoClass/B4_community_percent_change_plot_2.png",
       device = "png", height = 7, width = 11)

#combined figure:

b4_community_comp = ggarrange(B4_community_plot, B4_community_percent_change_plot_2,
                              labels = c("A", "B"),
                              nrow = 2)

ggsave(b4_community_comp, filename = "figures/PhytoClass/b4_community_comp.png",
       device = "png", height = 10, width = 11)

#all biossays:

all_bioassay = read_excel("data/2023-11-17_All_Bioassay_Chl_a.xlsx", sheet = "Sheet1")

summary(all_bioassay)
glimpse(all_bioassay)

comm_comp_all_bioassay = read_excel("data/PhytoClass_data.xlsx", sheet = "stack")

summary(comm_comp_all_bioassay)
glimpse(comm_comp_all_bioassay)

variable_names = list("May" = "A", "June" = "B", "July" = "C", "September" = "D")

variable_labeller = function(variable,value){
  return(variable_names[value])
}

All_comm_comp_bargraph = ggplot(comm_comp_all_bioassay, aes(fill=Group, 
                                                   y=Concentration, 
                                                   x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +
  geom_bar(position="stack", stat="identity") + 
  xlab("Treatment") +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  labs(fill ='Group') +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN+LP", "DIN_HP" = "DIN+HP")) +
  facet_wrap(~factor(Bioassay, levels=c('May', 'June', 'July', 'September')),  ncol=2, labeller=variable_labeller) +
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0)) + 
  ylim(0, 60)
ggsave(All_comm_comp_bargraph, filename = "figures/All_comm_comp_bargraph.png",
       device = "png", height = 7, width = 11)
