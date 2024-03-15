#2024-03-12 
#new community composition figures with diatoms and haptophytes combined

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)
library(ggpubr)
theme_set(theme_pubr())


####### import data

biomass_dh = read_excel("data/2024-03-06_data_for_stacked_plots.xlsx", sheet = "Biomass_dh")

percent_change_dh = read_excel("data/2024-03-06_data_for_stacked_plots.xlsx", sheet = "Percent_Change_dh")

biomass_dh$Group <- factor(biomass_dh$Group,
                        levels = c("Cyanobacteria", "Cryptophytes", "Dinoflagellates", "Green_Algae", "dh"),
                        labels = c("Cyanobacteria", "Cryptophytes", "Dinoflagellates", "Green Algae", "Diatoms/Haptophytes"))

percent_change_dh$Group <- factor(percent_change_dh$Group,
                           levels = c("Cyanobacteria", "Cryptophytes", "Dinoflagellates", "Green_Algae", "dh"),
                           labels = c("Cyanobacteria", "Cryptophytes", "Dinoflagellates", "Green Algae", "Diatoms/Haptophytes"))

group_colors = c("Cyanobacteria" = "aquamarine3", "Cryptophytes" = "lightpink3", "Diatoms/Haptophytes" = "lightgoldenrod", "Dinoflagellates" = "darkgoldenrod", "Green Algae" = "chartreuse4")


######################################### Bioassay 1

######### biomass plot

b1_biomass_dh = ggplot(biomass_dh, aes(fill = Group, y = Bioassay_1, x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = group_colors) +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 30)

ggsave(b1_biomass_dh, filename = "figures/b1_community_biomass_dh.png",
       device = "png", height = 12, width = 15)


######### percent change plot offset

b1_percent_change_2_dh = ggplot(percent_change_dh, aes(fill = Group, y = Bioassay_1, x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +
  geom_bar(position = position_dodge(), stat = "identity") +
  scale_fill_manual(values = group_colors) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", linewidth = 1) +
  geom_vline(xintercept=c(1.5, 2.5, 3.5, 4.5), linetype="solid", color = "black", linewidth = 0.5) +
  ylab("Percent Change") + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 30)

ggsave(b1_percent_change_2_dh, filename = "figures/b1_community_percent_change_2_dh.png",
       device = "png", height = 12, width = 15)

######### combined figure

b1_comm_comp_dh = ggarrange(b1_biomass_dh, b1_percent_change_2_dh,
                         labels = c("A", "B"),
                         nrow = 2)

ggsave(b1_comm_comp_dh, filename = "figures/b1_comm_comp_dh.png",
       device = "png", height = 12, width = 17)




######################################### Bioassay 2

######### biomass plot

b2_biomass_dh = ggplot(biomass_dh, aes(fill = Group, y = Bioassay_2, x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = group_colors) +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 30)

ggsave(b2_biomass_dh, filename = "figures/b2_community_biomass_dh.png",
       device = "png", height = 12, width = 15)


######### percent change plot offset

b2_percent_change_2_dh = ggplot(percent_change_dh, aes(fill = Group, y = Bioassay_2, x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +
  geom_bar(position = position_dodge(), stat = "identity") +
  scale_fill_manual(values = group_colors) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", linewidth = 1) +
  geom_vline(xintercept=c(1.5, 2.5, 3.5, 4.5), linetype="solid", color = "black", linewidth = 0.5) +
  ylab("Percent Change") + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 30)

ggsave(b2_percent_change_2_dh, filename = "figures/b2_community_percent_change_2_dh.png",
       device = "png", height = 12, width = 15)

######### combined figure

b2_comm_comp_dh = ggarrange(b2_biomass_dh, b2_percent_change_2_dh,
                            labels = c("A", "B"),
                            nrow = 2)

ggsave(b2_comm_comp_dh, filename = "figures/b2_comm_comp_dh.png",
       device = "png", height = 12, width = 17)










######################################### Bioassay 3

######### biomass plot

b3_biomass_dh = ggplot(biomass_dh, aes(fill = Group, y = Bioassay_3, x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = group_colors) +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 30)

ggsave(b3_biomass_dh, filename = "figures/b3_community_biomass_dh.png",
       device = "png", height = 12, width = 15)


######### percent change plot offset

b3_percent_change_2_dh = ggplot(percent_change_dh, aes(fill = Group, y = Bioassay_3, x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +
  geom_bar(position = position_dodge(), stat = "identity") +
  scale_fill_manual(values = group_colors) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", linewidth = 1) +
  geom_vline(xintercept=c(1.5, 2.5, 3.5, 4.5), linetype="solid", color = "black", linewidth = 0.5) +
  ylab("Percent Change") + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 30)

ggsave(b3_percent_change_2_dh, filename = "figures/b3_community_percent_change_2_dh.png",
       device = "png", height = 12, width = 15)

######### combined figure

b3_comm_comp_dh = ggarrange(b3_biomass_dh, b3_percent_change_2_dh,
                            labels = c("A", "B"),
                            nrow = 2)

ggsave(b3_comm_comp_dh, filename = "figures/b3_comm_comp_dh.png",
       device = "png", height = 12, width = 17)









######################################### Bioassay 4

######### biomass plot

b4_biomass_dh = ggplot(biomass_dh, aes(fill = Group, y = Bioassay_4, x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = group_colors) +
  ylab(expression(paste("Biomass \u03BCg", l^-1))) + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 30)

ggsave(b4_biomass_dh, filename = "figures/b4_community_biomass_dh.png",
       device = "png", height = 12, width = 15)


######### percent change plot offset

b4_percent_change_2_dh = ggplot(percent_change_dh, aes(fill = Group, y = Bioassay_4, x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) +
  geom_bar(position = position_dodge(), stat = "identity") +
  scale_fill_manual(values = group_colors) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", linewidth = 1) +
  geom_vline(xintercept=c(1.5, 2.5, 3.5, 4.5), linetype="solid", color = "black", linewidth = 0.5) +
  ylab("Percent Change") + 
  xlab("Treatment") + 
  labs(fill = 'Group') +
  scale_x_discrete(labels = c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 30)

ggsave(b4_percent_change_2_dh, filename = "figures/b4_community_percent_change_2_dh.png",
       device = "png", height = 12, width = 15)

######### combined figure

b4_comm_comp_dh = ggarrange(b1_biomass_dh, b4_percent_change_2_dh,
                            labels = c("A", "B"),
                            nrow = 2)

ggsave(b4_comm_comp_dh, filename = "figures/b4_comm_comp_dh.png",
       device = "png", height = 12, width = 17)
