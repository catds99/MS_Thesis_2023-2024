#graphing percent change in chl a relative to control
#2023-10-16

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)

######################## Bioassay 1

#import data:

bioassay_1.2 = read_excel("data/bioassay_1.2.xlsx", sheet = 5)

glimpse(bioassay_1.2)

bioassay_1_fig_5 = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = percent_change_relative_to_control),
           fill = "darkgrey",
           data = bioassay_1.2) +
  xlab("Treatment") +
  ylab("Percent Change in Chl a") +
  scale_x_discrete(labels= c("Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 25) +
  ylim(-5, 1500)
ggsave(bioassay_1_fig_5, filename = "figures/bioassay_1_fig_5.png",
       device = "png", height = 7, width = 11)

######################## Bioassay 2

#import data:

bioassay_2.2 = read_excel("data/bioassay_2.2.xlsx", sheet = 3)

glimpse(bioassay_2.2)

bioassay_2_fig_5 = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = percent_change_relative_to_control),
           fill = "darkgrey",
           data = bioassay_2.2) +
  xlab("Treatment") +
  ylab("Percent Change in Chl a") +
  scale_x_discrete(labels= c("Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 25) +
  ylim(-5, 1500)
ggsave(bioassay_2_fig_5, filename = "figures/bioassay_2_fig_5.png",
       device = "png", height = 7, width = 11)

######################## Bioassay 3

#import data:

bioassay_3.2 = read_excel("data/bioassay_3.2.xlsx", sheet = 3)

glimpse(bioassay_3.2)

bioassay_3_fig_5 = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = percent_change_relative_to_control),
           fill = "darkgrey",
           data = bioassay_3.2) +
  xlab("Treatment") +
  ylab("Percent Change in Chl a") +
  scale_x_discrete(labels= c("Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 25) +
  ylim(-5, 1500)
ggsave(bioassay_3_fig_5, filename = "figures/bioassay_3_fig_5.png",
       device = "png", height = 7, width = 11)

######################## Bioassay 4

#import data:

bioassay_4.2 = read_excel("data/bioassay_4.2.xlsx", sheet = 3)

glimpse(bioassay_4.2)

bioassay_4_fig_6 = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = percent_change_relative_to_control),
           fill = "darkgrey",
           data = bioassay_4.2) +
  xlab("Treatment") +
  ylab("Percent Change in Chl a") +
  scale_x_discrete(labels= c("Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 25) +
  ylim(-5, 1500)
ggsave(bioassay_4_fig_6, filename = "figures/bioassay_4_fig_6.png",
       device = "png", height = 7, width = 11)

