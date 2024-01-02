#graphing percent of control chl a 
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

bioassay_1_percent_of_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = percent_of_control),
           fill = "darkgrey",
           data = bioassay_1.2) +
  xlab("Treatment") +
  ylab("Percent of Control (Chl a)") +
  scale_x_discrete(labels= c("Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 20) +
  ylim(0, 1500)
ggsave(bioassay_1_percent_of_control, filename = "figures/bioassay_1_percent_of_control.png",
       device = "png", height = 7, width = 11)

######################## Bioassay 2

#import data:

bioassay_2.2 = read_excel("data/bioassay_2.2.xlsx", sheet = 3)

glimpse(bioassay_2.2)

bioassay_2_percent_of_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = percent_of_control),
           fill = "darkgrey",
           data = bioassay_2.2) +
  xlab("Treatment") +
  ylab("Percent of Control (Chl a)") +
  scale_x_discrete(labels= c("Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 20) +
  ylim(0, 1600)
ggsave(bioassay_2_percent_of_control, filename = "figures/bioassay_2_percent_of_control.png",
       device = "png", height = 7, width = 11)

######################## Bioassay 3

#import data:

bioassay_3.2 = read_excel("data/bioassay_3.2.xlsx", sheet = 3)

glimpse(bioassay_3.2)

bioassay_3_percent_of_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = percent_of_control),
           fill = "darkgrey",
           data = bioassay_3.2) +
  xlab("Treatment") +
  ylab("Percent of Control (Chl a)") +
  scale_x_discrete(labels= c("Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 20) +
  ylim(0, 1600)
ggsave(bioassay_3_percent_of_control, filename = "figures/bioassay_3_percent_of_control.png",
       device = "png", height = 7, width = 11)

######################## Bioassay 4

#import data:

bioassay_4.2 = read_excel("data/bioassay_4.2.xlsx", sheet = 3)

glimpse(bioassay_4.2)

bioassay_4_percent_of_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = percent_of_control),
           fill = "darkgrey",
           data = bioassay_4.2) +
  xlab("Treatment") +
  ylab("Percent of Control (Chl a)") +
  scale_x_discrete(labels= c("Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 20) +
  ylim(0, 1600)
ggsave(bioassay_4_percent_of_control, filename = "figures/bioassay_4_percent_of_control.png",
       device = "png", height = 7, width = 11)

