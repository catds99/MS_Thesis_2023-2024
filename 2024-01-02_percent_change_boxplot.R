#percent Change boxplots

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)
library(ggpubr)
library(broom)
library(agricolae)

pc_percent_change = read_excel("data/PhytoClass_data.xlsx", sheet = "percent_change")

summary(pc_percent_change)
glimpse(pc_percent_change)

bioassay_1_fig_4 = ggplot() +
  geom_boxplot(aes(x = fct_relevel(Other, "T_0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                   y = ALL_Chl_a),
               fill = "darkgrey",
               data = bioassay_1) +
  xlab("Treatment") +
  ylab(expression(paste("Chl a \u03BCg", l^-1))) +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic() 
ggsave(bioassay_1_fig_4, filename = "figures/bioassay_1_fig_4.png",
       device = "png", height = 7, width = 11)
               