#2023-07-31
#CDS

#practice analyzing bioassay 3

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

bioassay_3 = read_excel("data/2023-07-06_bioassay_3.xlsx", sheet = "Chl")

summary(bioassay_3)
glimpse(bioassay_3)

#find average per treatment

bioassay_3_avg = bioassay_3 %>%
  group_by(Other) %>%
  summarise(mean_chl = mean(ALL_Chl_a))

bioassay_3_avg

#create figure - bar graph based on average per treatment 

bioassay_3_fig_1 = ggplot() +
  geom_col(aes(x = fct_relevel(Other, "T_0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = mean_chl),
           fill = "darkgrey",
           data = bioassay_3_avg) +
  xlab("Treatment") +
  ylab(expression(paste("Chl a \u03BCg", l^-1))) +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 12) +
  ylim(0, 60)
ggsave(bioassay_3_fig_1, filename = "figures/bioassay_3_fig_1.png",
       device = "png", height = 7, width = 11)

#anova to compare treatments

leveneTest(bioassay_3$ALL_Chl_a ~ bioassay_3$Other)

oneway.test(ALL_Chl_a ~ Other,
            data = bioassay_3,
            var.equal = FALSE)

bioassay_3_post_hoc = games_howell_test(formula = ALL_Chl_a ~ Other,
                                        data = bioassay_3, 
                                        conf.level = 0.95, 
                                        detailed = FALSE)

print(bioassay_3_post_hoc, n = 22)

#####################################################################################

#net changes

bioassay_3.2 = read_excel("data/bioassay_3.2.xlsx", sheet = 2)

bioassay_3_fig_2 = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "T_0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Chl_a_change_t0),
           fill = "darkgrey",
           data = bioassay_3.2) +
  xlab("Treatment (compared to T=0)") +
  ylab(expression(paste("Chl a \u03BCg", l^-1))) +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic() 
ggsave(bioassay_3_fig_2, filename = "figures/bioassay_3_fig_2.png",
       device = "png", height = 7, width = 11)

bioassay_3_fig_3 = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "T_0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Chl_a_change_control),
           fill = "darkgrey",
           data = bioassay_3.2) +
  xlab("Treatment (compared to control)") +
  ylab(expression(paste("Chl a \u03BCg", l^-1))) +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic() 
ggsave(bioassay_3_fig_3, filename = "figures/bioassay_3_fig_3.png",
       device = "png", height = 7, width = 11)

#################################################################

#boxplots

bioassay_3_fig_4 = ggplot() +
  geom_boxplot(aes(x = fct_relevel(Other, "T_0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                   y = ALL_Chl_a),
               fill = "darkgrey",
               data = bioassay_3) +
  xlab("Treatment") +
  ylab(expression(paste("Chl a \u03BCg", l^-1))) +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic() 
ggsave(bioassay_3_fig_4, filename = "figures/bioassay_3_fig_4.png",
       device = "png", height = 7, width = 11)