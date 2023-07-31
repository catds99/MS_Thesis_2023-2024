#2023-07-31
#practice with PAM data - Bioassay 3 (07/04-06/23)

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)

#import data

bioassay_3_PAM = read_excel("data/2023-07-06_bioassay_3.xlsx", sheet = "PAM")

summary(bioassay_3_PAM)
glimpse(bioassay_3_PAM)

#find average per treatment

bioassay_3_PAM_avg = bioassay_3_PAM %>%
  group_by(Other) %>%
  summarise(mean_FvFm = mean(FvFm))

bioassay_3_PAM_avg

#boxplot5

bioassay_3_fig_5 = ggplot() +
  geom_boxplot(aes(x = fct_relevel(Other, "T_0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                   y = FvFm),
               fill = "darkgrey",
               data = bioassay_3_PAM) +
  xlab("Treatment") +
  ylab(expression(F[v]/F[m])) +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic() 
ggsave(bioassay_3_fig_5, filename = "figures/bioassay_3_fig_5.png",
       device = "png", height = 7, width = 11)