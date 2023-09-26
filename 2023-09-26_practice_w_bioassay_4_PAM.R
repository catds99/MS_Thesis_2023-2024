#2023-008-22
#practice with PAM data - Bioassay 2 (06/06-08/23)

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)

#import data

bioassay_4_PAM = read_excel("data/2023-09-03_bioassay_4.xlsx", sheet = "PAM")

summary(bioassay_4_PAM)
glimpse(bioassay_4_PAM)

#find average per treatment

bioassay_4_PAM_avg = bioassay_4_PAM %>%
  group_by(Other) %>%
  summarise(mean_FvFm = mean(FvFm))

bioassay_4_PAM_avg

#boxplot5

bioassay_4_fig_5 = ggplot() +
  geom_boxplot(aes(x = fct_relevel(Other, "T_0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                   y = FvFm),
               fill = "darkgrey",
               data = bioassay_4_PAM) +
  xlab("Treatment") +
  ylab(expression(F[v]/F[m])) +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic() 
ggsave(bioassay_4_fig_5, filename = "figures/bioassay_4_fig_5.png",
       device = "png", height = 7, width = 11)

#anova to compare treatments

leveneTest(bioassay_4_PAM$FvFm ~ bioassay_4_PAM$Other)

oneway.test(FvFm ~ Other,
            data = bioassay_4_PAM,
            var.equal = TRUE)

oneway.test(FvFm ~ Other,
            data = bioassay_4_PAM,
            var.equal = FALSE)

bioassay_4_PAM_ANOVA = aov(FvFm~Other, data=bioassay_4_PAM)

bioassay_4_PAM_ANOVA

bioassay_4_Tukey_PAM = tukey_hsd(bioassay_4_PAM_ANOVA, conf.level = 0.95)

print(bioassay_4_Tukey_PAM, n = 25)

bioassay_4_post_hoc = games_howell_test(formula = FvFm ~ Other,
                                        data = bioassay_4_PAM, 
                                        conf.level = 0.95, 
                                        detailed = FALSE)

print(bioassay_4_post_hoc, n = 22)
