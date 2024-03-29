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

bioassay_2_PAM = read_excel("data/2023-06-08_bioassay_2.xlsx", sheet = "PAM")

summary(bioassay_2_PAM)
glimpse(bioassay_2_PAM)

#find average per treatment

bioassay_2_PAM_avg = bioassay_2_PAM %>%
  group_by(Other) %>%
  summarise(mean_FvFm = mean(FvFm))

bioassay_2_PAM_avg

#boxplot5

bioassay_2_fig_5 = ggplot() +
  geom_boxplot(aes(x = fct_relevel(Other, "T_0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                   y = FvFm),
               fill = "darkgrey",
               data = bioassay_2_PAM) +
  xlab("Treatment") +
  ylab(expression(F[v]/F[m])) +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic() 
ggsave(bioassay_2_fig_5, filename = "figures/bioassay_2_fig_5.png",
       device = "png", height = 7, width = 11)

#anova to compare treatments

leveneTest(bioassay_2_PAM$FvFm ~ bioassay_2_PAM$Other)

oneway.test(FvFm ~ Other,
            data = bioassay_2_PAM,
            var.equal = TRUE)

oneway.test(FvFm ~ Other,
            data = bioassay_2_PAM,
            var.equal = FALSE)

bioassay_2_PAM_ANOVA = aov(FvFm~Other, data=bioassay_2_PAM)

bioassay_2_PAM_ANOVA

summary(bioassay_2_PAM_ANOVA)

bioassay_2_Tukey_PAM = tukey_hsd(bioassay_2_PAM_ANOVA, conf.level = 0.95)

print(bioassay_2_Tukey_PAM, n = 25)

bioassay_2_post_hoc = games_howell_test(formula = FvFm ~ Other,
                                        data = bioassay_2_PAM, 
                                        conf.level = 0.95, 
                                        detailed = FALSE)

print(bioassay_2_post_hoc, n = 22)

shapiro.test(bioassay_2_PAM$FvFm)

#import transformed data

PAM = read_excel("data/2023-11-17_All_Bioassay_PAM.xlsx", sheet = "all")

summary(PAM)
glimpse(PAM)

PAM_2 = PAM[, 1-35]

shapiro.test(PAM_2$Fv_Fm)

shapiro.test(PAM_2$ln_FvFm)

shapiro.test(PAM_2$inverse)

ks.test(PAM_2$Fv_Fm, PAM_2$Treatment)

leveneTest(bioassay_2_PAM$FvFm ~ bioassay_2_PAM$Other)

