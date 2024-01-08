#2024-01-08
#redo PAM stats

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)

#import data

PAM_2 = read_excel("data/2023-11-17_All_Bioassay_PAM.xlsx", sheet = "2_transformed")

glimpse(PAM_2)

PAM_3 = read_excel("data/2023-11-17_All_Bioassay_PAM.xlsx", sheet = "3_transformed")

PAM_4 = read_excel("data/2023-11-17_All_Bioassay_PAM.xlsx", sheet = "4_transformed")

######################normality and hom of variances

#B2

shapiro.test(PAM_2$Fv_Fm)

shapiro.test(PAM_2$ln_FvFm)

shapiro.test(PAM_2$inverse)

ks.test(PAM_2$Fv_Fm, PAM_2$Treatment = "T_0")

leveneTest(PAM_2$Fv_Fm ~ PAM_2$Treatment)

#B3

shapiro.test(PAM_3$Fv_Fm)

shapiro.test(PAM_3$ln_FvFm)

shapiro.test(PAM_3$inverse)

leveneTest(PAM_3$Fv_Fm ~ PAM_3$Treatment)

#B4

shapiro.test(PAM_4$Fv_Fm)

shapiro.test(PAM_4$ln_FvFm)

shapiro.test(PAM_4$inverse)

leveneTest(PAM_4$Fv_Fm ~ PAM_4$Treatment)


###########################################ANOVA

#B2

oneway.test(Fv_Fm ~ Treatment,
            data = PAM_2,
            var.equal = TRUE)

oneway.test(Fv_Fm ~ Treatment,
            data = PAM_2,
            var.equal = FALSE)

bioassay_2_PAM_ANOVA = aov(Fv_Fm~Treatment, data=PAM_2)

bioassay_2_PAM_ANOVA

summary(bioassay_2_PAM_ANOVA)

bioassay_2_Tukey_PAM = tukey_hsd(bioassay_2_PAM_ANOVA, conf.level = 0.95)

print(bioassay_2_Tukey_PAM, n = 25)

bioassay_2_post_hoc = games_howell_test(formula = Fv_Fm ~ Treatment,
                                        data = PAM_2, 
                                        conf.level = 0.95, 
                                        detailed = FALSE)

print(bioassay_2_post_hoc, n = 22)

regw_b2 = REGW.test(y = bioassay_2_PAM_ANOVA, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(regw_b2)

#B3

bioassay_3_PAM_ANOVA = aov(Fv_Fm~Treatment, data=PAM_3)

summary(bioassay_3_PAM_ANOVA)

oneway.test(Fv_Fm ~ Treatment,
            data = PAM_3,
            var.equal = FALSE)

bioassay_3_Tukey_PAM = tukey_hsd(bioassay_3_PAM_ANOVA, conf.level = 0.95)

print(bioassay_3_Tukey_PAM, n = 25)

bioassay_3_post_hoc = games_howell_test(formula = Fv_Fm ~ Treatment,
                                        data = PAM_3, 
                                        conf.level = 0.95, 
                                        detailed = FALSE)

print(bioassay_3_post_hoc, n = 22)

regw_b3 = REGW.test(y = bioassay_3_PAM_ANOVA, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(regw_b3)

#B4

bioassay_4_PAM_ANOVA = aov(Fv_Fm~Treatment, data=PAM_4)

summary(bioassay_4_PAM_ANOVA)

oneway.test(Fv_Fm ~ Treatment,
            data = PAM_4,
            var.equal = FALSE)

bioassay_4_Tukey_PAM = tukey_hsd(bioassay_4_PAM_ANOVA, conf.level = 0.95)

print(bioassay_4_Tukey_PAM, n = 25)

bioassay_4_post_hoc = games_howell_test(formula = Fv_Fm ~ Treatment,
                                        data = PAM_4, 
                                        conf.level = 0.95, 
                                        detailed = FALSE)

print(bioassay_4_post_hoc, n = 22)

regw_b4 = REGW.test(y = bioassay_4_PAM_ANOVA, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(regw_b4)
