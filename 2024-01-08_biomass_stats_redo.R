#2024-01-09
#redo individual bioassay total chl a biomass ANOVAs

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

#import data

B1 = read_excel("data/all_bioassays_percents.xlsx", sheet = "B1")

summary(B1)
glimpse(B1)

B2 = read_excel("data/all_bioassays_percents.xlsx", sheet = "B2")

summary(B2)
glimpse(B2)

B3 = read_excel("data/all_bioassays_percents.xlsx", sheet = "B3")

summary(B3)
glimpse(B3)

B4 = read_excel("data/all_bioassays_percents.xlsx", sheet = "B4")

summary(B4)
glimpse(B4)

######################B1

#Check for normality

model_B1 = lm(Concentration ~ Treatment, data = B1)

ggqqplot(residuals(model_B1))

shapiro_test(residuals(model_B1))

#check for homogeneity of variances

leveneTest(B1$Concentration ~ B1$Treatment)

#ANOVAs

B1_aov = aov(Concentration ~ Treatment, data = B1)

summary(B1_aov)

oneway.test(Concentration ~ Treatment,
            data = B1,
            var.equal = FALSE)

B1_kruskal = kruskal.test(Concentration ~ Treatment, data = B1)

B1_kruskal

#Post hoc

B1_REGW = REGW.test(y = B1_aov, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(B1_REGW)

B1_GH = games_howell_test(formula = Concentration ~ Treatment,
                          data = B1, 
                          conf.level = 0.95, 
                          detailed = FALSE)

print(B1_GH, n = 22)

B1_Tukey = tukey_hsd(B1_aov, conf.level = 0.95)

print(B1_Tukey, n = 22)

######################B2

#Check for normality

model_B2 = lm(Concentration ~ Treatment, data = B2)

ggqqplot(residuals(model_B2))

shapiro_test(residuals(model_B2))

#check for homogeneity of variances

leveneTest(B2$Concentration ~ B2$Treatment)

#ANOVAs

B2_aov = aov(Concentration ~ Treatment, data = B2)

summary(B2_aov)

oneway.test(Concentration ~ Treatment,
            data = B2,
            var.equal = FALSE)

B2_kruskal = kruskal.test(Concentration ~ Treatment, data = B2)

B2_kruskal

#Post hoc

B2_REGW = REGW.test(y = B2_aov, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(B2_REGW)

B2_GH = games_howell_test(formula = Concentration ~ Treatment,
                          data = B2, 
                          conf.level = 0.95, 
                          detailed = FALSE)

print(B2_GH, n = 22)

B2_Tukey = tukey_hsd(B2_aov, conf.level = 0.95)

print(B2_Tukey, n = 22)

######################B3

#Check for normality

model_B3 = lm(Concentration ~ Treatment, data = B3)

ggqqplot(residuals(model_B3))

shapiro_test(residuals(model_B3))

#check for homogeneity of variances

leveneTest(B3$Concentration ~ B3$Treatment)

#ANOVAs

B3_aov = aov(Concentration ~ Treatment, data = B3)

summary(B3_aov)

oneway.test(Concentration ~ Treatment,
            data = B3,
            var.equal = FALSE)

B3_kruskal = kruskal.test(Concentration ~ Treatment, data = B3)

B3_kruskal

#Post hoc

B3_REGW = REGW.test(y = B3_aov, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(B3_REGW)

B3_GH = games_howell_test(formula = Concentration ~ Treatment,
                          data = B3, 
                          conf.level = 0.95, 
                          detailed = FALSE)

print(B3_GH, n = 22)

B3_Tukey = tukey_hsd(B3_aov, conf.level = 0.95)

print(B3_Tukey, n = 22)

######################B4

#Check for normality

model_B4 = lm(Concentration ~ Treatment, data = B4)

ggqqplot(residuals(model_B4))

shapiro_test(residuals(model_B4))

#check for homogeneity of variances

leveneTest(B4$Concentration ~ B4$Treatment)

#ANOVAs

B4_aov = aov(Concentration ~ Treatment, data = B4)

summary(B4_aov)

oneway.test(Concentration ~ Treatment,
            data = B4,
            var.equal = FALSE)

B4_kruskal = kruskal.test(Concentration ~ Treatment, data = B4)

B4_kruskal

#Post hoc

B4_REGW = REGW.test(y = B4_aov, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(B4_REGW)

B4_GH = games_howell_test(formula = Concentration ~ Treatment,
                          data = B4, 
                          conf.level = 0.95, 
                          detailed = FALSE)

print(B4_GH, n = 22)

B4_Tukey = tukey_hsd(B4_aov, conf.level = 0.95)

print(B4_Tukey, n = 22)

###########################################transformed

######################B1

#Check for normality

model_B1_ln = lm(ln_concentration ~ Treatment, data = B1)

ggqqplot(residuals(model_B1_ln))

shapiro_test(residuals(model_B1_ln))

#check for homogeneity of variances

leveneTest(B1$ln_concentration ~ B1$Treatment)

#ANOVAs

B1_ln_aov = aov(ln_concentration ~ Treatment, data = B1)

summary(B1_ln_aov)

#Post hoc

B1_ln_REGW = REGW.test(y = B1_ln_aov, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(B1_ln_REGW)

######################B2

#Check for normality

model_B2_ln = lm(ln_concentration ~ Treatment, data = B2)

ggqqplot(residuals(model_B2_ln))

shapiro_test(residuals(model_B2_ln))

#check for homogeneity of variances

leveneTest(B2$ln_concentration ~ B2$Treatment)

#ANOVAs

B2_ln_aov = aov(ln_concentration ~ Treatment, data = B2)

summary(B2_ln_aov)

#Post hoc

B2_ln_REGW = REGW.test(y = B2_ln_aov, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(B2_ln_REGW)

######################B3

#Check for normality

model_B3_ln = lm(ln_concentration ~ Treatment, data = B3)

ggqqplot(residuals(model_B3_ln))

shapiro_test(residuals(model_B3_ln))

#check for homogeneity of variances

leveneTest(B3$ln_concentration ~ B3$Treatment)

#ANOVAs

B3_ln_aov = aov(ln_concentration ~ Treatment, data = B3)

summary(B3_ln_aov)

#Post hoc

B3_ln_REGW = REGW.test(y = B3_ln_aov, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(B3_ln_REGW)

######################B4

#Check for normality

model_B4_ln = lm(ln_concentration ~ Treatment, data = B4)

ggqqplot(residuals(model_B4_ln))

shapiro_test(residuals(model_B4_ln))

#check for homogeneity of variances

leveneTest(B4$ln_concentration ~ B4$Treatment)

#ANOVAs

B4_ln_aov = aov(ln_concentration ~ Treatment, data = B4)

summary(B4_ln_aov)

#Post hoc

B4_ln_REGW = REGW.test(y = B4_ln_aov, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(B4_ln_REGW)
