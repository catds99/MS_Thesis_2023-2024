#2023-12-22
#algal group percent difference ANOVAs with PhytoClass data

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

###############################################################Total chl a

#Kruskal-wallace:

kruskal_total = kruskal.test(Total_Chl_a ~ Treatment, data = pc_percent_change)

#ANOVA try 1:

total_chl_a_percent_change_ANOVA = aov(Total_Chl_a ~ Treatment, data = pc_percent_change)

summary(total_chl_a_percent_change_ANOVA)

#ANOVA try 2: 

total_chl_a_percent_change_ANOVA_2 = oneway.test(Total_Chl_a ~ Treatment,
                                            data = pc_percent_change,
                                            var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Total_Chl_a ~ Treatment,
            data = pc_percent_change,
            var.equal = FALSE)

#REGW

post_hoc_1_total_chl_a = REGW.test(y = total_chl_a_percent_change_ANOVA, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_1_total_chl_a)

#Games-Howell:

total_chl_a_percent_change_GH = games_howell_test(formula = Total_Chl_a ~ Treatment,
                                             data = pc_percent_change, 
                                             conf.level = 0.95, 
                                             detailed = FALSE)

print(total_chl_a_percent_change_GH, n = 22)

#Tukey post-hoc testing (parametric)

total_chl_a_percent_change_Tukey = tukey_hsd(total_chl_a_percent_change_ANOVA, conf.level = 0.95)

TukeyHSD(total_chl_a_percent_change_ANOVA)

#Check for normality

model_2 = lm(Total_Chl_a ~ Treatment, data = pc_percent_change)

ggqqplot(residuals(model_2))

shapiro_test(residuals(model_2))

#check for homogeneity of variances

leveneTest(pc_percent_change$Total_Chl_a ~ pc_percent_change$Treatment)

###############################################################Cyanobacteria

#Kruskal-wallis:

kruskal_cyanobacteria = kruskal.test(Cyanobacteria ~ Treatment, data = pc_percent_change)

#ANOVA try 1:

Cyanobacteria_percent_change_ANOVA = aov(Cyanobacteria ~ Treatment, data = pc_percent_change)

summary(Cyanobacteria_percent_change_ANOVA)

#ANOVA try 2: 

Cyanobacteria_percent_change_ANOVA_2 = oneway.test(Cyanobacteria ~ Treatment,
                                                 data = pc_percent_change,
                                                 var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Cyanobacteria ~ Treatment,
            data = pc_percent_change,
            var.equal = FALSE)

#REGW

post_hoc_1_Cyanobacteria = REGW.test(y = Cyanobacteria_percent_change_ANOVA, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_1_Cyanobacteria)

#Games-Howell:

Cyanobacteria_percent_change_GH = games_howell_test(formula = Cyanobacteria ~ Treatment,
                                                  data = pc_percent_change, 
                                                  conf.level = 0.95, 
                                                  detailed = FALSE)

print(Cyanobacteria_percent_change_GH, n = 22)

#Tukey post-hoc testing (parametric)

Cyanobacteria_percent_change_Tukey = tukey_hsd(Cyanobacteria_percent_change_ANOVA, conf.level = 0.95)

TukeyHSD(Cyanobacteria_percent_change_ANOVA)

#Check for normality

model_3 = lm(Cyanobacteria ~ Treatment, data = pc_percent_change)

ggqqplot(residuals(model_3))

shapiro_test(residuals(model_3))

#check for homogeneity of variances

leveneTest(pc_percent_change$Cyanobacteria ~ pc_percent_change$Treatment)

###############################################################Green Algae

#Kruskal-wallis:

kruskal_Green_Algae = kruskal.test(Green_Algae ~ Treatment, data = pc_percent_change)

#ANOVA try 1:

Green_Algae_percent_change_ANOVA = aov(Green_Algae ~ Treatment, data = pc_percent_change)

summary(Green_Algae_percent_change_ANOVA)

#ANOVA try 2: 

Green_Algae_percent_change_ANOVA_2 = oneway.test(Green_Algae ~ Treatment,
                                                   data = pc_percent_change,
                                                   var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Green_Algae ~ Treatment,
            data = pc_percent_change,
            var.equal = FALSE)

#REGW

post_hoc_1_Green_Algae = REGW.test(y = Green_Algae_percent_change_ANOVA, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_1_Green_Algae)

#Games-Howell:

Green_Algae_percent_change_GH = games_howell_test(formula = Green_Algae ~ Treatment,
                                                    data = pc_percent_change, 
                                                    conf.level = 0.95, 
                                                    detailed = FALSE)

print(Green_Algae_percent_change_GH, n = 22)

#Tukey post-hoc testing (parametric)

Green_Algae_percent_change_Tukey = tukey_hsd(Green_Algae_percent_change_ANOVA, conf.level = 0.95)

TukeyHSD(Green_Algae_percent_change_ANOVA)

#Check for normality

model_4 = lm(Green_Algae ~ Treatment, data = pc_percent_change)

ggqqplot(residuals(model_4))

shapiro_test(residuals(model_4))

#check for homogeneity of variances

leveneTest(pc_percent_change$Green_Algae ~ pc_percent_change$Treatment)

###############################################################Cryptophytes

#Kruskal-wallis:

kruskal_Cryptophytes = kruskal.test(Cryptophytes ~ Treatment, data = pc_percent_change)

#ANOVA try 1:

Cryptophytes_percent_change_ANOVA = aov(Cryptophytes ~ Treatment, data = pc_percent_change)

summary(Cryptophytes_percent_change_ANOVA)

#ANOVA try 2: 

Cryptophytes_percent_change_ANOVA_2 = oneway.test(Cryptophytes ~ Treatment,
                                                 data = pc_percent_change,
                                                 var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Cryptophytes ~ Treatment,
            data = pc_percent_change,
            var.equal = FALSE)

#REGW

post_hoc_1_Cryptophytes = REGW.test(y = Cryptophytes_percent_change_ANOVA, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_1_Cryptophytes)

#Games-Howell:

Cryptophytes_percent_change_GH = games_howell_test(formula = Cryptophytes ~ Treatment,
                                                  data = pc_percent_change, 
                                                  conf.level = 0.95, 
                                                  detailed = FALSE)

print(Cryptophytes_percent_change_GH, n = 22)

#Tukey post-hoc testing (parametric)

Cryptophytes_percent_change_Tukey = tukey_hsd(Cryptophytes_percent_change_ANOVA, conf.level = 0.95)

TukeyHSD(Cryptophytes_percent_change_ANOVA)

#Check for normality

model_5 = lm(Cryptophytes ~ Treatment, data = pc_percent_change)

ggqqplot(residuals(model_5))

shapiro_test(residuals(model_5))

#check for homogeneity of variances

leveneTest(pc_percent_change$Cryptophytes ~ pc_percent_change$Treatment)

###############################################################Diatoms

#Kruskal-wallis:

kruskal_Diatoms = kruskal.test(Diatoms ~ Treatment, data = pc_percent_change)

#ANOVA try 1:

Diatoms_percent_change_ANOVA = aov(Diatoms ~ Treatment, data = pc_percent_change)

summary(Diatoms_percent_change_ANOVA)

#ANOVA try 2: 

Diatoms_percent_change_ANOVA_2 = oneway.test(Diatoms ~ Treatment,
                                                  data = pc_percent_change,
                                                  var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Diatoms ~ Treatment,
            data = pc_percent_change,
            var.equal = FALSE)

#REGW

post_hoc_1_Diatoms = REGW.test(y = Diatoms_percent_change_ANOVA, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_1_Diatoms)

#Games-Howell:

Diatoms_percent_change_GH = games_howell_test(formula = Diatoms ~ Treatment,
                                                   data = pc_percent_change, 
                                                   conf.level = 0.95, 
                                                   detailed = FALSE)

print(Diatoms_percent_change_GH, n = 22)

#Tukey post-hoc testing (parametric)

Diatoms_percent_change_Tukey = tukey_hsd(Diatoms_percent_change_ANOVA, conf.level = 0.95)

TukeyHSD(Diatoms_percent_change_ANOVA)

#Check for normality

model_6 = lm(Diatoms ~ Treatment, data = pc_percent_change)

ggqqplot(residuals(model_6))

shapiro_test(residuals(model_6))

#check for homogeneity of variances

leveneTest(pc_percent_change$Diatoms ~ pc_percent_change$Treatment)

###############################################################Dinoflagellates

#Kruskal-wallis:

kruskal_Dinoflagellates = kruskal.test(Dinoflagellates ~ Treatment, data = pc_percent_change)

#ANOVA try 1:

Dinoflagellates_percent_change_ANOVA = aov(Dinoflagellates ~ Treatment, data = pc_percent_change)

summary(Dinoflagellates_percent_change_ANOVA)

#ANOVA try 2: 

Dinoflagellates_percent_change_ANOVA_2 = oneway.test(Dinoflagellates ~ Treatment,
                                             data = pc_percent_change,
                                             var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Dinoflagellates ~ Treatment,
            data = pc_percent_change,
            var.equal = FALSE)

#REGW

post_hoc_1_Dinoflagellates = REGW.test(y = Dinoflagellates_percent_change_ANOVA, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_1_Dinoflagellates)

#Games-Howell:

Dinoflagellates_percent_change_GH = games_howell_test(formula = Dinoflagellates ~ Treatment,
                                              data = pc_percent_change, 
                                              conf.level = 0.95, 
                                              detailed = FALSE)

print(Dinoflagellates_percent_change_GH, n = 22)

#Tukey post-hoc testing (parametric)

Dinoflagellates_percent_change_Tukey = tukey_hsd(Dinoflagellates_percent_change_ANOVA, conf.level = 0.95)

TukeyHSD(Dinoflagellates_percent_change_ANOVA)

#Check for normality

model_7 = lm(Dinoflagellates ~ Treatment, data = pc_percent_change)

ggqqplot(residuals(model_7))

shapiro_test(residuals(model_7))

#check for homogeneity of variances

leveneTest(pc_percent_change$Dinoflagellates ~ pc_percent_change$Treatment)

###############################################################HAptophytes

#Kruskal-wallis:

kruskal_Haptophytes = kruskal.test(Haptophytes ~ Treatment, data = pc_percent_change)

#ANOVA try 1:

Haptophytes_percent_change_ANOVA = aov(Haptophytes ~ Treatment, data = pc_percent_change)

summary(Haptophytes_percent_change_ANOVA)

#ANOVA try 2: 

Haptophytes_percent_change_ANOVA_2 = oneway.test(Haptophytes ~ Treatment,
                                                     data = pc_percent_change,
                                                     var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Haptophytes ~ Treatment,
            data = pc_percent_change,
            var.equal = FALSE)

#REGW

post_hoc_1_Haptophytes = REGW.test(y = Haptophytes_percent_change_ANOVA, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_1_Haptophytes)

#Games-Howell:

Haptophytes_percent_change_GH = games_howell_test(formula = Haptophytes ~ Treatment,
                                                      data = pc_percent_change, 
                                                      conf.level = 0.95, 
                                                      detailed = FALSE)

print(Haptophytes_percent_change_GH, n = 22)

#Tukey post-hoc testing (parametric)

Haptophytes_percent_change_Tukey = tukey_hsd(Haptophytes_percent_change_ANOVA, conf.level = 0.95)

TukeyHSD(Haptophytes_percent_change_ANOVA)

#Check for normality

model_8 = lm(Haptophytes ~ Treatment, data = pc_percent_change)

ggqqplot(residuals(model_8))

shapiro_test(residuals(model_8))

#check for homogeneity of variances

leveneTest(pc_percent_change$Haptophytes ~ pc_percent_change$Treatment)
