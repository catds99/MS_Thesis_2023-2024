#2023-12-13
#more anovas by bioassay

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)
library(ggpubr)
library(broom)

#######################################################Bioassay1

#import data

bioassay1_data = read_excel("data/data_for_anova.xlsx", sheet = "bioassay1")

summary(bioassay1_data)
glimpse(bioassay1_data)

###############total_Chl_a

#ANOVA try 1:
  
total_chl_a_ANOVA_b1.1 = aov(Total_Chl_a ~ Treatment, data = bioassay1_data)

summary(total_chl_a_ANOVA_b1.1)

#ANOVA try 2: 

total_chl_a_ANOVA_b1.2 = oneway.test(Total_Chl_a ~ Treatment,
                                            data = bioassay1_data,
                                            var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Total_Chl_a ~ Treatment,
            data = bioassay1_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_total_chl_1 = kruskal.test(Total_Chl_a ~ Treatment, data = bioassay1_data)

#Tukey post-hoc testing (parametric)

total_chl_Tukey_b1 = tukey_hsd(total_chl_a_ANOVA_b1.1, conf.level = 0.95)

TukeyHSD(total_chl_a_ANOVA_b1.1)

#REGW

post_hoc_total_b1 = REGW.test(y = total_chl_a_ANOVA_b1.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_total_b1)

#Games-Howell:

total_chl_GH_b1 = games_howell_test(formula = Total_Chl_a ~ Treatment,
                                             data = bioassay1_data, 
                                             conf.level = 0.95, 
                                             detailed = FALSE)

print(total_chl_GH_b1, n = 22)

#Check for normality

model_1.1 = lm(Total_Chl_a ~ Treatment, data = bioassay1_data)

ggqqplot(residuals(model_1.1))

shapiro_test(residuals(model_1.1))

#check for homogeneity of variances

leveneTest(bioassay1_data$Total_Chl_a ~ bioassay1_data$Treatment)

############### Chlorophytes

#ANOVA try 1:

Chlorophytes_ANOVA_b1.1 = aov(Chlorophytes ~ Treatment, data = bioassay1_data)

summary(Chlorophytes_ANOVA_b1.1)

#ANOVA try 2: 

Chlorophytes_ANOVA_b1.2 = oneway.test(Chlorophytes ~ Treatment,
                                     data = bioassay1_data,
                                     var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Chlorophytes ~ Treatment,
            data = bioassay1_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Chlorophytes_1 = kruskal.test(Chlorophytes ~ Treatment, data = bioassay1_data)

#Tukey post-hoc testing (parametric)

Chlorophytes_Tukey_b1 = tukey_hsd(Chlorophytes_ANOVA_b1.1, conf.level = 0.95)

TukeyHSD(Chlorophytes_ANOVA_b1.1)

#REGW

post_hoc_chloro_b1 = REGW.test(y = Chlorophytes_ANOVA_b1.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_chloro_b1)

#Games-Howell:

Chlorophytes_GH_b1 = games_howell_test(formula = Chlorophytes ~ Treatment,
                                    data = bioassay1_data, 
                                    conf.level = 0.95, 
                                    detailed = FALSE)

print(Chlorophytes_GH_b1, n = 22)

#Check for normality

model_1.2 = lm(Chlorophytes ~ Treatment, data = bioassay1_data)

ggqqplot(residuals(model_1.2))

shapiro_test(residuals(model_1.2))

#check for homogeneity of variances

leveneTest(bioassay1_data$Chlorophytes ~ bioassay1_data$Treatment)

############### Cryptophytes

#ANOVA try 1:

Cryptophytes_ANOVA_b1.1 = aov(Cryptophytes ~ Treatment, data = bioassay1_data)

summary(Cryptophytes_ANOVA_b1.1)

#ANOVA try 2: 

Cryptophytes_ANOVA_b1.2 = oneway.test(Cryptophytes ~ Treatment,
                                      data = bioassay1_data,
                                      var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Cryptophytes ~ Treatment,
            data = bioassay1_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Cryptophytes_1 = kruskal.test(Cryptophytes ~ Treatment, data = bioassay1_data)

#Tukey post-hoc testing (parametric)

Cryptophytes_Tukey_b1 = tukey_hsd(Cryptophytes_ANOVA_b1.1, conf.level = 0.95)

TukeyHSD(Cryptophytes_ANOVA_b1.1)

#REGW

post_hoc_crypto_b1 = REGW.test(y = Cryptophytes_ANOVA_b1.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_crypto_b1)

#Games-Howell:

Cryptophytes_GH_b1 = games_howell_test(formula = Cryptophytes ~ Treatment,
                                       data = bioassay1_data, 
                                       conf.level = 0.95, 
                                       detailed = FALSE)

print(Cryptophytes_GH_b1, n = 22)

#Check for normality

model_1.3 = lm(Cryptophytes ~ Treatment, data = bioassay1_data)

ggqqplot(residuals(model_1.3))

shapiro_test(residuals(model_1.3))

#check for homogeneity of variances

leveneTest(bioassay1_data$Cryptophytes ~ bioassay1_data$Treatment)

############### Cyanobacteria

#ANOVA try 1:

Cyanobacteria_ANOVA_b1.1 = aov(Cyanobacteria ~ Treatment, data = bioassay1_data)

summary(Cyanobacteria_ANOVA_b1.1)

#ANOVA try 2: 

Cyanobacteria_ANOVA_b1.2 = oneway.test(Cyanobacteria ~ Treatment,
                                      data = bioassay1_data,
                                      var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Cyanobacteria ~ Treatment,
            data = bioassay1_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Cyanobacteria_1 = kruskal.test(Cyanobacteria ~ Treatment, data = bioassay1_data)

#Tukey post-hoc testing (parametric)

Cyanobacteria_Tukey_b1 = tukey_hsd(Cyanobacteria_ANOVA_b1.1, conf.level = 0.95)

TukeyHSD(Cyanobacteria_ANOVA_b1.1)

#REGW

post_hoc_cyano_b1 = REGW.test(y = Cyanobacteria_ANOVA_b1.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_cyano_b1)

#Games-Howell:

Cyanobacteria_GH_b1 = games_howell_test(formula = Cyanobacteria ~ Treatment,
                                       data = bioassay1_data, 
                                       conf.level = 0.95, 
                                       detailed = FALSE)

print(Cyanobacteria_GH_b1, n = 22)

#Check for normality

model_1.4 = lm(Cyanobacteria ~ Treatment, data = bioassay1_data)

ggqqplot(residuals(model_1.4))

shapiro_test(residuals(model_1.4))

#check for homogeneity of variances

leveneTest(bioassay1_data$Cyanobacteria ~ bioassay1_data$Treatment)

############### Diatoms

#ANOVA try 1:

Diatoms_ANOVA_b1.1 = aov(Diatoms ~ Treatment, data = bioassay1_data)

summary(Diatoms_ANOVA_b1.1)

#ANOVA try 2: 

Diatoms_ANOVA_b1.2 = oneway.test(Diatoms ~ Treatment,
                                       data = bioassay1_data,
                                       var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Diatoms ~ Treatment,
            data = bioassay1_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Diatoms_1 = kruskal.test(Diatoms ~ Treatment, data = bioassay1_data)

#Tukey post-hoc testing (parametric)

Diatoms_Tukey_b1 = tukey_hsd(Diatoms_ANOVA_b1.1, conf.level = 0.95)

TukeyHSD(Diatoms_ANOVA_b1.1)

#REGW

post_hoc_Diatoms_b1 = REGW.test(y = Diatoms_ANOVA_b1.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Diatoms_b1)

#Games-Howell:

Diatoms_GH_b1 = games_howell_test(formula = Diatoms ~ Treatment,
                                        data = bioassay1_data, 
                                        conf.level = 0.95, 
                                        detailed = FALSE)

print(Diatoms_GH_b1, n = 22)

#Check for normality

model_1.5 = lm(Diatoms ~ Treatment, data = bioassay1_data)

ggqqplot(residuals(model_1.5))

shapiro_test(residuals(model_1.5))

#check for homogeneity of variances

leveneTest(bioassay1_data$Diatoms ~ bioassay1_data$Treatment)

############### Dinoflagellates

#ANOVA try 1:

Dinoflagellates_ANOVA_b1.1 = aov(Dinoflagellates ~ Treatment, data = bioassay1_data)

summary(Dinoflagellates_ANOVA_b1.1)

#ANOVA try 2: 

Dinoflagellates_ANOVA_b1.2 = oneway.test(Dinoflagellates ~ Treatment,
                                 data = bioassay1_data,
                                 var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Dinoflagellates ~ Treatment,
            data = bioassay1_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Dinoflagellates = kruskal.test(Dinoflagellates ~ Treatment, data = bioassay1_data)

#Tukey post-hoc testing (parametric)

Dinoflagellates_Tukey_b1 = tukey_hsd(Dinoflagellates_ANOVA_b1.1, conf.level = 0.95)

TukeyHSD(Dinoflagellates_ANOVA_b1.1)

#REGW

post_hoc_Dinoflagellates_b1 = REGW.test(y = Dinoflagellates_ANOVA_b1.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Dinoflagellates_b1)

#Games-Howell:

Dinoflagellates_GH_b1 = games_howell_test(formula = Dinoflagellates ~ Treatment,
                                  data = bioassay1_data, 
                                  conf.level = 0.95, 
                                  detailed = FALSE)

print(Dinoflagellates_GH_b1, n = 22)

#Check for normality

model_1.6 = lm(Dinoflagellates ~ Treatment, data = bioassay1_data)

ggqqplot(residuals(model_1.6))

shapiro_test(residuals(model_1.6))

#check for homogeneity of variances

leveneTest(bioassay1_data$Dinoflagellates ~ bioassay1_data$Treatment)

############### Prasinophytes

#ANOVA try 1:

Prasinophytes_ANOVA_b1.1 = aov(Prasinophytes ~ Treatment, data = bioassay1_data)

summary(Prasinophytes_ANOVA_b1.1)

#ANOVA try 2: 

Prasinophytes_ANOVA_b1.2 = oneway.test(Prasinophytes ~ Treatment,
                                         data = bioassay1_data,
                                         var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Prasinophytes ~ Treatment,
            data = bioassay1_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Prasinophytes = kruskal.test(Prasinophytes ~ Treatment, data = bioassay1_data)

#Tukey post-hoc testing (parametric)

Prasinophytes_Tukey_b1 = tukey_hsd(Prasinophytes_ANOVA_b1.1, conf.level = 0.95)

TukeyHSD(Prasinophytes_ANOVA_b1.1)

#REGW

post_hoc_Prasinophytes_b1 = REGW.test(y = Prasinophytes_ANOVA_b1.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Prasinophytes_b1)

#Games-Howell:

Prasinophytes_GH_b1 = games_howell_test(formula = Prasinophytes ~ Treatment,
                                          data = bioassay1_data, 
                                          conf.level = 0.95, 
                                          detailed = FALSE)

print(Prasinophytes_GH_b1, n = 22)

#Check for normality

model_1.7 = lm(Prasinophytes ~ Treatment, data = bioassay1_data)

ggqqplot(residuals(model_1.7))

shapiro_test(residuals(model_1.7))

#check for homogeneity of variances

leveneTest(bioassay1_data$Prasinophytes ~ bioassay1_data$Treatment)

############### Euglenophytes

#ANOVA try 1:

Euglenophytes_ANOVA_b1.1 = aov(Euglenophytes ~ Treatment, data = bioassay1_data)

summary(Euglenophytes_ANOVA_b1.1)

#ANOVA try 2: 

Euglenophytes_ANOVA_b1.2 = oneway.test(Euglenophytes ~ Treatment,
                                       data = bioassay1_data,
                                       var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Euglenophytes ~ Treatment,
            data = bioassay1_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Euglenophytes = kruskal.test(Euglenophytes ~ Treatment, data = bioassay1_data)

#Tukey post-hoc testing (parametric)

Euglenophytes_Tukey_b1 = tukey_hsd(Euglenophytes_ANOVA_b1.1, conf.level = 0.95)

TukeyHSD(Euglenophytes_ANOVA_b1.1)

#REGW

post_hoc_Euglenophytes_b1 = REGW.test(y = Euglenophytes_ANOVA_b1.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Euglenophytes_b1)

#Games-Howell:

Euglenophytes_GH_b1 = games_howell_test(formula = Euglenophytes ~ Treatment,
                                        data = bioassay1_data, 
                                        conf.level = 0.95, 
                                        detailed = FALSE)

print(Euglenophytes_GH_b1, n = 22)

#Check for normality

model_1.8 = lm(Euglenophytes ~ Treatment, data = bioassay1_data)

ggqqplot(residuals(model_1.8))

shapiro_test(residuals(model_1.8))

#check for homogeneity of variances

leveneTest(bioassay1_data$Euglenophytes ~ bioassay1_data$Treatment)

############### Haptophytes

#ANOVA try 1:

Haptophytes_ANOVA_b1.1 = aov(Haptophytes ~ Treatment, data = bioassay1_data)

summary(Haptophytes_ANOVA_b1.1)

#ANOVA try 2: 

Haptophytes_ANOVA_b1.2 = oneway.test(Haptophytes ~ Treatment,
                                       data = bioassay1_data,
                                       var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Haptophytes ~ Treatment,
            data = bioassay1_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Haptophytes = kruskal.test(Haptophytes ~ Treatment, data = bioassay1_data)

#Tukey post-hoc testing (parametric)

Haptophytes_Tukey_b1 = tukey_hsd(Haptophytes_ANOVA_b1.1, conf.level = 0.95)

TukeyHSD(Haptophytes_ANOVA_b1.1)

#REGW

post_hoc_Haptophytes_b1 = REGW.test(y = Haptophytes_ANOVA_b1.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Haptophytes_b1)

#Games-Howell:

Haptophytes_GH_b1 = games_howell_test(formula = Haptophytes ~ Treatment,
                                        data = bioassay1_data, 
                                        conf.level = 0.95, 
                                        detailed = FALSE)

print(Haptophytes_GH_b1, n = 22)

#Check for normality

model_1.9 = lm(Haptophytes ~ Treatment, data = bioassay1_data)

ggqqplot(residuals(model_1.9))

shapiro_test(residuals(model_1.9))

#check for homogeneity of variances

leveneTest(bioassay1_data$Haptophytes ~ bioassay1_data$Treatment)

#######################################################Bioassay2

#import data

bioassay2_data = read_excel("data/data_for_anova.xlsx", sheet = "bioassay2")

summary(bioassay2_data)
glimpse(bioassay2_data)

###############total_Chl_a

#ANOVA try 1:

total_chl_a_ANOVA_b2.1 = aov(Total_Chl_a ~ Treatment, data = bioassay2_data)

summary(total_chl_a_ANOVA_b2.1)

#ANOVA try 2: 

total_chl_a_ANOVA_b2.2 = oneway.test(Total_Chl_a ~ Treatment,
                                     data = bioassay2_data,
                                     var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Total_Chl_a ~ Treatment,
            data = bioassay2_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_total_chl_2 = kruskal.test(Total_Chl_a ~ Treatment, data = bioassay2_data)

#Tukey post-hoc testing (parametric)

total_chl_Tukey_b2 = tukey_hsd(total_chl_a_ANOVA_b2.1, conf.level = 0.95)

TukeyHSD(total_chl_a_ANOVA_b2.1)

#REGW

post_hoc_total_b2 = REGW.test(y = total_chl_a_ANOVA_b2.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_total_b2)

#Games-Howell:

total_chl_GH_b2 = games_howell_test(formula = Total_Chl_a ~ Treatment,
                                    data = bioassay2_data, 
                                    conf.level = 0.95, 
                                    detailed = FALSE)

print(total_chl_GH_b2, n = 22)

#Check for normality

model_2.1 = lm(Total_Chl_a ~ Treatment, data = bioassay2_data)

ggqqplot(residuals(model_2.1))

shapiro_test(residuals(model_2.1))

#check for homogeneity of variances

leveneTest(bioassay1_data$Total_Chl_a ~ bioassay2_data$Treatment)

############### Chlorophytes

#ANOVA try 1:

Chlorophytes_ANOVA_b2.1 = aov(Chlorophytes ~ Treatment, data = bioassay2_data)

summary(Chlorophytes_ANOVA_b2.1)

#ANOVA try 2: 

Chlorophytes_ANOVA_b2.2 = oneway.test(Chlorophytes ~ Treatment,
                                      data = bioassay2_data,
                                      var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Chlorophytes ~ Treatment,
            data = bioassay2_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Chlorophytes_2 = kruskal.test(Chlorophytes ~ Treatment, data = bioassay2_data)

#Tukey post-hoc testing (parametric)

Chlorophytes_Tukey_b2 = tukey_hsd(Chlorophytes_ANOVA_b2.1, conf.level = 0.95)

TukeyHSD(Chlorophytes_ANOVA_b2.1)

#REGW

post_hoc_chloro_b2 = REGW.test(y = Chlorophytes_ANOVA_b2.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_chloro_b2)

#Games-Howell:

Chlorophytes_GH_b2 = games_howell_test(formula = Chlorophytes ~ Treatment,
                                       data = bioassay2_data, 
                                       conf.level = 0.95, 
                                       detailed = FALSE)

print(Chlorophytes_GH_b2, n = 22)

#Check for normality

model_2.2 = lm(Chlorophytes ~ Treatment, data = bioassay2_data)

ggqqplot(residuals(model_2.2))

shapiro_test(residuals(model_2.2))

#check for homogeneity of variances

leveneTest(bioassay2_data$Chlorophytes ~ bioassay2_data$Treatment)

############### Cryptophytes

#ANOVA try 1:

Cryptophytes_ANOVA_b2.1 = aov(Cryptophytes ~ Treatment, data = bioassay2_data)

summary(Cryptophytes_ANOVA_b2.1)

#ANOVA try 2: 

Cryptophytes_ANOVA_b2.2 = oneway.test(Cryptophytes ~ Treatment,
                                      data = bioassay2_data,
                                      var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Cryptophytes ~ Treatment,
            data = bioassay2_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Cryptophytes_2 = kruskal.test(Cryptophytes ~ Treatment, data = bioassay2_data)

#Tukey post-hoc testing (parametric)

Cryptophytes_Tukey_b2 = tukey_hsd(Cryptophytes_ANOVA_b2.1, conf.level = 0.95)

TukeyHSD(Cryptophytes_ANOVA_b2.1)

#REGW

post_hoc_crypto_b2 = REGW.test(y = Cryptophytes_ANOVA_b2.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_crypto_b2)

#Games-Howell:

Cryptophytes_GH_b2 = games_howell_test(formula = Cryptophytes ~ Treatment,
                                       data = bioassay2_data, 
                                       conf.level = 0.95, 
                                       detailed = FALSE)

print(Cryptophytes_GH_b2, n = 22)

#Check for normality

model_2.3 = lm(Cryptophytes ~ Treatment, data = bioassay2_data)

ggqqplot(residuals(model_2.3))

shapiro_test(residuals(model_2.3))

#check for homogeneity of variances

leveneTest(bioassay2_data$Cryptophytes ~ bioassay2_data$Treatment)

############### Cyanobacteria

#ANOVA try 1:

Cyanobacteria_ANOVA_b2.1 = aov(Cyanobacteria ~ Treatment, data = bioassay2_data)

summary(Cyanobacteria_ANOVA_b2.1)

#ANOVA try 2: 

Cyanobacteria_ANOVA_b2.2 = oneway.test(Cyanobacteria ~ Treatment,
                                       data = bioassay2_data,
                                       var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Cyanobacteria ~ Treatment,
            data = bioassay2_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Cyanobacteria_2 = kruskal.test(Cyanobacteria ~ Treatment, data = bioassay2_data)

#Tukey post-hoc testing (parametric)

Cyanobacteria_Tukey_b2 = tukey_hsd(Cyanobacteria_ANOVA_b2.1, conf.level = 0.95)

TukeyHSD(Cyanobacteria_ANOVA_b2.1)

#REGW

post_hoc_cyano_b2 = REGW.test(y = Cyanobacteria_ANOVA_b2.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_cyano_b2)

#Games-Howell:

Cyanobacteria_GH_b2 = games_howell_test(formula = Cyanobacteria ~ Treatment,
                                        data = bioassay2_data, 
                                        conf.level = 0.95, 
                                        detailed = FALSE)

print(Cyanobacteria_GH_b2, n = 22)

#Check for normality

model_2.4 = lm(Cyanobacteria ~ Treatment, data = bioassay2_data)

ggqqplot(residuals(model_2.4))

shapiro_test(residuals(model_2.4))

#check for homogeneity of variances

leveneTest(bioassay2_data$Cyanobacteria ~ bioassay2_data$Treatment)

############### Diatoms

#ANOVA try 1:

Diatoms_ANOVA_b2.1 = aov(Diatoms ~ Treatment, data = bioassay2_data)

summary(Diatoms_ANOVA_b2.1)

#ANOVA try 2: 

Diatoms_ANOVA_b2.2 = oneway.test(Diatoms ~ Treatment,
                                 data = bioassay2_data,
                                 var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Diatoms ~ Treatment,
            data = bioassay2_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Diatoms_2 = kruskal.test(Diatoms ~ Treatment, data = bioassay2_data)

#Tukey post-hoc testing (parametric)

Diatoms_Tukey_b2 = tukey_hsd(Diatoms_ANOVA_b2.1, conf.level = 0.95)

TukeyHSD(Diatoms_ANOVA_b2.1)

#REGW

post_hoc_Diatoms_b2 = REGW.test(y = Diatoms_ANOVA_b2.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Diatoms_b2)

#Games-Howell:

Diatoms_GH_b2 = games_howell_test(formula = Diatoms ~ Treatment,
                                  data = bioassay2_data, 
                                  conf.level = 0.95, 
                                  detailed = FALSE)

print(Diatoms_GH_b2, n = 22)

#Check for normality

model_2.5 = lm(Diatoms ~ Treatment, data = bioassay2_data)

ggqqplot(residuals(model_2.5))

shapiro_test(residuals(model_2.5))

#check for homogeneity of variances

leveneTest(bioassay2_data$Diatoms ~ bioassay2_data$Treatment)

############### Dinoflagellates

#ANOVA try 1:

Dinoflagellates_ANOVA_b2.1 = aov(Dinoflagellates ~ Treatment, data = bioassay2_data)

summary(Dinoflagellates_ANOVA_b2.1)

#ANOVA try 2: 

Dinoflagellates_ANOVA_b2.2 = oneway.test(Dinoflagellates ~ Treatment,
                                         data = bioassay2_data,
                                         var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Dinoflagellates ~ Treatment,
            data = bioassay2_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Dinoflagellates_2 = kruskal.test(Dinoflagellates ~ Treatment, data = bioassay2_data)

#Tukey post-hoc testing (parametric)

Dinoflagellates_Tukey_b2 = tukey_hsd(Dinoflagellates_ANOVA_b2.1, conf.level = 0.95)

TukeyHSD(Dinoflagellates_ANOVA_b2.1)

#REGW

post_hoc_Dinoflagellates_b2 = REGW.test(y = Dinoflagellates_ANOVA_b2.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Dinoflagellates_b2)

#Games-Howell:

Dinoflagellates_GH_b2 = games_howell_test(formula = Dinoflagellates ~ Treatment,
                                          data = bioassay2_data, 
                                          conf.level = 0.95, 
                                          detailed = FALSE)

print(Dinoflagellates_GH_b2, n = 22)

#Check for normality

model_2.6 = lm(Dinoflagellates ~ Treatment, data = bioassay2_data)

ggqqplot(residuals(model_2.6))

shapiro_test(residuals(model_2.6))

#check for homogeneity of variances

leveneTest(bioassay2_data$Dinoflagellates ~ bioassay2_data$Treatment)

############### Prasinophytes

#ANOVA try 1:

Prasinophytes_ANOVA_b2.1 = aov(Prasinophytes ~ Treatment, data = bioassay2_data)

summary(Prasinophytes_ANOVA_b2.1)

#ANOVA try 2: 

Prasinophytes_ANOVA_b2.2 = oneway.test(Prasinophytes ~ Treatment,
                                       data = bioassay2_data,
                                       var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Prasinophytes ~ Treatment,
            data = bioassay2_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Prasinophytes_2 = kruskal.test(Prasinophytes ~ Treatment, data = bioassay2_data)

#Tukey post-hoc testing (parametric)

Prasinophytes_Tukey_b2 = tukey_hsd(Prasinophytes_ANOVA_b2.1, conf.level = 0.95)

TukeyHSD(Prasinophytes_ANOVA_b2.1)

#REGW

post_hoc_Prasinophytes_b2 = REGW.test(y = Prasinophytes_ANOVA_b2.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Prasinophytes_b2)

#Games-Howell:

Prasinophytes_GH_b2 = games_howell_test(formula = Prasinophytes ~ Treatment,
                                        data = bioassay2_data, 
                                        conf.level = 0.95, 
                                        detailed = FALSE)

print(Prasinophytes_GH_b2, n = 22)

#Check for normality

model_2.7 = lm(Prasinophytes ~ Treatment, data = bioassay2_data)

ggqqplot(residuals(model_2.7))

shapiro_test(residuals(model_2.7))

#check for homogeneity of variances

leveneTest(bioassay2_data$Prasinophytes ~ bioassay2_data$Treatment)

############### Euglenophytes

#ANOVA try 1:

Euglenophytes_ANOVA_b2.1 = aov(Euglenophytes ~ Treatment, data = bioassay2_data)

summary(Euglenophytes_ANOVA_b2.1)

#ANOVA try 2: 

Euglenophytes_ANOVA_b2.2 = oneway.test(Euglenophytes ~ Treatment,
                                       data = bioassay2_data,
                                       var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Euglenophytes ~ Treatment,
            data = bioassay2_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Euglenophytes_b2 = kruskal.test(Euglenophytes ~ Treatment, data = bioassay2_data)

#Tukey post-hoc testing (parametric)

Euglenophytes_Tukey_b2 = tukey_hsd(Euglenophytes_ANOVA_b2.1, conf.level = 0.95)

TukeyHSD(Euglenophytes_ANOVA_b2.1)

#REGW

post_hoc_Euglenophytes_b2 = REGW.test(y = Euglenophytes_ANOVA_b2.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Euglenophytes_b2)

#Games-Howell:

Euglenophytes_GH_b2 = games_howell_test(formula = Euglenophytes ~ Treatment,
                                        data = bioassay2_data, 
                                        conf.level = 0.95, 
                                        detailed = FALSE)

print(Euglenophytes_GH_b2, n = 22)

#Check for normality

model_2.8 = lm(Euglenophytes ~ Treatment, data = bioassay2_data)

ggqqplot(residuals(model_2.8))

shapiro_test(residuals(model_2.8))

#check for homogeneity of variances

leveneTest(bioassay2_data$Euglenophytes ~ bioassay2_data$Treatment)

############### Haptophytes

#ANOVA try 1:

Haptophytes_ANOVA_b2.1 = aov(Haptophytes ~ Treatment, data = bioassay2_data)

summary(Haptophytes_ANOVA_b2.1)

#ANOVA try 2: 

Haptophytes_ANOVA_b2.2 = oneway.test(Haptophytes ~ Treatment,
                                     data = bioassay2_data,
                                     var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Haptophytes ~ Treatment,
            data = bioassay2_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Haptophytes_b2 = kruskal.test(Haptophytes ~ Treatment, data = bioassay2_data)

#Tukey post-hoc testing (parametric)

Haptophytes_Tukey_b2 = tukey_hsd(Haptophytes_ANOVA_b2.1, conf.level = 0.95)

TukeyHSD(Haptophytes_ANOVA_b2.1)

#REGW

post_hoc_Haptophytes_b2 = REGW.test(y = Haptophytes_ANOVA_b2.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Haptophytes_b2)

#Games-Howell:

Haptophytes_GH_b2 = games_howell_test(formula = Haptophytes ~ Treatment,
                                      data = bioassay2_data, 
                                      conf.level = 0.95, 
                                      detailed = FALSE)

print(Haptophytes_GH_b2, n = 22)

#Check for normality

model_2.9 = lm(Haptophytes ~ Treatment, data = bioassay2_data)

ggqqplot(residuals(model_2.9))

shapiro_test(residuals(model_2.9))

#check for homogeneity of variances

leveneTest(bioassay2_data$Haptophytes ~ bioassay2_data$Treatment)

#######################################################Bioassay3

#import data

bioassay3_data = read_excel("data/data_for_anova.xlsx", sheet = "bioassay3")

summary(bioassay3_data)
glimpse(bioassay3_data)

###############total_Chl_a

#ANOVA try 1:

total_chl_a_ANOVA_b3.1 = aov(Total_Chl_a ~ Treatment, data = bioassay3_data)

summary(total_chl_a_ANOVA_b3.1)

#ANOVA try 2: 

total_chl_a_ANOVA_b3.2 = oneway.test(Total_Chl_a ~ Treatment,
                                     data = bioassay3_data,
                                     var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Total_Chl_a ~ Treatment,
            data = bioassay3_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_total_chl_3 = kruskal.test(Total_Chl_a ~ Treatment, data = bioassay3_data)

#Tukey post-hoc testing (parametric)

total_chl_Tukey_b3 = tukey_hsd(total_chl_a_ANOVA_b3.1, conf.level = 0.95)

TukeyHSD(total_chl_a_ANOVA_b3.1)

#REGW

post_hoc_total_b3 = REGW.test(y = total_chl_a_ANOVA_b3.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_total_b3)

#Games-Howell:

total_chl_GH_b3 = games_howell_test(formula = Total_Chl_a ~ Treatment,
                                    data = bioassay3_data, 
                                    conf.level = 0.95, 
                                    detailed = FALSE)

print(total_chl_GH_b3, n = 22)

#Check for normality

model_3.1 = lm(Total_Chl_a ~ Treatment, data = bioassay3_data)

ggqqplot(residuals(model_3.1))

shapiro_test(residuals(model_3.1))

#check for homogeneity of variances

leveneTest(bioassay3_data$Total_Chl_a ~ bioassay3_data$Treatment)

############### Chlorophytes

#ANOVA try 1:

Chlorophytes_ANOVA_b3.1 = aov(Chlorophytes ~ Treatment, data = bioassay3_data)

summary(Chlorophytes_ANOVA_b3.1)

#ANOVA try 2: 

Chlorophytes_ANOVA_b3.2 = oneway.test(Chlorophytes ~ Treatment,
                                      data = bioassay3_data,
                                      var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Chlorophytes ~ Treatment,
            data = bioassay3_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Chlorophytes_3 = kruskal.test(Chlorophytes ~ Treatment, data = bioassay3_data)

#Tukey post-hoc testing (parametric)

Chlorophytes_Tukey_b3 = tukey_hsd(Chlorophytes_ANOVA_b3.1, conf.level = 0.95)

TukeyHSD(Chlorophytes_ANOVA_b3.1)

#REGW

post_hoc_chloro_b3 = REGW.test(y = Chlorophytes_ANOVA_b3.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_chloro_b3)

#Games-Howell:

Chlorophytes_GH_b3 = games_howell_test(formula = Chlorophytes ~ Treatment,
                                       data = bioassay3_data, 
                                       conf.level = 0.95, 
                                       detailed = FALSE)

print(Chlorophytes_GH_b3, n = 22)

#Check for normality

model_3.2 = lm(Chlorophytes ~ Treatment, data = bioassay3_data)

ggqqplot(residuals(model_3.2))

shapiro_test(residuals(model_3.2))

#check for homogeneity of variances

leveneTest(bioassay3_data$Chlorophytes ~ bioassay3_data$Treatment)

############### Cryptophytes

#ANOVA try 1:

Cryptophytes_ANOVA_b3.1 = aov(Cryptophytes ~ Treatment, data = bioassay3_data)

summary(Cryptophytes_ANOVA_b3.1)

#ANOVA try 2: 

Cryptophytes_ANOVA_b3.2 = oneway.test(Cryptophytes ~ Treatment,
                                      data = bioassay3_data,
                                      var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Cryptophytes ~ Treatment,
            data = bioassay3_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Cryptophytes_3 = kruskal.test(Cryptophytes ~ Treatment, data = bioassay3_data)

#Tukey post-hoc testing (parametric)

Cryptophytes_Tukey_b3 = tukey_hsd(Cryptophytes_ANOVA_b3.1, conf.level = 0.95)

TukeyHSD(Cryptophytes_ANOVA_b3.1)

#REGW

post_hoc_crypto_b3 = REGW.test(y = Cryptophytes_ANOVA_b3.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_crypto_b3)

#Games-Howell:

Cryptophytes_GH_b3 = games_howell_test(formula = Cryptophytes ~ Treatment,
                                       data = bioassay3_data, 
                                       conf.level = 0.95, 
                                       detailed = FALSE)

print(Cryptophytes_GH_b3, n = 22)

#Check for normality

model_3.3 = lm(Cryptophytes ~ Treatment, data = bioassay3_data)

ggqqplot(residuals(model_3.3))

shapiro_test(residuals(model_3.3))

#check for homogeneity of variances

leveneTest(bioassay3_data$Cryptophytes ~ bioassay3_data$Treatment)

############### Cyanobacteria

#ANOVA try 1:

Cyanobacteria_ANOVA_b3.1 = aov(Cyanobacteria ~ Treatment, data = bioassay3_data)

summary(Cyanobacteria_ANOVA_b3.1)

#ANOVA try 2: 

Cyanobacteria_ANOVA_b3.2 = oneway.test(Cyanobacteria ~ Treatment,
                                       data = bioassay3_data,
                                       var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Cyanobacteria ~ Treatment,
            data = bioassay3_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Cyanobacteria_3 = kruskal.test(Cyanobacteria ~ Treatment, data = bioassay3_data)

#Tukey post-hoc testing (parametric)

Cyanobacteria_Tukey_b3 = tukey_hsd(Cyanobacteria_ANOVA_b3.1, conf.level = 0.95)

TukeyHSD(Cyanobacteria_ANOVA_b3.1)

#REGW

post_hoc_cyano_b3 = REGW.test(y = Cyanobacteria_ANOVA_b3.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_cyano_b3)

#Games-Howell:

Cyanobacteria_GH_b3 = games_howell_test(formula = Cyanobacteria ~ Treatment,
                                        data = bioassay3_data, 
                                        conf.level = 0.95, 
                                        detailed = FALSE)

print(Cyanobacteria_GH_b3, n = 22)

#Check for normality

model_3.4 = lm(Cyanobacteria ~ Treatment, data = bioassay3_data)

ggqqplot(residuals(model_3.4))

shapiro_test(residuals(model_3.4))

#check for homogeneity of variances

leveneTest(bioassay3_data$Cyanobacteria ~ bioassay3_data$Treatment)

############### Diatoms

#ANOVA try 1:

Diatoms_ANOVA_b3.1 = aov(Diatoms ~ Treatment, data = bioassay3_data)

summary(Diatoms_ANOVA_b3.1)

#ANOVA try 2: 

Diatoms_ANOVA_b3.2 = oneway.test(Diatoms ~ Treatment,
                                 data = bioassay3_data,
                                 var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Diatoms ~ Treatment,
            data = bioassay3_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Diatoms_3 = kruskal.test(Diatoms ~ Treatment, data = bioassay3_data)

#Tukey post-hoc testing (parametric)

Diatoms_Tukey_b3 = tukey_hsd(Diatoms_ANOVA_b3.1, conf.level = 0.95)

TukeyHSD(Diatoms_ANOVA_b3.1)

#REGW

post_hoc_Diatoms_b3 = REGW.test(y = Diatoms_ANOVA_b3.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Diatoms_b3)

#Games-Howell:

Diatoms_GH_b3 = games_howell_test(formula = Diatoms ~ Treatment,
                                  data = bioassay3_data, 
                                  conf.level = 0.95, 
                                  detailed = FALSE)

print(Diatoms_GH_b3, n = 22)

#Check for normality

model_3.5 = lm(Diatoms ~ Treatment, data = bioassay3_data)

ggqqplot(residuals(model_3.5))

shapiro_test(residuals(model_3.5))

#check for homogeneity of variances

leveneTest(bioassay3_data$Diatoms ~ bioassay3_data$Treatment)

############### Dinoflagellates

#ANOVA try 1:

Dinoflagellates_ANOVA_b3.1 = aov(Dinoflagellates ~ Treatment, data = bioassay3_data)

summary(Dinoflagellates_ANOVA_b3.1)

#ANOVA try 2: 

Dinoflagellates_ANOVA_b3.2 = oneway.test(Dinoflagellates ~ Treatment,
                                         data = bioassay3_data,
                                         var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Dinoflagellates ~ Treatment,
            data = bioassay3_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Dinoflagellates_3 = kruskal.test(Dinoflagellates ~ Treatment, data = bioassay3_data)

#Tukey post-hoc testing (parametric)

Dinoflagellates_Tukey_b3 = tukey_hsd(Dinoflagellates_ANOVA_b3.1, conf.level = 0.95)

TukeyHSD(Dinoflagellates_ANOVA_b3.1)

#REGW

post_hoc_Dinoflagellates_b3 = REGW.test(y = Dinoflagellates_ANOVA_b3.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Dinoflagellates_b3)

#Games-Howell:

Dinoflagellates_GH_b3 = games_howell_test(formula = Dinoflagellates ~ Treatment,
                                          data = bioassay3_data, 
                                          conf.level = 0.95, 
                                          detailed = FALSE)

print(Dinoflagellates_GH_b3, n = 22)

#Check for normality

model_3.6 = lm(Dinoflagellates ~ Treatment, data = bioassay3_data)

ggqqplot(residuals(model_3.6))

shapiro_test(residuals(model_3.6))

#check for homogeneity of variances

leveneTest(bioassay3_data$Dinoflagellates ~ bioassay3_data$Treatment)

############### Prasinophytes

#ANOVA try 1:

Prasinophytes_ANOVA_b3.1 = aov(Prasinophytes ~ Treatment, data = bioassay3_data)

summary(Prasinophytes_ANOVA_b3.1)

#ANOVA try 2: 

Prasinophytes_ANOVA_b3.2 = oneway.test(Prasinophytes ~ Treatment,
                                       data = bioassay3_data,
                                       var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Prasinophytes ~ Treatment,
            data = bioassay3_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Prasinophytes_3 = kruskal.test(Prasinophytes ~ Treatment, data = bioassay3_data)

#Tukey post-hoc testing (parametric)

Prasinophytes_Tukey_b3 = tukey_hsd(Prasinophytes_ANOVA_b3.1, conf.level = 0.95)

TukeyHSD(Prasinophytes_ANOVA_b3.1)

#REGW

post_hoc_Prasinophytes_b3 = REGW.test(y = Prasinophytes_ANOVA_b3.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Prasinophytes_b3)

#Games-Howell:

Prasinophytes_GH_b3 = games_howell_test(formula = Prasinophytes ~ Treatment,
                                        data = bioassay3_data, 
                                        conf.level = 0.95, 
                                        detailed = FALSE)

print(Prasinophytes_GH_b3, n = 22)

#Check for normality

model_3.7 = lm(Prasinophytes ~ Treatment, data = bioassay3_data)

ggqqplot(residuals(model_3.7))

shapiro_test(residuals(model_3.7))

#check for homogeneity of variances

leveneTest(bioassay3_data$Prasinophytes ~ bioassay3_data$Treatment)

############### Euglenophytes

#ANOVA try 1:

Euglenophytes_ANOVA_b3.1 = aov(Euglenophytes ~ Treatment, data = bioassay3_data)

summary(Euglenophytes_ANOVA_b3.1)

#ANOVA try 2: 

Euglenophytes_ANOVA_b3.2 = oneway.test(Euglenophytes ~ Treatment,
                                       data = bioassay3_data,
                                       var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Euglenophytes ~ Treatment,
            data = bioassay3_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Euglenophytes_b3 = kruskal.test(Euglenophytes ~ Treatment, data = bioassay3_data)

#Tukey post-hoc testing (parametric)

Euglenophytes_Tukey_b3 = tukey_hsd(Euglenophytes_ANOVA_b3.1, conf.level = 0.95)

TukeyHSD(Euglenophytes_ANOVA_b3.1)

#REGW

post_hoc_Euglenophytes_b3 = REGW.test(y = Euglenophytes_ANOVA_b3.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Euglenophytes_b3)

#Games-Howell:

Euglenophytes_GH_b3 = games_howell_test(formula = Euglenophytes ~ Treatment,
                                        data = bioassay3_data, 
                                        conf.level = 0.95, 
                                        detailed = FALSE)

print(Euglenophytes_GH_b3, n = 22)

#Check for normality

model_3.8 = lm(Euglenophytes ~ Treatment, data = bioassay3_data)

ggqqplot(residuals(model_3.8))

shapiro_test(residuals(model_3.8))

#check for homogeneity of variances

leveneTest(bioassay3_data$Euglenophytes ~ bioassay3_data$Treatment)

############### Haptophytes

#ANOVA try 1:

Haptophytes_ANOVA_b3.1 = aov(Haptophytes ~ Treatment, data = bioassay3_data)

summary(Haptophytes_ANOVA_b3.1)

#ANOVA try 2: 

Haptophytes_ANOVA_b3.2 = oneway.test(Haptophytes ~ Treatment,
                                     data = bioassay3_data,
                                     var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Haptophytes ~ Treatment,
            data = bioassay3_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Haptophytes_b3 = kruskal.test(Haptophytes ~ Treatment, data = bioassay3_data)

#Tukey post-hoc testing (parametric)

Haptophytes_Tukey_b3 = tukey_hsd(Haptophytes_ANOVA_b3.1, conf.level = 0.95)

TukeyHSD(Haptophytes_ANOVA_b3.1)

#REGW

post_hoc_Haptophytes_b3 = REGW.test(y = Haptophytes_ANOVA_b3.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Haptophytes_b3)

#Games-Howell:

Haptophytes_GH_b3 = games_howell_test(formula = Haptophytes ~ Treatment,
                                      data = bioassay3_data, 
                                      conf.level = 0.95, 
                                      detailed = FALSE)

print(Haptophytes_GH_b3, n = 22)

#Check for normality

model_3.9 = lm(Haptophytes ~ Treatment, data = bioassay3_data)

ggqqplot(residuals(model_3.9))

shapiro_test(residuals(model_3.9))

#check for homogeneity of variances

leveneTest(bioassay3_data$Haptophytes ~ bioassay3_data$Treatment)

#######################################################Bioassay4

#import data

bioassay4_data = read_excel("data/data_for_anova.xlsx", sheet = "bioassay4")

summary(bioassay4_data)
glimpse(bioassay4_data)

###############total_Chl_a

#ANOVA try 1:

total_chl_a_ANOVA_b4.1 = aov(Total_Chl_a ~ Treatment, data = bioassay4_data)

summary(total_chl_a_ANOVA_b4.1)

#ANOVA try 2: 

total_chl_a_ANOVA_b4.2 = oneway.test(Total_Chl_a ~ Treatment,
                                     data = bioassay4_data,
                                     var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Total_Chl_a ~ Treatment,
            data = bioassay4_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_total_chl_4 = kruskal.test(Total_Chl_a ~ Treatment, data = bioassay4_data)

#Tukey post-hoc testing (parametric)

total_chl_Tukey_b4 = tukey_hsd(total_chl_a_ANOVA_b4.1, conf.level = 0.95)

TukeyHSD(total_chl_a_ANOVA_b4.1)

#REGW

post_hoc_total_b4 = REGW.test(y = total_chl_a_ANOVA_b4.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_total_b4)

#Games-Howell:

total_chl_GH_b4 = games_howell_test(formula = Total_Chl_a ~ Treatment,
                                    data = bioassay4_data, 
                                    conf.level = 0.95, 
                                    detailed = FALSE)

print(total_chl_GH_b4, n = 22)

#Check for normality

model_4.1 = lm(Total_Chl_a ~ Treatment, data = bioassay4_data)

ggqqplot(residuals(model_4.1))

shapiro_test(residuals(model_4.1))

#check for homogeneity of variances

leveneTest(bioassay4_data$Total_Chl_a ~ bioassay4_data$Treatment)

############### Chlorophytes

#ANOVA try 1:

Chlorophytes_ANOVA_b4.1 = aov(Chlorophytes ~ Treatment, data = bioassay4_data)

summary(Chlorophytes_ANOVA_b4.1)

#ANOVA try 2: 

Chlorophytes_ANOVA_b4.2 = oneway.test(Chlorophytes ~ Treatment,
                                      data = bioassay4_data,
                                      var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Chlorophytes ~ Treatment,
            data = bioassay4_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Chlorophytes_4 = kruskal.test(Chlorophytes ~ Treatment, data = bioassay4_data)

#Tukey post-hoc testing (parametric)

Chlorophytes_Tukey_b4 = tukey_hsd(Chlorophytes_ANOVA_b4.1, conf.level = 0.95)

TukeyHSD(Chlorophytes_ANOVA_b4.1)

#REGW

post_hoc_chloro_b4 = REGW.test(y = Chlorophytes_ANOVA_b4.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_chloro_b4)

#Games-Howell:

Chlorophytes_GH_b4 = games_howell_test(formula = Chlorophytes ~ Treatment,
                                       data = bioassay4_data, 
                                       conf.level = 0.95, 
                                       detailed = FALSE)

print(Chlorophytes_GH_b4, n = 22)

#Check for normality

model_4.2 = lm(Chlorophytes ~ Treatment, data = bioassay4_data)

ggqqplot(residuals(model_4.2))

shapiro_test(residuals(model_4.2))

#check for homogeneity of variances

leveneTest(bioassay4_data$Chlorophytes ~ bioassay4_data$Treatment)

############### Cryptophytes

#ANOVA try 1:

Cryptophytes_ANOVA_b4.1 = aov(Cryptophytes ~ Treatment, data = bioassay4_data)

summary(Cryptophytes_ANOVA_b4.1)

#ANOVA try 2: 

Cryptophytes_ANOVA_b4.2 = oneway.test(Cryptophytes ~ Treatment,
                                      data = bioassay4_data,
                                      var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Cryptophytes ~ Treatment,
            data = bioassay4_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Cryptophytes_4 = kruskal.test(Cryptophytes ~ Treatment, data = bioassay4_data)

#Tukey post-hoc testing (parametric)

Cryptophytes_Tukey_b4 = tukey_hsd(Cryptophytes_ANOVA_b4.1, conf.level = 0.95)

TukeyHSD(Cryptophytes_ANOVA_b4.1)

#REGW

post_hoc_crypto_b4 = REGW.test(y = Cryptophytes_ANOVA_b4.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_crypto_b4)

#Games-Howell:

Cryptophytes_GH_b4 = games_howell_test(formula = Cryptophytes ~ Treatment,
                                       data = bioassay4_data, 
                                       conf.level = 0.95, 
                                       detailed = FALSE)

print(Cryptophytes_GH_b4, n = 22)

#Check for normality

model_4.3 = lm(Cryptophytes ~ Treatment, data = bioassay4_data)

ggqqplot(residuals(model_4.3))

shapiro_test(residuals(model_4.3))

#check for homogeneity of variances

leveneTest(bioassay4_data$Cryptophytes ~ bioassay4_data$Treatment)

############### Cyanobacteria

#ANOVA try 1:

Cyanobacteria_ANOVA_b4.1 = aov(Cyanobacteria ~ Treatment, data = bioassay4_data)

summary(Cyanobacteria_ANOVA_b4.1)

#ANOVA try 2: 

Cyanobacteria_ANOVA_b4.2 = oneway.test(Cyanobacteria ~ Treatment,
                                       data = bioassay4_data,
                                       var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Cyanobacteria ~ Treatment,
            data = bioassay4_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Cyanobacteria_4 = kruskal.test(Cyanobacteria ~ Treatment, data = bioassay4_data)

#Tukey post-hoc testing (parametric)

Cyanobacteria_Tukey_b4 = tukey_hsd(Cyanobacteria_ANOVA_b4.1, conf.level = 0.95)

TukeyHSD(Cyanobacteria_ANOVA_b4.1)

#REGW

post_hoc_cyano_b4 = REGW.test(y = Cyanobacteria_ANOVA_b4.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_cyano_b4)

#Games-Howell:

Cyanobacteria_GH_b4 = games_howell_test(formula = Cyanobacteria ~ Treatment,
                                        data = bioassay4_data, 
                                        conf.level = 0.95, 
                                        detailed = FALSE)

print(Cyanobacteria_GH_b4, n = 22)

#Check for normality

model_4.4 = lm(Cyanobacteria ~ Treatment, data = bioassay4_data)

ggqqplot(residuals(model_4.4))

shapiro_test(residuals(model_4.4))

#check for homogeneity of variances

leveneTest(bioassay4_data$Cyanobacteria ~ bioassay4_data$Treatment)

############### Diatoms

#ANOVA try 1:

Diatoms_ANOVA_b4.1 = aov(Diatoms ~ Treatment, data = bioassay4_data)

summary(Diatoms_ANOVA_b4.1)

#ANOVA try 2: 

Diatoms_ANOVA_b4.2 = oneway.test(Diatoms ~ Treatment,
                                 data = bioassay4_data,
                                 var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Diatoms ~ Treatment,
            data = bioassay4_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Diatoms_4 = kruskal.test(Diatoms ~ Treatment, data = bioassay4_data)

#Tukey post-hoc testing (parametric)

Diatoms_Tukey_b4 = tukey_hsd(Diatoms_ANOVA_b4.1, conf.level = 0.95)

TukeyHSD(Diatoms_ANOVA_b4.1)

#REGW

post_hoc_Diatoms_b4 = REGW.test(y = Diatoms_ANOVA_b4.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Diatoms_b4)

#Games-Howell:

Diatoms_GH_b4 = games_howell_test(formula = Diatoms ~ Treatment,
                                  data = bioassay4_data, 
                                  conf.level = 0.95, 
                                  detailed = FALSE)

print(Diatoms_GH_b4, n = 22)

#Check for normality

model_4.5 = lm(Diatoms ~ Treatment, data = bioassay4_data)

ggqqplot(residuals(model_4.5))

shapiro_test(residuals(model_4.5))

#check for homogeneity of variances

leveneTest(bioassay4_data$Diatoms ~ bioassay4_data$Treatment)

############### Dinoflagellates

#ANOVA try 1:

Dinoflagellates_ANOVA_b4.1 = aov(Dinoflagellates ~ Treatment, data = bioassay4_data)

summary(Dinoflagellates_ANOVA_b4.1)

#ANOVA try 2: 

Dinoflagellates_ANOVA_b4.2 = oneway.test(Dinoflagellates ~ Treatment,
                                         data = bioassay4_data,
                                         var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Dinoflagellates ~ Treatment,
            data = bioassay4_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Dinoflagellates_4 = kruskal.test(Dinoflagellates ~ Treatment, data = bioassay4_data)

#Tukey post-hoc testing (parametric)

Dinoflagellates_Tukey_b4 = tukey_hsd(Dinoflagellates_ANOVA_b4.1, conf.level = 0.95)

TukeyHSD(Dinoflagellates_ANOVA_b4.1)

#REGW

post_hoc_Dinoflagellates_b4 = REGW.test(y = Dinoflagellates_ANOVA_b4.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Dinoflagellates_b4)

#Games-Howell:

Dinoflagellates_GH_b4 = games_howell_test(formula = Dinoflagellates ~ Treatment,
                                          data = bioassay4_data, 
                                          conf.level = 0.95, 
                                          detailed = FALSE)

print(Dinoflagellates_GH_b4, n = 22)

#Check for normality

model_4.6 = lm(Dinoflagellates ~ Treatment, data = bioassay4_data)

ggqqplot(residuals(model_4.6))

shapiro_test(residuals(model_4.6))

#check for homogeneity of variances

leveneTest(bioassay4_data$Dinoflagellates ~ bioassay4_data$Treatment)

############### Prasinophytes

#ANOVA try 1:

Prasinophytes_ANOVA_b4.1 = aov(Prasinophytes ~ Treatment, data = bioassay4_data)

summary(Prasinophytes_ANOVA_b4.1)

#ANOVA try 2: 

Prasinophytes_ANOVA_b4.2 = oneway.test(Prasinophytes ~ Treatment,
                                       data = bioassay4_data,
                                       var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Prasinophytes ~ Treatment,
            data = bioassay4_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Prasinophytes_4 = kruskal.test(Prasinophytes ~ Treatment, data = bioassay4_data)

#Tukey post-hoc testing (parametric)

Prasinophytes_Tukey_b4 = tukey_hsd(Prasinophytes_ANOVA_b4.1, conf.level = 0.95)

TukeyHSD(Prasinophytes_ANOVA_b4.1)

#REGW

post_hoc_Prasinophytes_b4 = REGW.test(y = Prasinophytes_ANOVA_b4.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Prasinophytes_b4)

#Games-Howell:

Prasinophytes_GH_b4 = games_howell_test(formula = Prasinophytes ~ Treatment,
                                        data = bioassay4_data, 
                                        conf.level = 0.95, 
                                        detailed = FALSE)

print(Prasinophytes_GH_b4, n = 22)

#Check for normality

model_4.7 = lm(Prasinophytes ~ Treatment, data = bioassay4_data)

ggqqplot(residuals(model_4.7))

shapiro_test(residuals(model_4.7))

#check for homogeneity of variances

leveneTest(bioassay4_data$Prasinophytes ~ bioassay4_data$Treatment)

############### Euglenophytes

#ANOVA try 1:

Euglenophytes_ANOVA_b4.1 = aov(Euglenophytes ~ Treatment, data = bioassay4_data)

summary(Euglenophytes_ANOVA_b4.1)

#ANOVA try 2: 

Euglenophytes_ANOVA_b4.2 = oneway.test(Euglenophytes ~ Treatment,
                                       data = bioassay4_data,
                                       var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Euglenophytes ~ Treatment,
            data = bioassay4_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Euglenophytes_b4 = kruskal.test(Euglenophytes ~ Treatment, data = bioassay4_data)

#Tukey post-hoc testing (parametric)

Euglenophytes_Tukey_b4 = tukey_hsd(Euglenophytes_ANOVA_b4.1, conf.level = 0.95)

TukeyHSD(Euglenophytes_ANOVA_b4.1)

#REGW

post_hoc_Euglenophytes_b4 = REGW.test(y = Euglenophytes_ANOVA_b4.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Euglenophytes_b4)

#Games-Howell:

Euglenophytes_GH_b4 = games_howell_test(formula = Euglenophytes ~ Treatment,
                                        data = bioassay4_data, 
                                        conf.level = 0.95, 
                                        detailed = FALSE)

print(Euglenophytes_GH_b4, n = 22)

#Check for normality

model_4.8 = lm(Euglenophytes ~ Treatment, data = bioassay4_data)

ggqqplot(residuals(model_4.8))

shapiro_test(residuals(model_4.8))

#check for homogeneity of variances

leveneTest(bioassay4_data$Euglenophytes ~ bioassay4_data$Treatment)

############### Haptophytes

#ANOVA try 1:

Haptophytes_ANOVA_b4.1 = aov(Haptophytes ~ Treatment, data = bioassay4_data)

summary(Haptophytes_ANOVA_b4.1)

#ANOVA try 2: 

Haptophytes_ANOVA_b4.2 = oneway.test(Haptophytes ~ Treatment,
                                     data = bioassay4_data,
                                     var.equal = TRUE)

#Non-parametric ANOVA (Welchs)

oneway.test(Haptophytes ~ Treatment,
            data = bioassay4_data,
            var.equal = FALSE)

#Kruskal-wallace:

kruskal_Haptophytes_b4 = kruskal.test(Haptophytes ~ Treatment, data = bioassay4_data)

#Tukey post-hoc testing (parametric)

Haptophytes_Tukey_b4 = tukey_hsd(Haptophytes_ANOVA_b4.1, conf.level = 0.95)

TukeyHSD(Haptophytes_ANOVA_b4.1)

#REGW

post_hoc_Haptophytes_b4 = REGW.test(y = Haptophytes_ANOVA_b4.1, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_Haptophytes_b4)

#Games-Howell:

Haptophytes_GH_b4 = games_howell_test(formula = Haptophytes ~ Treatment,
                                      data = bioassay4_data, 
                                      conf.level = 0.95, 
                                      detailed = FALSE)

print(Haptophytes_GH_b4, n = 22)

#Check for normality

model_4.9 = lm(Haptophytes ~ Treatment, data = bioassay4_data)

ggqqplot(residuals(model_4.9))

shapiro_test(residuals(model_4.9))

#check for homogeneity of variances

leveneTest(bioassay4_data$Haptophytes ~ bioassay4_data$Treatment)
