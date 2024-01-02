#2023-12-11
#stats for individual algal groups

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)
library(ggpubr)
library(broom)

################################################### Chlorophytes

#import data

Chlorophytes = read_excel("data/Chemtax_by_group.xlsx", sheet = "Chlorophytes_percents")

summary(Chlorophytes)
glimpse(Chlorophytes)

#Kruskal-wallace:

kruskal_chloro = kruskal.test(Percent_dif_from_control ~ Treatment, data = Chlorophytes)
  #said significant differences

#ANOVA try 1:

chloro_percent_change_ANOVA = aov(Percent_dif_from_control ~ Treatment, data = Chlorophytes)

summary(chloro_percent_change_ANOVA)
  #not significant

post_hoc_1_chloro = REGW.test(y = chloro_percent_change_ANOVA, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
  #to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_1_chloro)
  #no groups significantly different from each other

#ANOVA try 2: 

chloro_percent_change_ANOVA_2 = oneway.test(Percent_dif_from_control ~ Treatment,
                                                data = Chlorophytes,
                                                var.equal = TRUE)
  #not signifcant

#Non-parametric ANOVA (Welchs)

oneway.test(Percent_dif_from_control ~ Treatment,
            data = Chlorophytes,
            var.equal = FALSE)

  #not significant

#Games-Howell:

chloro_percent_change_GH = games_howell_test(formula = Percent_dif_from_control ~ Treatment,
                                          data = Chlorophytes, 
                                          conf.level = 0.95, 
                                          detailed = FALSE)

print(chloro_percent_change_GH, n = 22)
  #difference between DIN and LP and HP - but shouldn't use this 

#Tukey post-hoc testing (parametric)

chloro_percent_change_Tukey = tukey_hsd(chloro_percent_change_ANOVA, conf.level = 0.95)

TukeyHSD(chloro_percent_change_ANOVA)

#Check for normality

model_2 = lm(Percent_dif_from_control ~ Treatment, data = Chlorophytes)

ggqqplot(residuals(model_2))

shapiro_test(residuals(model_2))

#not normal (either way), so use non-parametric

#check for homogeneity of variances

leveneTest(Chlorophytes$Percent_dif_from_control ~ Chlorophytes$Treatment)
#this passed, so we could potentially use a parametric test


################################################### Cryptophytes

#import data

Cryptophytes = read_excel("data/Chemtax_by_group.xlsx", sheet = "Cryptophytes_percents")

summary(Cryptophytes)
glimpse(Cryptophytes)

#ANOVA try 1:

crypto_percent_change_ANOVA = aov(Percent_dif_from_control ~ Treatment, data = Cryptophytes)

summary(crypto_percent_change_ANOVA)
  #significant p = 0.00785

#ANOVA try 2: 

crypto_percent_change_ANOVA_2 = oneway.test(Percent_dif_from_control ~ Treatment,
                                            data = Cryptophytes,
                                            var.equal = TRUE)
  # signifcant, p = 0.007848

#Non-parametric ANOVA (Welchs)

oneway.test(Percent_dif_from_control ~ Treatment,
            data = Cryptophytes,
            var.equal = FALSE)

  #significant, p = 0.01756

#Kruskal-wallace:

kruskal_crypto = kruskal.test(Percent_dif_from_control ~ Treatment, data = Cryptophytes)
  # significant differences, p = 0.006161

#Games-Howell:

crypto_percent_change_GH = games_howell_test(formula = Percent_dif_from_control ~ Treatment,
                                             data = Cryptophytes, 
                                             conf.level = 0.95, 
                                             detailed = FALSE)

print(crypto_percent_change_GH, n = 22)
  #difference between DIN and LP and HP

#REGW:

post_hoc_1_crypto = REGW.test(y = crypto_percent_change_ANOVA, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
  #to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_1_crypto)
  #DIN+HP and DIN+LP different from LP and HP, DIN and HP close to being significant different, but not quite there

#Tukey

crypto_percent_change_Tukey = tukey_hsd(crypto_percent_change_ANOVA, conf.level = 0.95)

TukeyHSD(crypto_percent_change_ANOVA)
  #differences: HP-DIN+HP, HP-DIN+LP

#Check for normality

model_3 = lm(Percent_dif_from_control ~ Treatment, data = Cryptophytes)

ggqqplot(residuals(model_3))

shapiro_test(residuals(model_3))

#not normal (either way), so use non-parametric

#check for homogeneity of variances

leveneTest(Cryptophytes$Percent_dif_from_control ~ Cryptophytes$Treatment)
#this passed, so we could potentially use a parametric test

################################################### Cyanobacteria

#import data

Cyanobacteria = read_excel("data/Chemtax_by_group.xlsx", sheet = "Cyanobacteria_percents")

summary(Cyanobacteria)
glimpse(Cyanobacteria)

#ANOVA try 1:

cyano_percent_change_ANOVA = aov(Percent_dif_from_control ~ Treatment, data = Cyanobacteria)

summary(cyano_percent_change_ANOVA)
# not significant p = 0.999

#ANOVA try 2: 

cyano_percent_change_ANOVA_2 = oneway.test(Percent_dif_from_control ~ Treatment,
                                            data = Cyanobacteria,
                                            var.equal = TRUE)
# not signifcant, p = 0.9988

#Non-parametric ANOVA (Welchs)

oneway.test(Percent_dif_from_control ~ Treatment,
            data = Cyanobacteria,
            var.equal = FALSE)

# not significant, p = 0.9946

#Kruskal-wallace:

kruskal_cyano = kruskal.test(Percent_dif_from_control ~ Treatment, data = Cyanobacteria)
# no significant differences, p = 0.9976

#Games-Howell:

cyano_percent_change_GH = games_howell_test(formula = Percent_dif_from_control ~ Treatment,
                                             data = Cyanobacteria, 
                                             conf.level = 0.95, 
                                             detailed = FALSE)

print(cyano_percent_change_GH, n = 22)
#difference between DIN and LP and HP

#REGW:

post_hoc_1_cyano = REGW.test(y = cyano_percent_change_ANOVA, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_1_cyano)
#DIN+HP and DIN+LP different from LP and HP, DIN and HP close to being significant different, but not quite there

#Tukey

cyano_percent_change_Tukey = tukey_hsd(cyano_percent_change_ANOVA, conf.level = 0.95)

TukeyHSD(cyano_percent_change_ANOVA)
#differences: HP-DIN+HP, HP-DIN+LP

#Check for normality

model_4 = lm(Percent_dif_from_control ~ Treatment, data = Cyanobacteria)

ggqqplot(residuals(model_4))

shapiro_test(residuals(model_4))

#normal

#check for homogeneity of variances

leveneTest(Cyanobacteria$Percent_dif_from_control ~ Cyanobacteria$Treatment)
#did not passs

################################################### Diatoms

#import data

Diatoms = read_excel("data/Chemtax_by_group.xlsx", sheet = "Diatoms_percents")

summary(Diatoms)
glimpse(Diatoms)

#ANOVA try 1:

diatom_percent_change_ANOVA = aov(Percent_dif_from_control ~ Treatment, data = Diatoms)

summary(diatom_percent_change_ANOVA)
# significant p = 0.00946

#ANOVA try 2: 

diatom_percent_change_ANOVA_2 = oneway.test(Percent_dif_from_control ~ Treatment,
                                           data = Diatoms,
                                           var.equal = TRUE)
# signifcant, p = 0.00946

#Non-parametric ANOVA (Welchs)

oneway.test(Percent_dif_from_control ~ Treatment,
            data = Diatoms,
            var.equal = FALSE)

# significant, p = 0.00112

#Kruskal-wallace:

kruskal_diatom = kruskal.test(Percent_dif_from_control ~ Treatment, data = Diatoms)
# significant differences, p = 0.005102

#Games-Howell:

diatom_percent_change_GH = games_howell_test(formula = Percent_dif_from_control ~ Treatment,
                                            data = Diatoms, 
                                            conf.level = 0.95, 
                                            detailed = FALSE)

print(diatom_percent_change_GH, n = 22)
#difference between DIN and LP and HP

#REGW:

post_hoc_1_diatom = REGW.test(y = diatom_percent_change_ANOVA, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_1_diatom)
#DIN+HP and DIN+LP different from LP and HP, DIN and HP close to being significant different, but not quite there

#Tukey

diatom_percent_change_Tukey = tukey_hsd(diatom_percent_change_ANOVA, conf.level = 0.95)

TukeyHSD(diatom_percent_change_ANOVA)
#differences: HP-DIN+HP, HP-DIN+LP

#Check for normality

model_5 = lm(Percent_dif_from_control ~ Treatment, data = Diatoms)

ggqqplot(residuals(model_5))

shapiro_test(residuals(model_5))

#not normal (either way), so use non-parametric

#check for homogeneity of variances

leveneTest(Diatoms$Percent_dif_from_control ~ Diatoms$Treatment)
#this passed, so we could potentially use a parametric test

################################################### Dinoflagellates

#import data

Dinoflagellates = read_excel("data/Chemtax_by_group.xlsx", sheet = "Dinoflagellates_percents")

summary(Dinoflagellates)
glimpse(Dinoflagellates)

#ANOVA try 1:

dino_percent_change_ANOVA = aov(Percent_dif_from_control ~ Treatment, data = Dinoflagellates)

summary(dino_percent_change_ANOVA)
# not significant p = 0.325

#ANOVA try 2: 

dino_percent_change_ANOVA_2 = oneway.test(Percent_dif_from_control ~ Treatment,
                                            data = Dinoflagellates,
                                            var.equal = TRUE)
# not signifcant, p = 0.3246

#Non-parametric ANOVA (Welchs)

oneway.test(Percent_dif_from_control ~ Treatment,
            data = Dinoflagellates,
            var.equal = FALSE)

# not significant, p = 0.3097

#Kruskal-wallace:

kruskal_dino = kruskal.test(Percent_dif_from_control ~ Treatment, data = Dinoflagellates)
# no significant differences, p = 0.2552

#Games-Howell:

dino_percent_change_GH = games_howell_test(formula = Percent_dif_from_control ~ Treatment,
                                             data = Dinoflagellates, 
                                             conf.level = 0.95, 
                                             detailed = FALSE)

print(dino_percent_change_GH, n = 22)
  #NS

#REGW:

post_hoc_1_dino = REGW.test(y = dino_percent_change_ANOVA, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_1_dino)
  #NS

#Tukey

dino_percent_change_Tukey = tukey_hsd(dino_percent_change_ANOVA, conf.level = 0.95)

TukeyHSD(dino_percent_change_ANOVA)
  #NS

#Check for normality

model_6 = lm(Percent_dif_from_control ~ Treatment, data = Dinoflagellates)

ggqqplot(residuals(model_6))

shapiro_test(residuals(model_6))

#not normal (either way), so use non-parametric

#check for homogeneity of variances

leveneTest(Dinoflagellates$Percent_dif_from_control ~ Dinoflagellates$Treatment)
#this did not pass

################################################### Prasinophytes

#import data

Prasinophytes = read_excel("data/Chemtax_by_group.xlsx", sheet = "Prasinophytes_percents")

summary(Prasinophytes)
glimpse(Prasinophytes)

#ANOVA try 1:

prasino_percent_change_ANOVA = aov(Percent_dif_from_control ~ Treatment, data = Prasinophytes)

summary(prasino_percent_change_ANOVA)
# not significant p = 0.0932 

#ANOVA try 2: 

prasino_percent_change_ANOVA_2 = oneway.test(Percent_dif_from_control ~ Treatment,
                                          data = Prasinophytes,
                                          var.equal = TRUE)
# not signifcant, p = 0.09316

#Non-parametric ANOVA (Welchs)

oneway.test(Percent_dif_from_control ~ Treatment,
            data = Prasinophytes,
            var.equal = FALSE)

# not significant, p = 0.09746

#Kruskal-wallace:

kruskal_prasino = kruskal.test(Percent_dif_from_control ~ Treatment, data = Prasinophytes)
# no significant differences, p = 0.3796

#Games-Howell:

prasino_percent_change_GH = games_howell_test(formula = Percent_dif_from_control ~ Treatment,
                                           data = Prasinophytes, 
                                           conf.level = 0.95, 
                                           detailed = FALSE)

print(prasino_percent_change_GH, n = 22)
#NS

#REGW:

post_hoc_1_prasino = REGW.test(y = prasino_percent_change_ANOVA, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_1_prasino)
#NS

#Tukey

prasino_percent_change_Tukey = tukey_hsd(prasino_percent_change_ANOVA, conf.level = 0.95)

TukeyHSD(prasino_percent_change_ANOVA)
#NS

#Check for normality

model_7 = lm(Percent_dif_from_control ~ Treatment, data = Prasinophytes)

ggqqplot(residuals(model_7))

shapiro_test(residuals(model_7))

#not normal (either way), so use non-parametric

#check for homogeneity of variances

leveneTest(Prasinophytes$Percent_dif_from_control ~ Prasinophytes$Treatment)
#this did pass


################################################### Euglenophytes

#import data

Euglenophytes = read_excel("data/Chemtax_by_group.xlsx", sheet = "Euglenophytes_percents")

summary(Euglenophytes)
glimpse(Euglenophytes)

#ANOVA try 1:

eugleno_percent_change_ANOVA = aov(Percent_dif_from_control ~ Treatment, data = Euglenophytes)

summary(eugleno_percent_change_ANOVA)
# not significant p = 0.453 

#ANOVA try 2: 

eugleno_percent_change_ANOVA_2 = oneway.test(Percent_dif_from_control ~ Treatment,
                                             data = Euglenophytes,
                                             var.equal = TRUE)
# not signifcant, p = 0.4535

#Non-parametric ANOVA (Welchs)

oneway.test(Percent_dif_from_control ~ Treatment,
            data = Euglenophytes,
            var.equal = FALSE)

# not significant, p = 0.6712

#Kruskal-wallace:

kruskal_eugleno = kruskal.test(Percent_dif_from_control ~ Treatment, data = Euglenophytes)
# no significant differences, p = 0.9438

#Games-Howell:

eugleno_percent_change_GH = games_howell_test(formula = Percent_dif_from_control ~ Treatment,
                                              data = Euglenophytes, 
                                              conf.level = 0.95, 
                                              detailed = FALSE)

print(eugleno_percent_change_GH, n = 22)
#NS

#REGW:

post_hoc_1_eugleno = REGW.test(y = eugleno_percent_change_ANOVA, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_1_eugleno)
#NS

#Tukey

eugleno_percent_change_Tukey = tukey_hsd(eugleno_percent_change_ANOVA, conf.level = 0.95)

TukeyHSD(eugleno_percent_change_ANOVA)
#NS

#Check for normality

model_8 = lm(Percent_dif_from_control ~ Treatment, data = Euglenophytes)

ggqqplot(residuals(model_8))

shapiro_test(residuals(model_8))

#not normal (either way), so use non-parametric

#check for homogeneity of variances

leveneTest(Euglenophytes$Percent_dif_from_control ~ Euglenophytes$Treatment)
#this did pass


################################################### Haptophytes

#import data

Haptophytes = read_excel("data/Chemtax_by_group.xlsx", sheet = "Haptophytes_percents")

summary(Haptophytes)
glimpse(Haptophytes)

#ANOVA try 1:

hapto_percent_change_ANOVA = aov(Percent_dif_from_control ~ Treatment, data = Haptophytes)

summary(hapto_percent_change_ANOVA)
# not significant p = 0.353 

#ANOVA try 2: 

hapto_percent_change_ANOVA_2 = oneway.test(Percent_dif_from_control ~ Treatment,
                                             data = Haptophytes,
                                             var.equal = TRUE)
# not signifcant, p = 0.3526

#Non-parametric ANOVA (Welchs)

oneway.test(Percent_dif_from_control ~ Treatment,
            data = Haptophytes,
            var.equal = FALSE)

# not significant, p = 0.2857

#Kruskal-wallace:

kruskal_hapto = kruskal.test(Percent_dif_from_control ~ Treatment, data = Haptophytes)
# no significant differences, p = 0.2078

#Games-Howell:

hapto_percent_change_GH = games_howell_test(formula = Percent_dif_from_control ~ Treatment,
                                              data = Haptophytes, 
                                              conf.level = 0.95, 
                                              detailed = FALSE)

print(hapto_percent_change_GH, n = 22)
#NS

#REGW:

post_hoc_1_hapto = REGW.test(y = hapto_percent_change_ANOVA, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
#to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc_1_hapto)
#NS

#Tukey

hapto_percent_change_Tukey = tukey_hsd(hapto_percent_change_ANOVA, conf.level = 0.95)

TukeyHSD(hapto_percent_change_ANOVA)
#NS

#Check for normality

model_9 = lm(Percent_dif_from_control ~ Treatment, data = Haptophytes)

ggqqplot(residuals(model_9))

shapiro_test(residuals(model_9))

#not normal (either way), so use non-parametric

#check for homogeneity of variances

leveneTest(Haptophytes$Percent_dif_from_control ~ Haptophytes$Treatment)
#this did pass

