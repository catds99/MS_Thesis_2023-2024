#2023-11-14
#Stats for Chemtax

library(stats)
library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library(broom)
library(dplyr)


############################# Bioassay 1

#Import data

bioassay_1 = read_excel("data/Chemtax_data.xlsx", sheet = "Bioassay_1_concentration")

summary(bioassay_1)
glimpse(bioassay_1)

#####check assumptions

#univariate normality

bioassay_1 %>%
  group_by(Sample) %>%
  shapiro_test(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto, Total) %>%
  arrange(variable)

#multivariate normality

bioassay_1 %>%
  select(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) %>%
  mshapiro_test()

#multicolinearity

multicolinearity_b1 = bioassay_1 %>% 
  cor_test(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto)

print(multicolinearity_b1, n = 64)

#homogeneity of covariances

box_m(bioassay_1[, c("Chloro", "Crypto", "Cyano", "Diat", "Dino", "Pras", "Eugleno", "Hapto")], bioassay_1$Sample)

#Homogeneity of variance

bioassay_1 %>% 
  gather(key = "variable", value = "value", Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) %>%
  group_by(variable) %>%
  levene_test(value ~ Sample)

#run test

cbind(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) ~ Sample

manova_result_1 = manova(cbind(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) ~ Sample, data = bioassay_1)

manova_result_1

summary(manova_result_1)

summary.aov(manova_result_1)


############################# Bioassay 2

#Import data

bioassay_2 = read_excel("data/Chemtax_data.xlsx", sheet = "Bioassay_2_concentration")

summary(bioassay_2)
glimpse(bioassay_2)

#####check assumptions

#univariate normality

univariate_norm_b2 = bioassay_2 %>%
  group_by(Sample) %>%
  shapiro_test(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto, Total) %>%
  arrange(variable)

print(univariate_norm_b2, n = 63)

#multivariate normality

bioassay_2 %>%
  select(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) %>%
  mshapiro_test()

#multicolinearity

multicolinearity_b2 = bioassay_2 %>% 
  cor_test(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto)

print(multicolinearity_b2, n = 64)

#homogeneity of covariances

box_m(bioassay_1[, c("Chloro", "Crypto", "Cyano", "Diat", "Dino", "Pras", "Eugleno", "Hapto")], bioassay_1$Sample)

#Homogeneity of variance

bioassay_2 %>% 
  gather(key = "variable", value = "value", Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) %>%
  group_by(variable) %>%
  levene_test(value ~ Sample)

#run test

cbind(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) ~ Sample

manova_result_2 = manova(cbind(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) ~ Sample, data = bioassay_2)

manova_result_2

summary(manova_result_2)

summary.aov(manova_result_2)

############################# Bioassay 3

#Import data

bioassay_3 = read_excel("data/Chemtax_data.xlsx", sheet = "Bioassay_3_concentration")

summary(bioassay_3)
glimpse(bioassay_3)

#####check assumptions

#univariate normality

univariate_norm_b3 = bioassay_3 %>%
  group_by(Sample) %>%
  shapiro_test(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) %>%
  arrange(variable)

print(univariate_norm_b3, n = 63)

#multivariate normality

bioassay_3 %>%
  select(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) %>%
  mshapiro_test()

#multicolinearity

multicolinearity_b3 = bioassay_3 %>% 
  cor_test(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto)

print(multicolinearity_b3, n = 64)

#homogeneity of covariances

box_m(bioassay_3[, c("Chloro", "Crypto", "Cyano", "Diat", "Dino", "Pras", "Eugleno", "Hapto")], bioassay_3$Sample)

#Homogeneity of variance

bioassay_3 %>% 
  gather(key = "variable", value = "value", Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) %>%
  group_by(variable) %>%
  levene_test(value ~ Sample)

#run test

cbind(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) ~ Sample

manova_result_3 = manova(cbind(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) ~ Sample, data = bioassay_3)

manova_result_3

summary(manova_result_3)

summary.aov(manova_result_3)

############################# Bioassay 4

#Import data

bioassay_4 = read_excel("data/Chemtax_data.xlsx", sheet = "Bioassay_4_concentration")

summary(bioassay_4)
glimpse(bioassay_4)

#####check assumptions

#univariate normality

univariate_norm_b4 = bioassay_4 %>%
  group_by(Sample) %>%
  shapiro_test(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) %>%
  arrange(variable)

print(univariate_norm_b4, n = 63)

#multivariate normality

bioassay_4 %>%
  select(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) %>%
  mshapiro_test()

#multicolinearity

multicolinearity_b4 = bioassay_4 %>% 
  cor_test(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto)

print(multicolinearity_b4, n = 64)

#homogeneity of covariances

box_m(bioassay_4[, c("Chloro", "Crypto", "Cyano", "Diat", "Dino", "Pras", "Eugleno", "Hapto")], bioassay_3$Sample)

#Homogeneity of variance

bioassay_4 %>% 
  gather(key = "variable", value = "value", Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) %>%
  group_by(variable) %>%
  levene_test(value ~ Sample)

#run test

cbind(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) ~ Sample

manova_result_4 = manova(cbind(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) ~ Sample, data = bioassay_4)

manova_result_4

summary(manova_result_4)

summary.aov(manova_result_4)

