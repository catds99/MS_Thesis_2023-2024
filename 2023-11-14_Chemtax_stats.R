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
  shapiro_test(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) 

shapiro.test(bioassay_1$Chloro)

bioassay_1 %>%
  select(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) %>%
  mshapiro_test()

#homogeneity of covariances

box_m(bioassay_1[, c("Chloro", "Crypto", "Cyano", "Diat", "Dino", "Pras", "Eugleno", "Hapto")], bioassay_1$Sample)

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

bioassay_1 %>%
  group_by(Sample) %>%
  shapiro_test(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) 

shapiro.test(bioassay_1$Chloro)

bioassay_1 %>%
  select(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) %>%
  mshapiro_test()

#homogeneity of covariances

box_m(bioassay_1[, c("Chloro", "Crypto", "Cyano", "Diat", "Dino", "Pras", "Eugleno", "Hapto")], bioassay_1$Sample)

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

bioassay_1 %>%
  group_by(Sample) %>%
  shapiro_test(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) 

shapiro.test(bioassay_1$Chloro)

bioassay_1 %>%
  select(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) %>%
  mshapiro_test()

#homogeneity of covariances

box_m(bioassay_1[, c("Chloro", "Crypto", "Cyano", "Diat", "Dino", "Pras", "Eugleno", "Hapto")], bioassay_1$Sample)

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

bioassay_1 %>%
  group_by(Sample) %>%
  shapiro_test(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) 

shapiro.test(bioassay_1$Chloro)

bioassay_1 %>%
  select(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) %>%
  mshapiro_test()

#homogeneity of covariances

box_m(bioassay_1[, c("Chloro", "Crypto", "Cyano", "Diat", "Dino", "Pras", "Eugleno", "Hapto")], bioassay_1$Sample)

#run test

cbind(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) ~ Sample

manova_result_4 = manova(cbind(Chloro, Crypto, Cyano, Diat, Dino, Pras, Eugleno, Hapto) ~ Sample, data = bioassay_4)

manova_result_4

summary(manova_result_4)

summary.aov(manova_result_4)

