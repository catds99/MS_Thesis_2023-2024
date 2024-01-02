#2023-12-22 
#MANOVAS for community composition with PhytoClass Data

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)
library(ggpubr)
library(broom)

#import data:

all_groups_percent_changes = read_excel("data/PhytoClass_Data.xlsx", sheet = "percent_change")

summary(all_groups_percent_changes)
glimpse(all_groups_percent_changes)

#MANOVA

all_groups_model = lm(cbind(Cyanobacteria, Green_Algae, Cryptophytes, Diatoms, Dinoflagellates, Haptophytes) ~ Treatment, 
                      data = all_groups_percent_changes)
Manova(all_groups_model, test.statistic = "Pillai")

manova_result = manova(cbind(Cyanobacteria, Green_Algae, Cryptophytes, Diatoms, Dinoflagellates, Haptophytes) ~ Treatment, 
                       data = all_groups_percent_changes)
summary(manova_result)

#####check assumptions

#univariate normality

univariate_norm = all_groups_percent_changes %>%
  group_by(Treatment) %>%
  shapiro_test(Cyanobacteria, Green_Algae, Cryptophytes, Diatoms, Dinoflagellates, Haptophytes) %>%
  arrange(variable)

print(univariate_norm, n = 40)

#multivariate normality

all_groups_percent_changes %>%
  select(Cyanobacteria, Green_Algae, Cryptophytes, Diatoms, Dinoflagellates, Haptophytes) %>%
  mshapiro_test()

#multicolinearity

multicolinearity = all_groups_percent_changes %>% 
  cor_test(Cyanobacteria, Green_Algae, Cryptophytes, Diatoms, Dinoflagellates, Haptophytes)

print(multicolinearity, n = 64)

#homogeneity of covariances

box_m(all_groups_percent_changes[, c("Cyanobacteria", "Green_Algae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes")], all_groups_percent_changes$Treatment)

#Homogeneity of variance

all_groups_percent_changes %>% 
  gather(key = "variable", value = "value", Cyanobacteria, Green_Algae, Cryptophytes, Diatoms, Dinoflagellates, Haptophytes) %>%
  group_by(variable) %>%
  levene_test(value ~ Treatment)


############################################ Two way MANOVA for concentrations

#import data:

pc_all_groups_biomass = read_excel("data/PhytoClass_data.xlsx", sheet = "Absolute_Abundances")

summary(pc_all_groups_biomass)
glimpse(pc_all_groups_biomass)

#MANOVA

# model = lm(DV ~ main_effect + blocking_factor, data = data)

all_groups_biomass_model = lm(cbind(Cyanobacteria, Green_Algae, Cryptophytes, Diatoms, Dinoflagellates, Haptophytes) ~ Treatment + Bioassay, 
                              data = pc_all_groups_biomass)
Manova(all_groups_biomass_model, test.statistic = "Pillai")

biomass_manova_result = manova(cbind(Cyanobacteria, Green_Algae, Cryptophytes, Diatoms, Dinoflagellates, Haptophytes) ~ Bioassay + Treatment, 
                               data = pc_all_groups_biomass)
summary(biomass_manova_result)

#####check assumptions

#univariate normality

univariate_norm = pc_all_groups_biomass %>%
  group_by(Treatment) %>%
  shapiro_test(Cyanobacteria, Green_Algae, Cryptophytes, Diatoms, Dinoflagellates, Haptophytes) %>%
  arrange(variable)

print(univariate_norm, n = 40)

#multivariate normality

pc_all_groups_biomass %>%
  select(Cyanobacteria, Green_Algae, Cryptophytes, Diatoms, Dinoflagellates, Haptophytes) %>%
  mshapiro_test()

#multicolinearity

multicolinearity = pc_all_groups_biomass %>% 
  cor_test(Cyanobacteria, Green_Algae, Cryptophytes, Diatoms, Dinoflagellates, Haptophytes)

print(multicolinearity, n = 64)

#homogeneity of covariances

box_m(pc_all_groups_biomass[, c("Cyanobacteria", "Green_Algae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes")], pc_all_groups_biomass$Treatment)

#Homogeneity of variance

pc_all_groups_biomass %>% 
  gather(key = "variable", value = "value", Cyanobacteria, Green_Algae, Cryptophytes, Diatoms, Dinoflagellates, Haptophytes) %>%
  group_by(variable) %>%
  levene_test(value ~ Treatment)

