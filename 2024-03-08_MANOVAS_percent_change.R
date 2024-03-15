#2024-03-08
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
library(agricolae)


R.version

#import data:

phytoclass = read_excel("data/2024-02-20_Percent_change_data.xlsx", sheet = "2")

glimpse(phytoclass)

###################### biomass data frame

biomass = phytoclass %>%
  select(Bioassay, Treatment, Total_Chl_a, Cyanobacteria, Green_Algae, Cryptophytes, Diatoms, Dinoflagellates, Haptophytes)

glimpse(biomass)

###################### percent change data frame

percent_change = phytoclass %>%
  select(Bioassay_3, Treatment_3, Total_Chl_a_4, Cyanobacteria_4, Green_Algae_4, Cryptophytes_4, Diatoms_4, Dinoflagellates_4, Haptophytes_4)

glimpse(percent_change)








################################ one-way MANOVA with percent change

percent_change_model = lm(cbind(Cyanobacteria_4, Green_Algae_4, Cryptophytes_4, Diatoms_4, Dinoflagellates_4, Haptophytes_4) ~ Treatment_3, 
                      data = percent_change)

Manova(percent_change_model, test.statistic = "Pillai")

#####check assumptions

#univariate normality

univariate_norm = percent_change %>%
  group_by(Treatment_3) %>%
  shapiro_test(Cyanobacteria_4, Green_Algae_4, Cryptophytes_4, Diatoms_4, Dinoflagellates_4, Haptophytes_4) %>%
  arrange(variable)

print(univariate_norm, n = 40)

#multivariate normality

percent_change %>%
  select(Cyanobacteria_4, Green_Algae_4, Cryptophytes_4, Diatoms_4, Dinoflagellates_4, Haptophytes_4) %>%
  mshapiro_test()

#multicolinearity

multicolinearity = percent_change %>% 
  cor_test(Cyanobacteria_4, Green_Algae_4, Cryptophytes_4, Diatoms_4, Dinoflagellates_4, Haptophytes_4)

print(multicolinearity, n = 64)

#homogeneity of covariances

box_m(percent_change[, c("Cyanobacteria_4", "Green_Algae_4", "Cryptophytes_4", "Diatoms_4", "Dinoflagellates_4", "Haptophytes_4")], percent_change$Treatment_3)

#Homogeneity of variance

percent_change %>% 
  gather(key = "variable", value = "value", Cyanobacteria_4, Green_Algae_4, Cryptophytes_4, Diatoms_4, Dinoflagellates_4, Haptophytes_4) %>%
  group_by(variable) %>%
  levene_test(value ~ Treatment_3)














############################################ Two way MANOVA for concentrations

#MANOVA

# model = lm(DV ~ main_effect + blocking_factor, data = data)

# model = lm(DV ~ main_effect + factor(blocking_factor), data = data)

biomass_model = lm(cbind(Cyanobacteria, Green_Algae, Cryptophytes, Diatoms, Dinoflagellates, Haptophytes) ~ Treatment + Bioassay, 
                              data = biomass)
Manova(biomass_model, test.statistic = "Pillai")

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













############################################ Two way MANOVA for percent change

#MANOVA

# model = lm(DV ~ main_effect + blocking_factor, data = data)

# model = lm(DV ~ main_effect + factor(blocking_factor), data = data)

two_way_percent_change_model = lm(cbind(Cyanobacteria_4, Green_Algae_4, Cryptophytes_4, Diatoms_4, Dinoflagellates_4, Haptophytes_4) ~ Treatment_3 + Bioassay_3, 
                   data = percent_change)
Manova(two_way_percent_change_model, test.statistic = "Pillai")

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














############################################ Two way MANOVA for percent change with combined diatoms and haptophytes

phytoclass_dh = read_excel("data/2024-03-12_percent_change_data_with_dh.xlsx", sheet = "Sheet2")

glimpse(phytoclass_dh)

percent_change_dh = subset(phytoclass_dh, select = c(Bioassay_3, Treatment_3, 
                                               Cyanobacteria_4, Green_Algae_4, 
                                               Cryptophytes_4, Dinoflagellates_4, dh_4))
glimpse(percent_change_dh)


#MANOVA

# model = lm(DV ~ main_effect + blocking_factor, data = data)

# model = lm(DV ~ main_effect + factor(blocking_factor), data = data)

two_way_percent_change_model = lm(cbind(Cyanobacteria_4, Green_Algae_4, Cryptophytes_4, dh_4, Dinoflagellates_4) ~ Treatment_3 + Bioassay_3, 
                                  data = percent_change_dh)
Manova(two_way_percent_change_model, test.statistic = "Pillai")

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

