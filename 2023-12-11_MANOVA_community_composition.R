#2023-12-11 
#MANOVAS for community composition

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

all_groups_percent_dif = read_excel("data/Chemtax_by_Group.xlsx", sheet = "all_groups_percent_dif")

summary(all_groups_percent_dif)
glimpse(all_groups_percent_dif)

#MANOVA

all_groups_model = lm(cbind(Chlorophytes, Cryptophytes, Cyanobacteria, Diatoms, Dinoflagellates,
                            Prasinophytes, Euglenophytes, Haptophytes) ~ Treatment, 
                      data = all_groups_percent_dif)
Manova(all_groups_model, test.statistic = "Pillai")

manova_result = manova(cbind(Chlorophytes, Cryptophytes, Cyanobacteria, Diatoms, Dinoflagellates,
                              Prasinophytes, Euglenophytes, Haptophytes) ~ Treatment, 
                        data = all_groups_percent_dif)
summary(manova_result)


#####check assumptions

#univariate normality

univariate_norm = all_groups_percent_dif %>%
  group_by(Treatment) %>%
  shapiro_test(Chlorophytes, Cryptophytes, Cyanobacteria, Diatoms, Dinoflagellates,
               Prasinophytes, Euglenophytes, Haptophytes) %>%
  arrange(variable)

print(univariate_norm, n = 40)

#multivariate normality

all_groups_percent_dif %>%
  select(Chlorophytes, Cryptophytes, Cyanobacteria, Diatoms, Dinoflagellates,
         Prasinophytes, Euglenophytes, Haptophytes) %>%
  mshapiro_test()

#multicolinearity

multicolinearity = all_groups_percent_dif %>% 
  cor_test(Chlorophytes, Cryptophytes, Cyanobacteria, Diatoms, Dinoflagellates,
                                    Prasinophytes, Euglenophytes, Haptophytes)

print(multicolinearity, n = 64)

#homogeneity of covariances

box_m(all_groups_percent_dif[, c("Chlorophytes", "Cryptophytes", "Cyanobacteria", "Diatoms", "Dinoflagellates", "Prasinophytes", "Euglenophytes", "Haptophytes")], all_groups_percent_dif$Treatment)

#Homogeneity of variance

all_groups_percent_dif %>% 
  gather(key = "variable", value = "value", Chlorophytes, Cryptophytes, Cyanobacteria, Diatoms, Dinoflagellates,
         Prasinophytes, Euglenophytes, Haptophytes) %>%
  group_by(variable) %>%
  levene_test(value ~ Treatment)


############################################ Two way MANOVA for concentrations

#import data:

all_groups_biomass = read_excel("data/data_for_2_factor_anova.xlsx", sheet = "biomass")

summary(all_groups_biomass)
glimpse(all_groups_biomass)

#MANOVA

# model = lm(DV ~ main_effect + blocking_factor, data = data)

all_groups_biomass_model = lm(cbind(Chlorophytes, Cryptophytes, Cyanobacteria, Diatoms, Dinoflagellates,
                            Prasinophytes, Euglenophytes, Haptophytes) ~ Treatment + Bioassay, 
                      data = all_groups_biomass)
Manova(all_groups_biomass_model, test.statistic = "Pillai")

biomass_manova_result = manova(cbind(Chlorophytes, Cryptophytes, Cyanobacteria, Diatoms, Dinoflagellates,
                             Prasinophytes, Euglenophytes, Haptophytes) ~ Bioassay + Treatment, 
                       data = all_groups_biomass)
summary(biomass_manova_result)

#####check assumptions

#univariate normality

univariate_norm = all_groups_biomass %>%
  group_by(Treatment) %>%
  shapiro_test(Chlorophytes, Cryptophytes, Cyanobacteria, Diatoms, Dinoflagellates,
               Prasinophytes, Euglenophytes, Haptophytes) %>%
  arrange(variable)

print(univariate_norm, n = 40)

#multivariate normality

all_groups_percent_dif %>%
  select(Chlorophytes, Cryptophytes, Cyanobacteria, Diatoms, Dinoflagellates,
         Prasinophytes, Euglenophytes, Haptophytes) %>%
  mshapiro_test()

#multicolinearity

multicolinearity = all_groups_percent_dif %>% 
  cor_test(Chlorophytes, Cryptophytes, Cyanobacteria, Diatoms, Dinoflagellates,
           Prasinophytes, Euglenophytes, Haptophytes)

print(multicolinearity, n = 64)

#homogeneity of covariances

box_m(all_groups_percent_dif[, c("Chlorophytes", "Cryptophytes", "Cyanobacteria", "Diatoms", "Dinoflagellates", "Prasinophytes", "Euglenophytes", "Haptophytes")], all_groups_percent_dif$Treatment)

#Homogeneity of variance

all_groups_percent_dif %>% 
  gather(key = "variable", value = "value", Chlorophytes, Cryptophytes, Cyanobacteria, Diatoms, Dinoflagellates,
         Prasinophytes, Euglenophytes, Haptophytes) %>%
  group_by(variable) %>%
  levene_test(value ~ Treatment)

