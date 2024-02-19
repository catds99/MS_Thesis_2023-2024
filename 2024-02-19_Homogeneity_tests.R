#2024-02-19

#check for normality and homogeneity 
#Total Chl a and individual groups

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

pc_abundances = read_excel("data/PhytoClass_data.xlsx", sheet = "Absolute_Abundances")

glimpse(pc_abundances)

t0_total_1 = pc_abundances %>%
  select("Total_Chl_a", "Bioassay", "Treatment") %>%
  filter(Bioassay == "May",
         Treatment == "T0") %>%
  drop(Bioassay, Treatment)

ks.test(t0_total_1, "Total_Chl_a")

control_total_1 = pc_abundances %>%
  select("Total_Chl_a", "Bioassay", "Treatment") %>%
  filter(Bioassay == "May",
         Treatment == "T0")






result = leveneTest(Total_Chl_a ~ interaction(Treatment, Bioassay), 
                    data = pc_abundances)
print(result)

cyano_levene = leveneTest(Cyanobacteria ~ interaction(Treatment, Bioassay), 
                    data = pc_abundances)
print(cyano_levene)

ga_levene = leveneTest(Green_Algae ~ interaction(Treatment, Bioassay), 
                          data = pc_abundances)
print(ga_levene)

crypto_levene = leveneTest(Cryptophytes ~ interaction(Treatment, Bioassay), 
                       data = pc_abundances)
print(crypto_levene)

diatoms_levene = leveneTest(Diatoms ~ interaction(Treatment, Bioassay), 
                           data = pc_abundances)
print(diatoms_levene)

dino_levene = leveneTest(Dinoflagellates ~ interaction(Treatment, Bioassay), 
                            data = pc_abundances)
print(dino_levene)

hapto_levene = leveneTest(Haptophytes ~ interaction(Treatment, Bioassay), 
                         data = pc_abundances)
print(hapto_levene)

###########################################transformed

pc_abundances_transformed = read_excel("data/PhytoClass_data.xlsx", sheet = "transformed")

glimpse(pc_abundances_transformed)

result_ln = leveneTest(ln_Total_Chl_a ~ interaction(Treatment, Bioassay), 
                    data = pc_abundances_transformed)
print(result_ln)

cyano_levene_ln = leveneTest(ln_Cyanobacteria ~ interaction(Treatment, Bioassay), 
                          data = pc_abundances_transformed)
print(cyano_levene_ln)

ga_levene_ln = leveneTest(ln_Green_Algae ~ interaction(Treatment, Bioassay), 
                       data = pc_abundances_transformed)
print(ga_levene_ln)

crypto_levene_ln = leveneTest(ln_Cryptophytes ~ interaction(Treatment, Bioassay), 
                           data = pc_abundances_transformed)
print(crypto_levene_ln)

diatoms_levene_ln = leveneTest(ln_Diatoms ~ interaction(Treatment, Bioassay), 
                            data = pc_abundances_transformed)
print(diatoms_levene_ln)

dino_levene_ln = leveneTest(ln_Dinoflagellates ~ interaction(Treatment, Bioassay), 
                         data = pc_abundances_transformed)
print(dino_levene_ln)

hapto_levene_ln = leveneTest(ln_Haptophytes ~ interaction(Treatment, Bioassay), 
                          data = pc_abundances_transformed)
print(hapto_levene_ln)
