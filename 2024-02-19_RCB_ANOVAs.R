#2024-02-19

#RCB ANOVAS
#Total Chl a and individual groups
#DV = biomass or percent change, main effect = treatment, block = bioassay

#model code:

# model = lm(DV ~ main_effect + factor(blocking_factor), data = data)

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)
library(ggpubr)
library(broom)

############################################Biomass

#import data:

pc_abundances = read_excel("data/PhytoClass_data.xlsx", sheet = "Absolute_Abundances")

glimpse(pc_abundances)

############Total Chl a

total_chl_a_aov = aov(Total_Chl_a ~ Treatment + factor(Bioassay), data = pc_abundances)

summary(total_chl_a_aov)

############Cyanobacteria

cyano_aov = aov(Cyanobacteria ~ Treatment + factor(Bioassay), data = pc_abundances)

summary(cyano_aov)

############Green Algae

ga_aov = aov(Green_Algae ~ Treatment + factor(Bioassay), data = pc_abundances)

summary(ga_aov)

############Cryptophytes

crypto_aov = aov(Cryptophytes ~ Treatment + factor(Bioassay), data = pc_abundances)

summary(crypto_aov)

############Diatoms

diatoms_aov = aov(Diatoms ~ Treatment + factor(Bioassay), data = pc_abundances)

summary(diatoms_aov)

############Dinoflagellates

dino_aov = aov(Dinoflagellates ~ Treatment + factor(Bioassay), data = pc_abundances)

summary(dino_aov)

############Haptophytes

hapto_aov = aov(Haptophytes ~ Treatment + factor(Bioassay), data = pc_abundances)

summary(hapto_aov)

############################################Biomass transformed

#import data:

pc_abundances_transformed = read_excel("data/PhytoClass_data.xlsx", sheet = "transformed")

glimpse(pc_abundances_transformed)

############Total Chl a

ln_total_chl_a_aov = aov(ln_Total_Chl_a ~ Treatment + factor(Bioassay), data = pc_abundances_transformed)

summary(ln_total_chl_a_aov)

############Cyanobacteria

ln_cyano_aov = aov(ln_Cyanobacteria ~ Treatment + factor(Bioassay), data = pc_abundances_transformed)

summary(ln_cyano_aov)

############Green Algae

ln_ga_aov = aov(ln_Green_Algae ~ Treatment + factor(Bioassay), data = pc_abundances_transformed)

summary(ln_ga_aov)

############Cryptophytes

ln_crypto_aov = aov(ln_Cryptophytes ~ Treatment + factor(Bioassay), data = pc_abundances_transformed)

summary(ln_crypto_aov)

############Diatoms

ln_diatoms_aov = aov(ln_Diatoms ~ Treatment + factor(Bioassay), data = pc_abundances_transformed)

summary(ln_diatoms_aov)

############Dinoflagellates

ln_dino_aov = aov(ln_Dinoflagellates ~ Treatment + factor(Bioassay), data = pc_abundances_transformed)

summary(ln_dino_aov)

############Haptophytes

ln_hapto_aov = aov(ln_Haptophytes ~ Treatment + factor(Bioassay), data = pc_abundances_transformed)

summary(ln_hapto_aov)

############################################Percent Change

#import data:

pc_percent_change = read_excel("data/PhytoClass_data.xlsx", sheet = "percent_change")

glimpse(pc_percent_change)

############Total Chl a

pc_total_chl_a_aov = aov(Total_Chl_a ~ Treatment + factor(Bioassay), data = pc_percent_change)

summary(pc_total_chl_a_aov)

############Cyanobacteria

pc_cyano_aov = aov(Cyanobacteria ~ Treatment + factor(Bioassay), data = pc_percent_change)

summary(pc_cyano_aov)

############Green_Algae

pc_ga_aov = aov(Green_Algae ~ Treatment + factor(Bioassay), data = pc_percent_change)

summary(pc_ga_aov)

############Cryptophytes

pc_crypto_aov = aov(Cryptophytes ~ Treatment + factor(Bioassay), data = pc_percent_change)

summary(pc_crypto_aov)

############Diatoms

pc_diatoms_aov = aov(Diatoms ~ Treatment + factor(Bioassay), data = pc_percent_change)

summary(pc_diatoms_aov)

############Dinoflagellates

pc_dino_aov = aov(Dinoflagellates ~ Treatment + factor(Bioassay), data = pc_percent_change)

summary(pc_dino_aov)

############Haptophytes

pc_hapto_aov = aov(Haptophytes ~ Treatment + factor(Bioassay), data = pc_percent_change)

summary(pc_hapto_aov)

