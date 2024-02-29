#2024-02-22 
#PERMANOVAS for community composition with PhytoClass Data, new percent cahnge data

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)
library(ggpubr)
library(broom)

install.packages("vegan")

library(vegan)

##################################import data

data = read_excel("data/2024-02-20_Percent_change_data.xlsx", sheet = "2")

glimpse(data)

###################One way permanovas

###DV = biomass, factor = treatment

biomass_1_way = adonis2(data[ , c("Cyanobacteria", "Green_Algae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes")] ~ Treatment,
                        data = data,
                        method = "euc")

print(biomass_1_way)

###DV = percent change, factor = treatment

pc_data = read_excel("data/2024-02-20_Percent_change_data.xlsx", sheet = "Sheet3")

glimpse(pc_data)

percent_change_1_way = adonis2(pc_data[ , c("Cyanobacteria_4", "Green_Algae_4", "Cryptophytes_4", "Diatoms_4", "Dinoflagellates_4", "Haptophytes_4")] ~ Treatment_3,
                        data = pc_data,
                        method = "euc")

print(percent_change_1_way)

percent_change_1_way_decimal = adonis2(pc_data[ , c("Cyanobacteria_3", "Green_Algae_3", "Cryptophytes_3", "Diatoms_3", "Dinoflagellates_3", "Haptophytes_3")] ~ Treatment_3,
                               data = pc_data,
                               method = "euc")

print(percent_change_1_way_decimal)
























###################two way permanovas

###DV = biomass, factor = treatment, block = bioassay

biomass_2_way = adonis2(data[ , c("Cyanobacteria", "Green_Algae", "Cryptophytes", "Diatoms", "Dinoflagellates", "Haptophytes")] ~ Treatment + factor(Bioassay),
                        data = data,
                        method = "euc")

print(biomass_2_way)

percent_change_2_way = adonis2(pc_data[ , c("Cyanobacteria_4", "Green_Algae_4", "Cryptophytes_4", "Diatoms_4", "Dinoflagellates_4", "Haptophytes_4")] ~ Treatment_3 + factor(Bioassay_3),
                               data = pc_data,
                               method = "euc")

print(percent_change_2_way)

percent_change_2_way_decimal = adonis2(pc_data[ , c("Cyanobacteria_3", "Green_Algae_3", "Cryptophytes_3", "Diatoms_3", "Dinoflagellates_3", "Haptophytes_3")] ~ Treatment_3 + factor(Bioassay_3),
                                       data = pc_data,
                                       method = "euc")

print(percent_change_2_way_decimal)


