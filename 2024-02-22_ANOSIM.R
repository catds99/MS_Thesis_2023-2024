#2024-02-22 
#anosim for community composition with PhytoClass Data, new percent cahnge data

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)
library(ggpubr)
library(broom)
library(vegan)

##################################import data

percent_change = read_excel("data/2024-02-20_Percent_change_data.xlsx", sheet = "Sheet3")

glimpse(percent_change)

percent_change_2 = read_excel("data/2024-02-20_Percent_change_data.xlsx", sheet = "Sheet4")

glimpse(percent_change_2)

###################percent change

percent = percent_change %>%
  select("Treatment_3", "Cyanobacteria_4", "Green_Algae_4", "Cryptophytes_4", "Diatoms_4", "Dinoflagellates_4", "Haptophytes_4")

glimpse(percent)

decimal = percent_change %>%
  select("Treatment_3", "Cyanobacteria_3", "Green_Algae_3", "Cryptophytes_3", "Diatoms_3", "Dinoflagellates_3", "Haptophytes_3")

glimpse(decimal)


percent_change_anosim = anosim(x = percent, grouping = percent$Treatment_3, permutations = 999)
grazing.results.anosim




percent_2 = percent_change_2 %>%
  select("Treatment_3", "Cyanobacteria_4", "Green_Algae_4", "Cryptophytes_4", "Diatoms_4", "Dinoflagellates_4", "Haptophytes_4")

glimpse(percent_2)

decimal_2 = percent_change_2 %>%
  select("Treatment_3", "Cyanobacteria_3", "Green_Algae_3", "Cryptophytes_3", "Diatoms_3", "Dinoflagellates_3", "Haptophytes_3")

glimpse(decimal_2)


percent_change_anosim = anosim(x = percent_2, 
                               grouping = percent$Treatment_3, 
                               distance = "euc",
                               permutations = 999)
print(percent_change_anosim)

