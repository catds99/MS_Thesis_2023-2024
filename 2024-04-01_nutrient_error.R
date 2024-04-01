#2024-02-16
#barplots for all bioassay with and without transformed data

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)


nutrients = read_excel("data/Final_nutrient_data.xlsx", sheet = "all_concentrations")

glimpse(nutrients)

nutrients_summary = nutrients %>%
  group_by(Sample_ID) %>%
  summarise(
    B1_nitrate = mean(B1_NO3),
    B1_nitrite = mean(B1_NO2),
    B1_ammonium = mean(B1_NH3),
    B1_phosphate = mean(B1_PO4),
    B2_nitrate = mean(B2_NO3),
    B2_nitrite = mean(B2_NO2),
    B2_ammonium = mean(B2_NH3),
    B2_phosphate = mean(B2_PO4),
    B3_nitrate = mean(B3_NO3),
    B3_nitrite = mean(B3_NO2),
    B3_ammonium = mean(B3_NH3),
    B3_phosphate = mean(B3_PO4),
    B4_nitrate = mean(B4_NO3),
    B4_nitrite = mean(B4_NO2),
    B4_ammonium = mean(B4_NH3),
    B4_phosphate = mean(B4_PO4),
    B1_nitrate_sd = sd(B1_NO3, na.rm = TRUE),
    B1_nitrite_sd = sd(B1_NO2, na.rm = TRUE),
    B1_ammonium_sd = sd(B1_NH3, na.rm = TRUE),
    B1_phosphate_sd = sd(B1_PO4, na.rm = TRUE),
    B2_nitrate_sd = sd(B2_NO3, na.rm = TRUE),
    B2_nitrite_sd = sd(B2_NO2, na.rm = TRUE),
    B2_ammonium_sd = sd(B2_NH3, na.rm = TRUE),
    B2_phosphate_sd = sd(B2_PO4, na.rm = TRUE),
    B3_nitrate_sd = sd(B3_NO3, na.rm = TRUE),
    B3_nitrite_sd = sd(B3_NO2, na.rm = TRUE),
    B3_ammonium_sd = sd(B3_NH3, na.rm = TRUE),
    B3_phosphate_sd = sd(B3_PO4, na.rm = TRUE),
    B4_nitrate_sd = sd(B4_NO3, na.rm = TRUE),
    B4_nitrite_sd = sd(B4_NO2, na.rm = TRUE),
    B4_ammonium_sd = sd(B4_NH3, na.rm = TRUE),
    B4_phosphate_sd = sd(B4_PO4, na.rm = TRUE)
  )

print(nutrients_summary)
