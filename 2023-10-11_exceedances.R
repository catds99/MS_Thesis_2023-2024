#graphing DIN:DIP > 25 exceedances in NI
#2023-10-11

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)

install.packages("ggpubr")
library(ggpubr)

#import data

clambank_nutrients = read_excel("data/Clambank 3d Data.xlsx", sheet = "DINDIP25")

summary(clambank_nutrients)
glimpse(clambank_nutrients)

#plot exceedances

DIN_to_DIP_exceedances = ggplot(data = clambank_nutrients) +
  geom_line(aes(x = Year, y = Percent_of_samples)) +
  labs(x = "Year", y = "Percent of samples with DIN:DIP > 25") +
  theme_classic(base_size = 25)
ggsave(DIN_to_DIP_exceedances, filename = "figures/DIN_to_DIP_exceedances.png",
       device = "png", height = 7, width = 11)
