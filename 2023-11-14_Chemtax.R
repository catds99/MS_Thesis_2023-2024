#2023-11-14
#Stacked barplots using chemtax data

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)

#########################Bioassay 1

#import data

bioassay_1 = read_excel("data/Chemtax_data.xlsx", sheet = "Bioassay_1_percent")

summary(bioassay_1)
glimpse(bioassay_1)