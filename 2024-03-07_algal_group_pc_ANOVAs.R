#2024-03-07
#stats for individual algal groups

install.packages("tidyverse")
install.packages("dplyr")
install.packages("lubridate")
install.packages("readxl")
install.packages("ggplot2")
install.packages("car")
install.packages("rstatix")
install.packages("ggpubr")
install.packages("broom")
install.packages("agricolae")

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


######################################## import data

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












################################################# Total Chl a

############### two-way anova biomass

tca.aov.1 = aov(Total_Chl_a ~ Treatment + factor(Bioassay), data = biomass)

summary(tca.aov.1)

############### one-way anova percent change

tca.aov.2 = aov(Total_Chl_a_4 ~ Treatment_3, data = percent_change)

summary(tca.aov.2)

############### two-way anova percent change

tca.aov.3 = aov(Total_Chl_a_4 ~ Treatment_3 + factor(Bioassay_3), data = percent_change)

summary(tca.aov.3)

############### post-hoc

REGW_tca = REGW.test(y = tca.aov.2, "Treatment_3", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
    #to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(REGW_tca)
    #DIN groups significantly greater than LP and HP, DIN+HP greater than DIN

REGW_tca.2 = REGW.test(y = tca.aov.3, "Treatment_3", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
    #to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(REGW_tca.2)
    #DIN groups significantly greater than LP and HP










################################################# Cyanobacteria

############### two-way anova biomass

cyano.aov.1 = aov(Cyanobacteria ~ Treatment + factor(Bioassay), data = biomass)

summary(cyano.aov.1)

############### one-way anova percent change

cyano.aov.2 = aov(Cyanobacteria_4 ~ Treatment_3, data = percent_change)

summary(cyano.aov.2)

############### two-way anova percent change

cyano.aov.3 = aov(Cyanobacteria_4 ~ Treatment_3 + factor(Bioassay_3), data = percent_change)

summary(cyano.aov.3)







################################################# Cryptophytes

############### two-way anova biomass

crypto.aov.1 = aov(Cryptophytes ~ Treatment + factor(Bioassay), data = biomass)

summary(crypto.aov.1)

############### one-way anova percent change

crypto.aov.2 = aov(Cryptophytes_4 ~ Treatment_3, data = percent_change)

summary(crypto.aov.2)

############### two-way anova percent change

crypto.aov.3 = aov(Cryptophytes_4 ~ Treatment_3 + factor(Bioassay_3), data = percent_change)

summary(crypto.aov.3)

############### post-hoc

REGW_crypto = REGW.test(y = crypto.aov.2, "Treatment_3", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
    #to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(REGW_crypto)
    #DIN groups significantly greater than LP and HP

REGW_crypto.2 = REGW.test(y = crypto.aov.3, "Treatment_3", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
    #to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(REGW_crypto.2)
    #DIN groups significantly greater than LP and HP









################################################# Diatoms

############### two-way anova biomass

diatom.aov.1 = aov(Diatoms ~ Treatment + factor(Bioassay), data = biomass)

summary(diatom.aov.1)

############### one-way anova percent change

diatom.aov.2 = aov(Diatoms_4 ~ Treatment_3, data = percent_change)

summary(diatom.aov.2)

############### two-way anova percent change

diatom.aov.3 = aov(Diatoms_4 ~ Treatment_3 + factor(Bioassay_3), data = percent_change)

summary(diatom.aov.3)

############### post-hoc

REGW_diatom = REGW.test(y = diatom.aov.2, "Treatment_3", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
    #to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(REGW_diatom)
    #DIN groups significantly greater than LP and HP

REGW_diatom.2 = REGW.test(y = diatom.aov.3, "Treatment_3", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
    #to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(REGW_diatom.2)
    #DIN groups significantly greater than LP and HP, DIN+HP close to being greater than DIN alone










################################################# Dinoflagellates

############### two-way anova biomass

dino.aov.1 = aov(Dinoflagellates ~ Treatment + factor(Bioassay), data = biomass)

summary(dino.aov.1)

############### one-way anova percent change

dino.aov.2 = aov(Dinoflagellates_4 ~ Treatment_3, data = percent_change)

summary(dino.aov.2)

############### two-way anova percent change

dino.aov.3 = aov(Dinoflagellates_4 ~ Treatment_3 + factor(Bioassay_3), data = percent_change)

summary(dino.aov.3)

############### post-hoc

REGW_dino = REGW.test(y = dino.aov.2, "Treatment_3", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
    #to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(REGW_dino)
    #DIN+HP significantly greater than LP and HP, DIN+LP almost greater than HP

REGW_dino.2 = REGW.test(y = dino.aov.3, "Treatment_3", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
    #to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(REGW_dino.2)
    #DIN combined groups significantly greater than LP and HP, DIN greater than HP almost greater than LP











################################################# Green Algae

############### two-way anova biomass

ga.aov.1 = aov(Green_Algae ~ Treatment + factor(Bioassay), data = biomass)

summary(ga.aov.1)

############### one-way anova percent change

ga.aov.2 = aov(Green_Algae_4 ~ Treatment_3, data = percent_change)

summary(ga.aov.2)

############### two-way anova percent change

ga.aov.3 = aov(Green_Algae_4 ~ Treatment_3 + factor(Bioassay_3), data = percent_change)

summary(ga.aov.3)

############### post-hoc

REGW_ga = REGW.test(y = ga.aov.2, "Treatment_3", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
    #to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(REGW_ga)
    #DIN groups significantly greater than LP and HP

REGW_ga.2 = REGW.test(y = ga.aov.3, "Treatment_3", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
    #to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(REGW_ga.2)
    #DIN groups significantly greater than LP and HP










################################################# Haptophytes

############### two-way anova biomass

hapto.aov.1 = aov(Haptophytes ~ Treatment + factor(Bioassay), data = biomass)

summary(hapto.aov.1)

############### one-way anova percent change

hapto.aov.2 = aov(Haptophytes_4 ~ Treatment_3, data = percent_change)

summary(hapto.aov.2)

############### two-way anova percent change

hapto.aov.3 = aov(Haptophytes_4 ~ Treatment_3 + factor(Bioassay_3), data = percent_change)

summary(hapto.aov.3)

############### post-hoc

REGW_hapto = REGW.test(y = hapto.aov.2, "Treatment_3", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
    #to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(REGW_hapto)
    #DIN group significantly greater than DIN combined groups

REGW_hapto.2 = REGW.test(y = hapto.aov.3, "Treatment_3", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
    #to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(REGW_hapto.2)
    #DIN group significantly greater than DIN combined groups




