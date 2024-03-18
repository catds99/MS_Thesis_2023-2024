#2024-03-15
#anovas for total chl a


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

phytoclass_dh =  read_excel("data/2024-03-12_percent_change_data_with_dh.xlsx", sheet = "Sheet2")

glimpse(phytoclass_dh)


tca = phytoclass_dh %>%
  dplyr::select(Bioassay_3, Treatment_3, Total_Chl_a_4)

glimpse(tca)

b1 = tca %>%
  dplyr::filter(tca$Bioassay_3 == c('May')
  )

b2 = tca %>%
  dplyr::filter(tca$Bioassay_3 == c('June')
  )

b3 = tca %>%
  dplyr::filter(tca$Bioassay_3 == c('July')
  )

b4 = tca %>%
  dplyr::filter(tca$Bioassay_3 == c('September')
  )



############### one-way anova percent change

tca.aov.2 = aov(Total_Chl_a_4 ~ Treatment_3, data = tca)

summary(tca.aov.2)

############### two-way anova percent change

tca.aov.3 = aov(Total_Chl_a_4 ~ Treatment_3 + factor(Bioassay_3), data = tca)

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






######################################### Bioassay 1

############### one-way anova percent change

b1.tca.aov = aov(Total_Chl_a_4 ~ Treatment_3, data = b1)

summary(b1.tca.aov)

REGW_b1_tca = REGW.test(y = b1.tca.aov, "Treatment_3", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)

REGW_b1_tca











######################################### Bioassay 2

############### one-way anova percent change

b2.tca.aov = aov(Total_Chl_a_4 ~ Treatment_3, data = b2)

summary(b2.tca.aov)

REGW_b2_tca = REGW.test(y = b2.tca.aov, "Treatment_3", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)

REGW_b2_tca















######################################### Bioassay 3

############### one-way anova percent change

b3.tca.aov = aov(Total_Chl_a_4 ~ Treatment_3, data = b3)

summary(b3.tca.aov)

REGW_b3_tca = REGW.test(y = b3.tca.aov, "Treatment_3", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)

REGW_b3_tca



















######################################### Bioassay 4

############### one-way anova percent change

b4.tca.aov = aov(Total_Chl_a_4 ~ Treatment_3, data = b4)

summary(b4.tca.aov)

REGW_b4_tca = REGW.test(y = b4.tca.aov, "Treatment_3", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)

REGW_b4_tca
