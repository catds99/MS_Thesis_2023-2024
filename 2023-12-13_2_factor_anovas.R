#2023-12-13
#two way ANOVAs

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

percent_dif_data = read_excel("data/data_for_2_factor_anova.xlsx", sheet = "percent_dif")

summary(percent_dif_data)
glimpse(percent_dif_data)

biomass_data = read_excel("data/data_for_2_factor_anova.xlsx", sheet = "biomass")

summary(biomass_data)
glimpse(biomass_data)

######################################################## Biomass ANOVAs

#############Total Chl a

res.aov1 = aov(Total_Chl_a ~ Bioassay * Treatment, data = biomass_data)
summary(res.aov1)

res.aov1b = aov(Total_Chl_a ~ Bioassay + Treatment, data = biomass_data)
summary(res.aov1b)

model_res.aov1b = lm(Total_Chl_a ~ Treatment + Bioassay, data = biomass_data)
anova(model_res.aov1b)

leveneTest(Total_Chl_a ~ Bioassay * Treatment, data = biomass_data)

aov_residuals1 = residuals(object = res.aov1)
shapiro.test(x = aov_residuals1)

friedman.test(y=biomass_data$Total_Chl_a, 
              groups=biomass_data$Treatment, 
              blocks=biomass_data$Bioassay)

#############Chlorophytes

res.aov2 = aov(Chlorophytes ~ Bioassay * Treatment, data = biomass_data)
summary(res.aov2)

leveneTest(Chlorophytes ~ Bioassay * Treatment, data = biomass_data)

aov_residuals2 = residuals(object = res.aov2)
shapiro.test(x = aov_residuals2)

#############Cryptophytes

res.aov3 = aov(Cryptophytes ~ Bioassay * Treatment, data = biomass_data)
summary(res.aov3)

leveneTest(Cryptophytes ~ Bioassay * Treatment, data = biomass_data)

aov_residuals3 = residuals(object = res.aov3)
shapiro.test(x = aov_residuals3)

#############Cyanobacteria

res.aov4 = aov(Cyanobacteria ~ Bioassay * Treatment, data = biomass_data)
summary(res.aov4)

leveneTest(Cyanobacteria ~ Bioassay * Treatment, data = biomass_data)

aov_residuals4 = residuals(object = res.aov4)
shapiro.test(x = aov_residuals4)

#############Diatoms

res.aov5 = aov(Diatoms ~ Bioassay * Treatment, data = biomass_data)
summary(res.aov5)

leveneTest(Diatoms ~ Bioassay * Treatment, data = biomass_data)

aov_residuals5 = residuals(object = res.aov5)
shapiro.test(x = aov_residuals5)

#############Dinoflagellates

res.aov6 = aov(Dinoflagellates ~ Bioassay * Treatment, data = biomass_data)
summary(res.aov6)

leveneTest(Dinoflagellates ~ Bioassay * Treatment, data = biomass_data)

aov_residuals6 = residuals(object = res.aov6)
shapiro.test(x = aov_residuals6)

#############Prasinophytes

res.aov7 = aov(Prasinophytes ~ Bioassay * Treatment, data = biomass_data)
summary(res.aov7)

leveneTest(Prasinophytes ~ Bioassay * Treatment, data = biomass_data)

aov_residuals7 = residuals(object = res.aov7)
shapiro.test(x = aov_residuals7)

#############Euglenophytes

res.aov8 = aov(Euglenophytes ~ Bioassay * Treatment, data = biomass_data)
summary(res.aov8)

leveneTest(Euglenophytes ~ Bioassay * Treatment, data = biomass_data)

aov_residuals8 = residuals(object = res.aov8)
shapiro.test(x = aov_residuals8)

#############Haptophytes

res.aov9 = aov(Haptophytes ~ Bioassay * Treatment, data = biomass_data)
summary(res.aov9)

leveneTest(Haptophytes ~ Bioassay * Treatment, data = biomass_data)

aov_residuals9 = residuals(object = res.aov9)
shapiro.test(x = aov_residuals9)

######################################################## Percent difference ANOVAs

#############Total Chl a

res.aov1.2 = aov(Total_Chl_a ~ Bioassay + Treatment, 
                 data = percent_dif_data)
summary(res.aov1.2)

leveneTest(Total_Chl_a ~ Bioassay * Treatment, data = percent_dif_data)

aov_residuals1.2 = residuals(object = res.aov1.2)
shapiro.test(x = aov_residuals1.2)

friedman.test(y = percent_dif_data$Total_Chl_a, 
              groups=percent_dif_data$Treatment, 
              blocks=percent_dif_data$Bioassay)

#############Chlorophytes

res.aov2.2 = aov(Chlorophytes ~ Bioassay + Treatment, 
               data = percent_dif_data)
summary(res.aov2.2)

leveneTest(Chlorophytes ~ Bioassay * Treatment, data = percent_dif_data)

aov_residuals2.2 = residuals(object = res.aov2.2)
shapiro.test(x = aov_residuals2.2)

friedman.test(y = percent_dif_data$Chlorophytes, 
              groups=percent_dif_data$Treatment, 
              blocks=percent_dif_data$Bioassay)

#############Cryptophytes

res.aov3.2 = aov(Cryptophytes ~ Bioassay + Treatment, 
               data = percent_dif_data)
summary(res.aov3.2)

leveneTest(Cryptophytes ~ Bioassay * Treatment, data = percent_dif_data)

aov_residuals3.2 = residuals(object = res.aov3.2)
shapiro.test(x = aov_residuals3.2)

friedman.test(y = percent_dif_data$Cryptophytes, 
              groups=percent_dif_data$Treatment, 
              blocks=percent_dif_data$Bioassay)

#############Cyanobacteria

res.aov4.2 = aov(Cyanobacteria ~ Bioassay + Treatment, 
               data = percent_dif_data)
summary(res.aov4.2)

leveneTest(Cyanobacteria ~ Bioassay * Treatment, data = percent_dif_data)

aov_residuals4.2 = residuals(object = res.aov4.2)
shapiro.test(x = aov_residuals4.2)

friedman.test(y = percent_dif_data$Cyanobacteria, 
              groups=percent_dif_data$Treatment, 
              blocks=percent_dif_data$Bioassay)

#############Diatoms

res.aov5.2 = aov(Diatoms ~ Bioassay + Treatment, 
               data = percent_dif_data)
summary(res.aov5.2)

leveneTest(Diatoms ~ Bioassay * Treatment, data = percent_dif_data)

aov_residuals5.2 = residuals(object = res.aov5.2)
shapiro.test(x = aov_residuals5.2)

friedman.test(y = percent_dif_data$Diatoms, 
              groups=percent_dif_data$Treatment, 
              blocks=percent_dif_data$Bioassay)

#############Dinoflagellates

res.aov6.2 = aov(Dinoflagellates ~ Bioassay + Treatment, 
               data = percent_dif_data)
summary(res.aov6.2)

leveneTest(Dinoflagellates ~ Bioassay * Treatment, data = percent_dif_data)

aov_residuals6.2 = residuals(object = res.aov6.2)
shapiro.test(x = aov_residuals6.2)

friedman.test(y = percent_dif_data$Dinoflagellates, 
              groups=percent_dif_data$Treatment, 
              blocks=percent_dif_data$Bioassay)

#############Prasinophytes

res.aov7.2 = aov(Prasinophytes ~ Bioassay + Treatment, 
               data = percent_dif_data)
summary(res.aov7.2)

leveneTest(Prasinophytes ~ Bioassay * Treatment, data = percent_dif_data)

aov_residuals7.2 = residuals(object = res.aov7.2)
shapiro.test(x = aov_residuals7.2)

friedman.test(y = percent_dif_data$Prasinophytes, 
              groups=percent_dif_data$Treatment, 
              blocks=percent_dif_data$Bioassay)

#############Euglenophytes

res.aov8.2 = aov(Euglenophytes ~ Bioassay + Treatment, 
               data = percent_dif_data)
summary(res.aov8.2)

leveneTest(Euglenophytes ~ Bioassay * Treatment, data = percent_dif_data)

aov_residuals8.2 = residuals(object = res.aov8.2)
shapiro.test(x = aov_residuals8.2)

friedman.test(y = percent_dif_data$Euglenophytes, 
              groups=percent_dif_data$Treatment, 
              blocks=percent_dif_data$Bioassay)

#############Haptophytes

res.aov9.2 = aov(Haptophytes ~ Bioassay + Treatment, 
               data = percent_dif_data)
summary(res.aov9.2)

leveneTest(Haptophytes ~ Bioassay * Treatment, data = percent_dif_data)

aov_residuals9.2 = residuals(object = res.aov9.2)
shapiro.test(x = aov_residuals9.2)

friedman.test(y = percent_dif_data$Haptophytes, 
              groups=percent_dif_data$Treatment, 
              blocks=percent_dif_data$Bioassay)
