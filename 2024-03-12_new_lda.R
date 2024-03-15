#2024-03-08
#LDA with phytoclass data

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
library(MASS)


#import data:

phytoclass = read_excel("data/2024-03-12_percent_change_data_with_dh.xlsx", sheet = "Sheet2")

glimpse(phytoclass)

biomass = subset(phytoclass, select = c(Treatment, 
                                        Cyanobacteria, Green_Algae, 
                                        Cryptophytes, Dinoflagellates, dh))

glimpse(biomass)

percent_change = subset(phytoclass, select = c(Treatment_3, 
                                             Cyanobacteria_4, Green_Algae_4, 
                                             Cryptophytes_4, Dinoflagellates_4, dh_4))

glimpse(percent_change)

percent_change <- percent_change[-c(101:140), ]

percent_change$Cyanobacteria_4[is.na(percent_change$Cyanobacteria_4)] <- 0


#####LDA biomass

biomass_lda <- lda(Treatment ~ ., data=biomass)
biomass_lda

print(biomass_lda)

biomass_lda_values <- predict(biomass_lda)

# Model accuracy
mean(biomass_lda_values$class==biomass$Treatment)

ldahist(data = biomass_lda_values$x[,1], g=biomass$Treatment)


#plot(pc_abundances_lda_values$x[,1],pc_abundances_lda_values$x[,2]) # make a scatterplot
#text(pc_abundances_lda_values$x[,1],pc_abundances_lda_values$x[,2],pc_abundances$Treatment,cex=0.7,pos=4,col="red")

####################################plot

newdata = data.frame(type = biomass[,1], lda = biomass_lda_values$x)

newdata$Treatment = factor(newdata$Treatment, levels=c("T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"),
                           labels = c("Time Zero", "Control", "DIN", "LP", "HP", "DIN + LP", "DIN + HP")) 

treatment_colors = c("Time Zero" = "darkorchid", "Control" = "plum", "DIN" = "yellow", "LP" = "lightskyblue", "HP" = "royalblue4", "DIN + LP" = "darkolivegreen3", "DIN + HP" = "darkolivegreen")

lda_plot = ggplot(newdata) + 
  geom_point(aes(lda.LD1, lda.LD2, colour = Treatment), size = 2.5) +
  scale_color_manual(values = treatment_colors) +
  ylab("Function 2") + 
  xlab("Function 1") +
  theme_classic(base_size = 25)
ggsave(lda_plot, filename = "figures/PhytoClass/lda_plot.png",
       device = "png", height = 7, width = 11)











#####LDA percent change

percent_change_lda <- lda(Treatment_3 ~ ., data=percent_change)
percent_change_lda

print(percent_change_lda)

percent_change_lda_values <- predict(percent_change_lda)

# Model accuracy
mean(percent_change_lda_values$class==percent_change$Treatment_3)

ldahist(data = percent_change_lda_values$x[,1], g=percent_change$Treatment_3)


#plot(pc_abundances_lda_values$x[,1],pc_abundances_lda_values$x[,2]) # make a scatterplot
#text(pc_abundances_lda_values$x[,1],pc_abundances_lda_values$x[,2],pc_abundances$Treatment,cex=0.7,pos=4,col="red")

####################################plot

newdata.2 = data.frame(type = percent_change[,1], lda = percent_change_lda_values$x)

newdata.2$Treatment_3 = factor(newdata.2$Treatment_3, levels=c("DIN", "LP", "HP", "DIN_LP", "DIN_HP"),
                           labels = c("DIN", "LP", "HP", "DIN + LP", "DIN + HP")) 

treatment_colors.2 = c("DIN" = "goldenrod", "LP" = "lightskyblue", "HP" = "royalblue4", "DIN + LP" = "darkolivegreen3", "DIN + HP" = "darkolivegreen")

lda_plot_percent_change_dh = ggplot(newdata.2) + 
  geom_point(aes(lda.LD1, lda.LD2, colour = Treatment_3), size = 2.5) +
  scale_color_manual(name = "Treatment",
                     values = treatment_colors.2) +
  ylab("Function 2") + 
  xlab("Function 1") +
  theme_classic(base_size = 25)
ggsave(lda_plot_percent_change_dh, filename = "figures/PhytoClass/lda_plot_percent_change_dh.png",
       device = "png", height = 10, width = 15)










################################################################ relative abundance




#import data:

rel_abundance = read_excel("data/2024-03-11_relative_abundance.xlsx", sheet = "Sheet1")

glimpse(rel_abundance)

ra = subset(rel_abundance, select = c(Treatment, 
                                      percent_cyano, percent_ga, 
                                      percent_crypto, percent_diatom, 
                                      percent_dino, percent_hapto))

glimpse(ra)

#####LDA relative abundance

relative_abundance_lda <- lda(Treatment ~ ., data=ra)
relative_abundance_lda

print(relative_abundance_lda)

relative_abundance_lda_values <- predict(relative_abundance_lda)

# Model accuracy
mean(relative_abundance_lda_values$class==ra$Treatment)

ldahist(data = relative_abundance_lda_values$x[,1], g=ra$Treatment)


#plot(pc_abundances_lda_values$x[,1],pc_abundances_lda_values$x[,2]) # make a scatterplot
#text(pc_abundances_lda_values$x[,1],pc_abundances_lda_values$x[,2],pc_abundances$Treatment,cex=0.7,pos=4,col="red")

####################################plot

new = data.frame(type = ra[,1], lda = relative_abundance_lda_values$x)

new$Treatment = factor(new$Treatment, levels=c("T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"),
                           labels = c("Time Zero", "Control", "DIN", "LP", "HP", "DIN + LP", "DIN + HP")) 

treatment_colors = c("Time Zero" = "darkorchid", "Control" = "plum", "DIN" = "goldenrod", "LP" = "lightskyblue", "HP" = "royalblue4", "DIN + LP" = "darkolivegreen3", "DIN + HP" = "darkolivegreen")

rel_abundance_lda_plot = ggplot(new) + 
  geom_point(aes(lda.LD1, lda.LD2, colour = Treatment), size = 2.5) +
  scale_color_manual(values = treatment_colors) +
  ylab("Function 2") + 
  xlab("Function 1") +
  theme_classic(base_size = 25)
ggsave(rel_abundance_lda_plot, filename = "figures/PhytoClass/rel_abundance_lda_plot.png",
       device = "png", height = 7, width = 11)




