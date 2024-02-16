#2024-01-03
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

install.packages("MASS")

library(MASS)

#import data:

pc_Absolute_Abundances = read_excel("C:/Users/cathe/Desktop/MS Thesis/MS_Thesis_2023-2024/data/PhytoClass_data.xlsx", sheet = "Absolute_Abundances")

summary(pc_Absolute_Abundances)
glimpse(pc_Absolute_Abundances)

pc_abundances = subset(pc_Absolute_Abundances, select = -c(Bioassay, Sample_Number, Total_Chl_a, Bioassay_2, 
                                                           Treatment_2, mean_Total_Chl_a, 
                                                           mean_Cyanobacteria, mean_Green_Algae, 
                                                           mean_Cryptophytes, mean_Diatoms, 
                                                           mean_Dinoflagellates, mean_Haptophytes) )

glimpse(pc_abundances)

#####LDA

#pc_abundances_lda = lda(x = pc_abundances, grouping = pc_abundances$Treatment)
#pc_abundances_lda

pc_abundances_lda <- lda(Treatment ~ ., data=pc_abundances)
pc_abundances_lda

print(pc_abundances_lda)

pc_abundances_lda_values <- predict(pc_abundances_lda)

# Model accuracy
mean(pc_abundances_lda_values$class==pc_abundances$Treatment)

ldahist(data = pc_abundances_lda_values$x[,1], g=pc_abundances$Treatment)


#plot(pc_abundances_lda_values$x[,1],pc_abundances_lda_values$x[,2]) # make a scatterplot
#text(pc_abundances_lda_values$x[,1],pc_abundances_lda_values$x[,2],pc_abundances$Treatment,cex=0.7,pos=4,col="red")

####################################plot

newdata = data.frame(type = pc_abundances[,1], lda = pc_abundances_lda_values$x)

lda_plot = ggplot(newdata) + 
  geom_point(aes(lda.LD1, lda.LD2, colour = Treatment), size = 2.5) +
  ylab("Function 2") + 
  xlab("Function 1") +
  theme_classic(base_size = 14)
ggsave(lda_plot, filename = "figures/PhytoClass/lda_plot.png",
       device = "png", height = 7, width = 11)

#some attempts at adjusting legend:

scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", 
                           "DIN" = "DIN", "LP" = "LP", "HP" = "HP", 
                           "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  scale_fill_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", 
                                "DIN" = "DIN", "LP" = "LP", "HP" = "HP", 
                                "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
lda_plot + scale_fill_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", 
                                         "DIN" = "DIN", "LP" = "LP", "HP" = "HP", 
                                         "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP"))

