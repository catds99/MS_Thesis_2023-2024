#2023-12-21
#barplots with phytoclass data

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)


#import data

pc_percent_of_control = read_excel("data/PhytoClass_data.xlsx", sheet = "percent_of_control")

summary(pc_percent_of_control)
glimpse(pc_percent_of_control)

pc_percent_of_control_avg = pc_percent_of_control %>%
  group_by(Treatment) %>%
  summarise(Total_Chl_a_mean = mean(Total_Chl_a),
            Cyanobacteria_mean = mean(Cyanobacteria),
            Green_Algae_mean = mean(Green_Algae),
            Cryptophytes_mean = mean(Cryptophytes),
            Diatoms_mean = mean(Diatoms),
            Dinoflagellates_mean = mean(Dinoflagellates),
            Haptophytes_mean = mean(Haptophytes))

pc_percent_change = read_excel("data/PhytoClass_data.xlsx", sheet = "percent_change")

summary(pc_percent_change)
glimpse(pc_percent_change)

pc_percent_change_avg = pc_percent_change %>%
  group_by(Treatment) %>%
  summarise(Total_Chl_a_mean = mean(Total_Chl_a),
            Cyanobacteria_mean = mean(Cyanobacteria),
            Green_Algae_mean = mean(Green_Algae),
            Cryptophytes_mean = mean(Cryptophytes),
            Diatoms_mean = mean(Diatoms),
            Dinoflagellates_mean = mean(Dinoflagellates),
            Haptophytes_mean = mean(Haptophytes))

pc_mean_abundances = read_excel("data/PhytoClass_data.xlsx", sheet = "mean_abundances")

summary(pc_mean_abundances)
glimpse(pc_mean_abundances)

#########################Total Chl a

#Total chl a percent of control

Total_chl_a_percent_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Total_Chl_a_mean),
           fill = "darkgrey",
           data = pc_percent_of_control_avg) +
  xlab("Treatment") +
  ylab("Percent of Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1000, label="Total Chl a", color="black") +
  theme_classic(base_size = 12) 
ggsave(Total_chl_a_percent_control, filename = "figures/PhytoClass/Total_chl_a_percent_control.png",
       device = "png", height = 7, width = 11)

#Total chl a percent difference from control

Total_chl_a_percent_change = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Total_Chl_a_mean),
           fill = "darkgrey",
           data = pc_percent_change_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1000, label="Total Chl a", color="black") +
  theme_classic(base_size = 12) 
ggsave(Total_chl_a_percent_change, filename = "figures/PhytoClass/Total_chl_a_percent_change.png",
       device = "png", height = 7, width = 11)

#Total chl a concentration by bioassay

variable_names = list("1" = "May", "2" = "June", "3" = "July", "4" = "September")

variable_labeller = function(variable,value){
  return(variable_names[value])
}

Total_Chl_a_individual_bioassays = ggplot(data = pc_mean_abundances, 
                                     aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=mean_Total_Chl_a)) +
  geom_bar(stat='identity', fill="darkgrey") +
  xlab("Treatment") +
  ylab("Total Chl a") +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN+LP", "DIN_HP" = "DIN+HP")) +
  facet_wrap(~factor(Bioassay, levels=c('1', '2', '3', '4')),  ncol=2, labeller=variable_labeller) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0)) + 
  ylim(0, 55)
ggsave(Total_Chl_a_individual_bioassays, filename = "figures/PhytoClass/Total_Chl_a_individual_bioassays.png",
       device = "png", height = 7, width = 11)

#########################Green Algae

#Green Algae percent of control

Green_Algae_percent_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Green_Algae_mean),
           fill = "darkgrey",
           data = pc_percent_of_control_avg) +
  xlab("Treatment") +
  ylab("Percent of Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1000, label="Green Algae", color="black") +
  theme_classic(base_size = 12) 
ggsave(Green_Algae_percent_control, filename = "figures/PhytoClass/Green_Algae_percent_control.png",
       device = "png", height = 7, width = 11)

#Green Algae percent difference from control

Green_Algae_percent_change = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Green_Algae_mean),
           fill = "darkgrey",
           data = pc_percent_change_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1000, label="Green Algae", color="black") +
  theme_classic(base_size = 12) 
ggsave(Green_Algae_percent_change, filename = "figures/PhytoClass/Green_Algae_percent_change.png",
       device = "png", height = 7, width = 11)

#Green Algae concentration by bioassay

variable_names = list("1" = "May", "2" = "June", "3" = "July", "4" = "September")

variable_labeller = function(variable,value){
  return(variable_names[value])
}

Green_Algae_individual_bioassays = ggplot(data = pc_mean_abundances, 
                                          aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=mean_Green_Algae)) +
  geom_bar(stat='identity', fill="darkgrey") +
  xlab("Treatment") +
  ylab("Green Algae") +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN+LP", "DIN_HP" = "DIN+HP")) +
  facet_wrap(~factor(Bioassay, levels=c('1', '2', '3', '4')),  ncol=2, labeller=variable_labeller) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0)) + 
  ylim(0, 8)
ggsave(Green_Algae_individual_bioassays, filename = "figures/PhytoClass/Green_Algae_individual_bioassays.png",
       device = "png", height = 7, width = 11)


#########################Cryptophytes

#Cryptophytes percent of control

Cryptophytes_percent_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Cryptophytes_mean),
           fill = "darkgrey",
           data = pc_percent_of_control_avg) +
  xlab("Treatment") +
  ylab("Percent of Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1000, label="Cryptophytes", color="black") +
  theme_classic(base_size = 12) 
ggsave(Cryptophytes_percent_control, filename = "figures/PhytoClass/Cryptophytes_percent_control.png",
       device = "png", height = 7, width = 11)

#Cryptophytes percent difference from control

Cryptophytes_percent_change = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Cryptophytes_mean),
           fill = "darkgrey",
           data = pc_percent_change_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1000, label="Cryptophytes", color="black") +
  theme_classic(base_size = 12) 
ggsave(Cryptophytes_percent_change, filename = "figures/PhytoClass/Cryptophytes_percent_change.png",
       device = "png", height = 7, width = 11)

#Cryptophytes concentration by bioassay

variable_names = list("1" = "May", "2" = "June", "3" = "July", "4" = "September")

variable_labeller = function(variable,value){
  return(variable_names[value])
}

Cryptophytes_individual_bioassays = ggplot(data = pc_mean_abundances, 
                                          aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=mean_Cryptophytes)) +
  geom_bar(stat='identity', fill="darkgrey") +
  xlab("Treatment") +
  ylab("Cryptophytes") +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN+LP", "DIN_HP" = "DIN+HP")) +
  facet_wrap(~factor(Bioassay, levels=c('1', '2', '3', '4')),  ncol=2, labeller=variable_labeller) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0)) + 
  ylim(0, 2.5)
ggsave(Cryptophytes_individual_bioassays, filename = "figures/PhytoClass/Cryptophytes_individual_bioassays.png",
       device = "png", height = 7, width = 11)

#########################Diatoms

#Diatoms percent of control

Diatoms_percent_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Diatoms_mean),
           fill = "darkgrey",
           data = pc_percent_of_control_avg) +
  xlab("Treatment") +
  ylab("Percent of Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=2000, label="Diatoms", color="black") +
  theme_classic(base_size = 12) 
ggsave(Diatoms_percent_control, filename = "figures/PhytoClass/Diatoms_percent_control.png",
       device = "png", height = 7, width = 11)

#Diatoms percent difference from control

Diatoms_percent_change = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Diatoms_mean),
           fill = "darkgrey",
           data = pc_percent_change_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1000, label="Diatoms", color="black") +
  theme_classic(base_size = 12) 
ggsave(Diatoms_percent_change, filename = "figures/PhytoClass/Diatoms_percent_change.png",
       device = "png", height = 7, width = 11)

#Diatoms concentration by bioassay

variable_names = list("1" = "May", "2" = "June", "3" = "July", "4" = "September")

variable_labeller = function(variable,value){
  return(variable_names[value])
}

Diatoms_individual_bioassays = ggplot(data = pc_mean_abundances, 
                                           aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=mean_Diatoms)) +
  geom_bar(stat='identity', fill="darkgrey") +
  xlab("Treatment") +
  ylab("Diatoms") +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN+LP", "DIN_HP" = "DIN+HP")) +
  facet_wrap(~factor(Bioassay, levels=c('1', '2', '3', '4')),  ncol=2, labeller=variable_labeller) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0)) + 
  ylim(0, 50)
ggsave(Diatoms_individual_bioassays, filename = "figures/PhytoClass/Diatoms_individual_bioassays.png",
       device = "png", height = 7, width = 11)

#########################Dinoflagellates

#Dinoflagellates percent of control

Dinoflagellates_percent_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Dinoflagellates_mean),
           fill = "darkgrey",
           data = pc_percent_of_control_avg) +
  xlab("Treatment") +
  ylab("Percent of Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1000, label="Dinoflagellates", color="black") +
  theme_classic(base_size = 12) 
ggsave(Dinoflagellates_percent_control, filename = "figures/PhytoClass/Dinoflagellates_percent_control.png",
       device = "png", height = 7, width = 11)

#Dinoflagellates percent difference from control

Dinoflagellates_percent_change = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Dinoflagellates_mean),
           fill = "darkgrey",
           data = pc_percent_change_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1000, label="Dinoflagellates", color="black") +
  theme_classic(base_size = 12) 
ggsave(Dinoflagellates_percent_change, filename = "figures/PhytoClass/Dinoflagellates_percent_change.png",
       device = "png", height = 7, width = 11)

#Dinoflagellates concentration by bioassay

variable_names = list("1" = "May", "2" = "June", "3" = "July", "4" = "September")

variable_labeller = function(variable,value){
  return(variable_names[value])
}

Dinoflagellates_individual_bioassays = ggplot(data = pc_mean_abundances, 
                                      aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=mean_Dinoflagellates)) +
  geom_bar(stat='identity', fill="darkgrey") +
  xlab("Treatment") +
  ylab("Dinoflagellates") +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN+LP", "DIN_HP" = "DIN+HP")) +
  facet_wrap(~factor(Bioassay, levels=c('1', '2', '3', '4')),  ncol=2, labeller=variable_labeller) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0)) + 
  ylim(0, 0.3)
ggsave(Dinoflagellates_individual_bioassays, filename = "figures/PhytoClass/Dinoflagellates_individual_bioassays.png",
       device = "png", height = 7, width = 11)

#########################Haptophytes

#Haptophytes percent of control

Haptophytes_percent_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Haptophytes_mean),
           fill = "darkgrey",
           data = pc_percent_of_control_avg) +
  xlab("Treatment") +
  ylab("Percent of Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1000, label="Haptophytes", color="black") +
  theme_classic(base_size = 12) 
ggsave(Haptophytes_percent_control, filename = "figures/PhytoClass/Haptophytes_percent_control.png",
       device = "png", height = 7, width = 11)

#Haptophytes percent difference from control

Haptophytes_percent_change = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Haptophytes_mean),
           fill = "darkgrey",
           data = pc_percent_change_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1000, label="Haptophytes", color="black") +
  theme_classic(base_size = 12) 
ggsave(Haptophytes_percent_change, filename = "figures/PhytoClass/Haptophytes_percent_change.png",
       device = "png", height = 7, width = 11)

#Haptophytes concentration by bioassay

variable_names = list("1" = "May", "2" = "June", "3" = "July", "4" = "September")

variable_labeller = function(variable,value){
  return(variable_names[value])
}

Haptophytes_individual_bioassays = ggplot(data = pc_mean_abundances, 
                                              aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=mean_Haptophytes)) +
  geom_bar(stat='identity', fill="darkgrey") +
  xlab("Treatment") +
  ylab("Haptophytes") +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN+LP", "DIN_HP" = "DIN+HP")) +
  facet_wrap(~factor(Bioassay, levels=c('1', '2', '3', '4')),  ncol=2, labeller=variable_labeller) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0)) + 
  ylim(0, 6)
ggsave(Haptophytes_individual_bioassays, filename = "figures/PhytoClass/Haptophytes_individual_bioassays.png",
       device = "png", height = 7, width = 11)
