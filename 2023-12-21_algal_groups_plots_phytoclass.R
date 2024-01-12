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
            Haptophytes_mean = mean(Haptophytes), 
            Total_Chl_a_sd = sd(Total_Chl_a, na.rm = TRUE),
            Cyanobacteria_sd = sd(Cyanobacteria, na.rm = TRUE),
            Green_Algae_sd = sd(Green_Algae, na.rm = TRUE),
            Cryptophytes_sd = sd(Cryptophytes, na.rm = TRUE),
            Diatoms_sd = sd(Diatoms, na.rm = TRUE),
            Dinoflagellates_sd = sd(Dinoflagellates, na.rm = TRUE),
            Haptophytes_sd = sd(Haptophytes, na.rm = TRUE)
            )

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
  theme_classic(base_size = 14) +
  ylim(0, 1000)
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
  theme_classic(base_size = 14) +
  ylim(0,800)
ggsave(Total_chl_a_percent_change, filename = "figures/PhytoClass/Total_chl_a_percent_change.png",
       device = "png", height = 7, width = 11)

#Total chl a percent difference from control with stdev and percents added

pc_percent_change_avg = pc_percent_change %>%
  group_by(Treatment) %>%
  summarise(Total_Chl_a_mean = mean(Total_Chl_a),
            Cyanobacteria_mean = mean(Cyanobacteria),
            Green_Algae_mean = mean(Green_Algae),
            Cryptophytes_mean = mean(Cryptophytes),
            Diatoms_mean = mean(Diatoms),
            Dinoflagellates_mean = mean(Dinoflagellates),
            Haptophytes_mean = mean(Haptophytes))

total_chl_a_summary = pc_percent_change_avg %>%
  summarise(
    total_chl_a_sd = sd(Total_Chl_a_mean, na.rm = TRUE),
  )

pc_percent_change_summary = pc_percent_change %>%
  group_by(Treatment) %>%
  summarise(
    tca = mean(Total_Chl_a),
    cyano = mean(Cyanobacteria),
    ga = mean(Green_Algae),
    crypto = mean(Cryptophytes),
    dia = mean(Diatoms),
    dino = mean(Dinoflagellates),
    hapto = mean(Haptophytes),
    tca_sd = sd(Total_Chl_a, na.rm = TRUE),
    cyano_sd = sd(Cyanobacteria, na.rm = TRUE),
    ga_sd = sd(Green_Algae, na.rm = TRUE),
    crypto_sd = sd(Cryptophytes, na.rm = TRUE),
    dia_sd = sd(Diatoms, na.rm = TRUE),
    dino_sd = sd(Dinoflagellates, na.rm = TRUE),
    hapto_sd = sd(Haptophytes, na.rm = TRUE)
  )
pc_percent_change_summary

Total_chl_a_percent_change_2 = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = tca),
           fill = "darkgrey",
           data = pc_percent_change_summary) +
  geom_errorbar(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    y = tca,
                    ymin = tca-tca_sd, 
                    ymax = tca+tca_sd,
                    width = 0.25),
                    data = pc_percent_change_summary) +
  geom_text(aes(label = c("565.57%", "46.03%", "24.37%", "693.61%", "779.14%"), 
                x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                y = -75), 
            vjust = -0.5,
            data = pc_percent_change_summary) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  ylim(-75,1400)
ggsave(Total_chl_a_percent_change_2, filename = "figures/PhytoClass/Total_chl_a_percent_change_2.png",
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

Green_Algae_percent_change_2 = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Green_Algae_mean),
           fill = "darkgrey",
           data = pc_percent_change_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) 
ggsave(Green_Algae_percent_change_2, filename = "figures/PhytoClass/Green_Algae_percent_change_2.png",
       device = "png", height = 7, width = 11)

Green_Algae_percent_change_3 = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = ga),
           fill = "darkgrey",
           data = pc_percent_change_summary) +
  geom_errorbar(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    y = ga,
                    ymin = ga-ga_sd, 
                    ymax = ga+ga_sd,
                    width = 0.25),
                data = pc_percent_change_summary) +
  geom_text(aes(label = c("343%", "482%", "440%", "6.71%", "20.3%"), 
                x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                y = -75), 
            vjust = -0.5,
            data = pc_percent_change_summary) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  ylim(-75, 1000)
ggsave(Green_Algae_percent_change_3, filename = "figures/PhytoClass/Green_Algae_percent_change_3.png",
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

Cryptophytes_percent_change_2 = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Cryptophytes_mean),
           fill = "darkgrey",
           data = pc_percent_change_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  ylim(0, 700)
ggsave(Cryptophytes_percent_change_2, filename = "figures/PhytoClass/Cryptophytes_percent_change_2.png",
       device = "png", height = 7, width = 11)

Cryptophytes_percent_change_3 = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = crypto),
           fill = "darkgrey",
           data = pc_percent_change_summary) +
  geom_errorbar(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    y = crypto,
                    ymin = crypto-crypto_sd, 
                    ymax = crypto+crypto_sd,
                    width = 0.25),
                data = pc_percent_change_summary) +
  geom_text(aes(label = c("518%", "622%", "587%", "19.5%", "38.5%"), 
                x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                y = -75), 
            vjust = -0.5,
            data = pc_percent_change_summary) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  ylim(-75, 1200)
ggsave(Cryptophytes_percent_change_3, filename = "figures/PhytoClass/Cryptophytes_percent_change_3.png",
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

Diatoms_percent_change_2 = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Diatoms_mean),
           fill = "darkgrey",
           data = pc_percent_change_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  ylim(0,2000)
ggsave(Diatoms_percent_change_2, filename = "figures/PhytoClass/Diatoms_percent_change_2.png",
       device = "png", height = 7, width = 11)

Diatoms_percent_change_3 = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = dia),
           fill = "darkgrey",
           data = pc_percent_change_summary) +
  geom_errorbar(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    y = dia,
                    ymin = dia-dia_sd, 
                    ymax = dia+dia_sd,
                    width = 0.25),
                data = pc_percent_change_summary) +
  geom_text(aes(label = c("1347%", "1846%", "1533%", "5.65%", "34.7%"), 
                x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                y = -250), 
            vjust = -0.5,
            data = pc_percent_change_summary) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  ylim(-250, 3800)
ggsave(Diatoms_percent_change_3, filename = "figures/PhytoClass/Diatoms_percent_change_3.png",
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

Dinoflagellates_percent_change_2 = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Dinoflagellates_mean),
           fill = "darkgrey",
           data = pc_percent_change_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  ylim(-50, 800)
ggsave(Dinoflagellates_percent_change_2, filename = "figures/PhytoClass/Dinoflagellates_percent_change_2.png",
       device = "png", height = 7, width = 11)

Dinoflagellates_percent_change_3 = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = dino),
           fill = "darkgrey",
           data = pc_percent_change_summary) +
  geom_errorbar(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    y = dino,
                    ymin = dino-dino_sd, 
                    ymax = dino+dino_sd,
                    width = 0.25),
                data = pc_percent_change_summary) +
  geom_text(aes(label = c("539%", "708%", "600%", "-10.6%", "29%"), 
                x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                y = -850), 
            vjust = -0.5,
            data = pc_percent_change_summary) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  ylim(-850, 2000)
ggsave(Dinoflagellates_percent_change_3, filename = "figures/PhytoClass/Dinoflagellates_percent_change_3.png",
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

Haptophytes_percent_change_2 = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Haptophytes_mean),
           fill = "darkgrey",
           data = pc_percent_change_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  ylim(-50, 150)
ggsave(Haptophytes_percent_change_2, filename = "figures/PhytoClass/Haptophytes_percent_change_2.png",
       device = "png", height = 7, width = 11)

Haptophytes_percent_change_3 = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = hapto),
           fill = "darkgrey",
           data = pc_percent_change_summary) +
  geom_errorbar(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    y = hapto,
                    ymin = hapto-hapto_sd, 
                    ymax = hapto+hapto_sd,
                    width = 0.25),
                data = pc_percent_change_summary) +
  geom_text(aes(label = c("122%", "-26.4%", "-30.6%", "53.5%", "52.4%"), 
                x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                y = -250), 
            vjust = -0.5,
            data = pc_percent_change_summary) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  ylim(-250, 500)
ggsave(Haptophytes_percent_change_3, filename = "figures/PhytoClass/Haptophytes_percent_change_3.png",
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

#########################Cyanobacteria

#Haptophytes percent difference from control

Cyanobacteria_percent_change_2 = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Cyanobacteria_mean),
           fill = "darkgrey",
           data = pc_percent_change_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14)
ggsave(Cyanobacteria_percent_change_2, filename = "figures/PhytoClass/Cyanobacteria_percent_change_2.png",
       device = "png", height = 7, width = 11)

Cyanobacteria_percent_change_3 = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = cyano),
           fill = "darkgrey",
           data = pc_percent_change_summary) +
  geom_errorbar(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    y = cyano,
                    ymin = cyano-cyano_sd, 
                    ymax = cyano+cyano_sd,
                    width = 0.25),
                data = pc_percent_change_summary) +
  geom_text(aes(label = c("39.7%", "15.2%", "16.4%", "30.8%", "20.6%"), 
                x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                y = -100), 
            vjust = -0.5,
            data = pc_percent_change_summary) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  ylim(-100, 150)
ggsave(Cyanobacteria_percent_change_3, filename = "figures/PhytoClass/Cyanobacteria_percent_change_3.png",
       device = "png", height = 7, width = 11)
