#2023-12-08
#barplots for individual algal groups

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)

#########################Chlorophytes

#import data

Chlorophytes = read_excel("data/Chemtax_by_group.xlsx", sheet = "Chlorophytes_percents")

summary(Chlorophytes)
glimpse(Chlorophytes)

Chlorophytes_avg = Chlorophytes %>%
  group_by(Treatment) %>%
  summarise(Percent_of_control_avg = mean(Percent_of_control),
            Percent_dif_from_control_avg = mean(Percent_dif_from_control))

#chlorophyte percent of control

Chloro_percent_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Percent_of_control_avg),
           fill = "darkgrey",
           data = Chlorophytes_avg) +
  xlab("Treatment") +
  ylab("Percent of Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1050, label="Chlorophytes", color="black") +
  theme_classic(base_size = 12) 
ggsave(Chloro_percent_control, filename = "figures/Chloro_percent_control.png",
       device = "png", height = 7, width = 11)

#chloro percent difference from control

Chloro_percent_change_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Percent_dif_from_control_avg),
           fill = "darkgrey",
           data = Chlorophytes_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1050, label="Chlorophytes", color="black") +
  theme_classic(base_size = 12) 
ggsave(Chloro_percent_change_control, filename = "figures/Chloro_percent_change_control.png",
       device = "png", height = 7, width = 11)

#Chlorophyte mean concentration all bioassays

Chlorophytes_conc = read_excel("data/Chemtax_by_group.xlsx", sheet = "Chlorophytes_general")

summary(Chlorophytes_conc)
glimpse(Chlorophytes_conc)

Chlorophytes_conc_avg = Chlorophytes_conc %>%
  group_by(Group) %>%
  summarise(Concentration_avg = mean(Concentration))

Chloro_biomass_all = ggplot() +
  geom_col(aes(x = fct_relevel(Group, "T_0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Concentration_avg),
           fill = "darkgrey",
           data = Chlorophytes_conc_avg) +
  xlab("Treatment (all bioassays together)") +
  ylab(expression(paste("Chlorophytes Concentration \u03BCg", l^-1))) +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=2.5, label="Chlorophytes", color="black") +
  theme_classic(base_size = 12) 
ggsave(Chloro_biomass_all, filename = "figures/Chloro_biomass_all.png",
       device = "png", height = 7, width = 11)

#Chlorohpyte concentration by bioassay

all_bioassay = read_excel("data/Chemtax_by_group.xlsx", sheet = "Chlorophytes_concentration")

summary(all_bioassay)
glimpse(all_bioassay)

variable_names = list("1" = "May", "2" = "June", "3" = "July", "4" = "September")

variable_labeller = function(variable,value){
  return(variable_names[value])
}

Chloro_individual_bioassays = ggplot(data = all_bioassay, 
                            aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=Concentration_avg)) +
  geom_bar(stat='identity', fill="darkgrey") +
  xlab("Treatment") +
  ylab("Chlorophytes") +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN+LP", "DIN_HP" = "DIN+HP")) +
  facet_wrap(~factor(Bioassay, levels=c('1', '2', '3', '4')),  ncol=2, labeller=variable_labeller) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0)) + 
  ylim(0, 6)
ggsave(Chloro_individual_bioassays, filename = "figures/Chloro_individual_bioassays.png",
       device = "png", height = 7, width = 11)

################################Cryptophytes

#import data

Cryptophytes = read_excel("data/Chemtax_by_group.xlsx", sheet = "Cryptophytes_percents")

summary(Cryptophytes)
glimpse(Cryptophytes)

Cryptophytes_avg = Cryptophytes %>%
  group_by(Treatment) %>%
  summarise(Percent_of_control_avg = mean(Percent_of_control),
            Percent_dif_from_control_avg = mean(Percent_dif_from_control))

#Cryptophytes percent of control

Crypto_percent_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Percent_of_control_avg),
           fill = "darkgrey",
           data = Cryptophytes_avg) +
  xlab("Treatment") +
  ylab("Percent of Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=400, label="Cryptophytes", color="black") +
  theme_classic(base_size = 12) 
ggsave(Crypto_percent_control, filename = "figures/Crypto_percent_control.png",
       device = "png", height = 7, width = 11)

#crypto percent difference from control

Crypto_percent_change_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Percent_dif_from_control_avg),
           fill = "darkgrey",
           data = Cryptophytes_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=300, label="Cryptophytes", color="black") +
  theme_classic(base_size = 12) 
ggsave(Crypto_percent_change_control, filename = "figures/Crypto_percent_change_control.png",
       device = "png", height = 7, width = 11)

#Cryptophytes mean concentration all bioassays

Cryptophytes_conc = read_excel("data/Chemtax_by_group.xlsx", sheet = "Cryptophytes_general")

summary(Cryptophytes_conc)
glimpse(Cryptophytes_conc)

Cryptophytes_conc_avg = Cryptophytes_conc %>%
  group_by(Group) %>%
  summarise(Concentration_avg = mean(Concentration))

Crypto_biomass_all = ggplot() +
  geom_col(aes(x = fct_relevel(Group, "T_0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Concentration_avg),
           fill = "darkgrey",
           data = Cryptophytes_conc_avg) +
  xlab("Treatment (all bioassays together)") +
  ylab("Cryptophytes") +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1, label="Cryptophytes", color="black") +
  theme_classic(base_size = 12) 
ggsave(Crypto_biomass_all, filename = "figures/Crypto_biomass_all.png",
       device = "png", height = 7, width = 11)

#Cryptophytes concentration by bioassay

all_bioassay = read_excel("data/Chemtax_by_group.xlsx", sheet = "Cryptophytes_concentration")

summary(all_bioassay)
glimpse(all_bioassay)

variable_names = list("1" = "May", "2" = "June", "3" = "July", "4" = "September")

variable_labeller = function(variable,value){
  return(variable_names[value])
}

Crypto_individual_bioassays = ggplot(data = all_bioassay, 
                                     aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=Concentration_avg)) +
  geom_bar(stat='identity', fill="darkgrey") +
  xlab("Treatment") +
  ylab("Cryptophytes") +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN+LP", "DIN_HP" = "DIN+HP")) +
  facet_wrap(~factor(Bioassay, levels=c('1', '2', '3', '4')),  ncol=2, labeller=variable_labeller) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0)) + 
  ylim(0, 1.25)
ggsave(Crypto_individual_bioassays, filename = "figures/Crypto_individual_bioassays.png",
       device = "png", height = 7, width = 11)

################################Cyanobacteria

#import data

Cyanobacteria = read_excel("data/Chemtax_by_group.xlsx", sheet = "Cyanobacteria_percents")

summary(Cyanobacteria)
glimpse(Cyanobacteria)

Cyanobacteria_avg = Cyanobacteria %>%
  group_by(Treatment) %>%
  summarise(Percent_of_control_avg = mean(Percent_of_control),
            Percent_dif_from_control_avg = mean(Percent_dif_from_control))

#Cyanobacteria percent of control

Cyano_percent_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Percent_of_control_avg),
           fill = "darkgrey",
           data = Cyanobacteria_avg) +
  xlab("Treatment") +
  ylab("Percent of Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=140, label="Cyanobacteria", color="black") +
  theme_classic(base_size = 12) 
ggsave(Cyano_percent_control, filename = "figures/Cyano_percent_control.png",
       device = "png", height = 7, width = 11)

#cyano percent difference from control

Cyano_percent_change_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Percent_dif_from_control_avg),
           fill = "darkgrey",
           data = Cyanobacteria_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=30, label="Cyanobacteria", color="black") +
  theme_classic(base_size = 12) 
ggsave(Cyano_percent_change_control, filename = "figures/Cyano_percent_change_control.png",
       device = "png", height = 7, width = 11)

#Cyanobacteria mean concentration all bioassays

Cyanobacteria_conc = read_excel("data/Chemtax_by_group.xlsx", sheet = "Cyanobacteria_general")

summary(Cyanobacteria_conc)
glimpse(Cyanobacteria_conc)

Cyanobacteria_conc_avg = Cyanobacteria_conc %>%
  group_by(Group) %>%
  summarise(Concentration_avg = mean(Concentration))

Cyano_biomass_all = ggplot() +
  geom_col(aes(x = fct_relevel(Group, "T_0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Concentration_avg),
           fill = "darkgrey",
           data = Cyanobacteria_conc_avg) +
  xlab("Treatment (all bioassays together)") +
  ylab("Cyanobacteria") +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=0.5, label="Cyanobacteria", color="black") +
  theme_classic(base_size = 12) 
ggsave(Cyano_biomass_all, filename = "figures/Cyano_biomass_all.png",
       device = "png", height = 7, width = 11)

#Cyanobacteria concentration by bioassay

all_bioassay = read_excel("data/Chemtax_by_group.xlsx", sheet = "Cyanobacteria_concentration")

summary(all_bioassay)
glimpse(all_bioassay)

variable_names = list("1" = "May", "2" = "June", "3" = "July", "4" = "September")

variable_labeller = function(variable,value){
  return(variable_names[value])
}

Cyano_individual_bioassays = ggplot(data = all_bioassay, 
                                     aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=Concentration_avg)) +
  geom_bar(stat='identity', fill="darkgrey") +
  xlab("Treatment") +
  ylab("Cyanobacteria") +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN+LP", "DIN_HP" = "DIN+HP")) +
  facet_wrap(~factor(Bioassay, levels=c('1', '2', '3', '4')),  ncol=2, labeller=variable_labeller) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0)) + 
  ylim(0, 1.2)
ggsave(Cyano_individual_bioassays, filename = "figures/Cyano_individual_bioassays.png",
       device = "png", height = 7, width = 11)

################################Diatoms

#import data

Diatoms = read_excel("data/Chemtax_by_group.xlsx", sheet = "Diatoms_percents")

summary(Diatoms)
glimpse(Diatoms)

Diatoms_avg = Diatoms %>%
  group_by(Treatment) %>%
  summarise(Percent_of_control_avg = mean(Percent_of_control),
            Percent_dif_from_control_avg = mean(Percent_dif_from_control))

#Diatoms percent of control

Diatoms_percent_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Percent_of_control_avg),
           fill = "darkgrey",
           data = Diatoms_avg) +
  xlab("Treatment") +
  ylab("Percent of Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1500, label="Diatoms", color="black") +
  theme_classic(base_size = 12) 
ggsave(Diatoms_percent_control, filename = "figures/Diatoms_percent_control.png",
       device = "png", height = 7, width = 11)

#Diatoms percent difference from control

Diatoms_percent_change_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Percent_dif_from_control_avg),
           fill = "darkgrey",
           data = Diatoms_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1250, label="Diatoms", color="black") +
  theme_classic(base_size = 12) 
ggsave(Diatoms_percent_change_control, filename = "figures/Diatoms_percent_change_control.png",
       device = "png", height = 7, width = 11)

#Diatoms mean concentration all bioassays

Diatoms_conc = read_excel("data/Chemtax_by_group.xlsx", sheet = "Diatoms_general")

summary(Diatoms_conc)
glimpse(Diatoms_conc)

Diatoms_conc_avg = Diatoms_conc %>%
  group_by(Group) %>%
  summarise(Concentration_avg = mean(Concentration))

Diatoms_biomass_all = ggplot() +
  geom_col(aes(x = fct_relevel(Group, "T_0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Concentration_avg),
           fill = "darkgrey",
           data = Diatoms_conc_avg) +
  xlab("Treatment (all bioassays together)") +
  ylab("Diatoms") +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=30, label="Diatoms", color="black") +
  theme_classic(base_size = 12) 
ggsave(Diatoms_biomass_all, filename = "figures/Diatoms_biomass_all.png",
       device = "png", height = 7, width = 11)

#Diatoms concentration by bioassay

all_bioassay = read_excel("data/Chemtax_by_group.xlsx", sheet = "Diatoms_concentration")

summary(all_bioassay)
glimpse(all_bioassay)

variable_names = list("1" = "May", "2" = "June", "3" = "July", "4" = "September")

variable_labeller = function(variable,value){
  return(variable_names[value])
}

Diatoms_individual_bioassays = ggplot(data = all_bioassay, 
                                    aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=Concentration_avg)) +
  geom_bar(stat='identity', fill="darkgrey") +
  xlab("Treatment") +
  ylab("Diatoms") +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN+LP", "DIN_HP" = "DIN+HP")) +
  facet_wrap(~factor(Bioassay, levels=c('1', '2', '3', '4')),  ncol=2, labeller=variable_labeller) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0)) + 
  ylim(0, 45)
ggsave(Diatoms_individual_bioassays, filename = "figures/Diatoms_individual_bioassays.png",
       device = "png", height = 7, width = 11)

################################Dinoflagellates

#import data

Dinoflagellates = read_excel("data/Chemtax_by_group.xlsx", sheet = "Dinoflagellates_percents")

summary(Dinoflagellates)
glimpse(Dinoflagellates)

Dinoflagellates_avg = Dinoflagellates %>%
  group_by(Treatment) %>%
  summarise(Percent_of_control_avg = mean(Percent_of_control),
            Percent_dif_from_control_avg = mean(Percent_dif_from_control))

#Dinoflagellates percent of control

Dinoflagellates_percent_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Percent_of_control_avg),
           fill = "darkgrey",
           data = Dinoflagellates_avg) +
  xlab("Treatment") +
  ylab("Percent of Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1700, label="Dinoflagellates", color="black") +
  theme_classic(base_size = 12) 
ggsave(Dinoflagellates_percent_control, filename = "figures/Dinoflagellates_percent_control.png",
       device = "png", height = 7, width = 11)

#Dinoflagellates percent difference from control

Dinoflagellates_percent_change_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Percent_dif_from_control_avg),
           fill = "darkgrey",
           data = Dinoflagellates_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1600, label="Dinoflagellates", color="black") +
  theme_classic(base_size = 12) 
ggsave(Dinoflagellates_percent_change_control, filename = "figures/Dinoflagellates_percent_change_control.png",
       device = "png", height = 7, width = 11)

#Dinoflagellates mean concentration all bioassays

Dinoflagellates_conc = read_excel("data/Chemtax_by_group.xlsx", sheet = "Dinoflagellates_general")

summary(Dinoflagellates_conc)
glimpse(Dinoflagellates_conc)

Dinoflagellates_conc_avg = Dinoflagellates_conc %>%
  group_by(Group) %>%
  summarise(Concentration_avg = mean(Concentration))

Dinoflagellates_biomass_all = ggplot() +
  geom_col(aes(x = fct_relevel(Group, "T_0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Concentration_avg),
           fill = "darkgrey",
           data = Dinoflagellates_conc_avg) +
  xlab("Treatment (all bioassays together)") +
  ylab("Dinoflagellates") +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=0.15, label="Dinoflagellates", color="black") +
  theme_classic(base_size = 12) 
ggsave(Dinoflagellates_biomass_all, filename = "figures/Dinoflagellates_biomass_all.png",
       device = "png", height = 7, width = 11)

#Dinoflagellates concentration by bioassay

all_bioassay = read_excel("data/Chemtax_by_group.xlsx", sheet = "Dinoflagellates_concentration")

summary(all_bioassay)
glimpse(all_bioassay)

variable_names = list("1" = "May", "2" = "June", "3" = "July", "4" = "September")

variable_labeller = function(variable,value){
  return(variable_names[value])
}

Dinoflagellates_individual_bioassays = ggplot(data = all_bioassay, 
                                      aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=Concentration_avg)) +
  geom_bar(stat='identity', fill="darkgrey") +
  xlab("Treatment") +
  ylab("Dinoflagellates") +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN+LP", "DIN_HP" = "DIN+HP")) +
  facet_wrap(~factor(Bioassay, levels=c('1', '2', '3', '4')),  ncol=2, labeller=variable_labeller) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0)) + 
  ylim(0, 0.25)
ggsave(Dinoflagellates_individual_bioassays, filename = "figures/Dinoflagellates_individual_bioassays.png",
       device = "png", height = 7, width = 11)

################################Prasinophytes

#import data

Prasinophytes = read_excel("data/Chemtax_by_group.xlsx", sheet = "Prasinophytes_percents")

summary(Prasinophytes)
glimpse(Prasinophytes)

Prasinophytes_avg = Prasinophytes %>%
  group_by(Treatment) %>%
  summarise(Percent_of_control_avg = mean(Percent_of_control),
            Percent_dif_from_control_avg = mean(Percent_dif_from_control))

#Prasinophytes percent of control

Prasinophytes_percent_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Percent_of_control_avg),
           fill = "darkgrey",
           data = Prasinophytes_avg) +
  xlab("Treatment") +
  ylab("Percent of Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1000, label="Prasinophytes", color="black") +
  theme_classic(base_size = 12) 
ggsave(Prasinophytes_percent_control, filename = "figures/Prasinophytes_percent_control.png",
       device = "png", height = 7, width = 11)

#Prasinophytes percent difference from control

Prasinophytes_percent_change_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Percent_dif_from_control_avg),
           fill = "darkgrey",
           data = Prasinophytes_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=900, label="Prasinophytes", color="black") +
  theme_classic(base_size = 12) 
ggsave(Prasinophytes_percent_change_control, filename = "figures/Prasinophytes_percent_change_control.png",
       device = "png", height = 7, width = 11)

#Prasinophytes mean concentration all bioassays

Prasinophytes_conc = read_excel("data/Chemtax_by_group.xlsx", sheet = "Prasinophytes_general")

summary(Prasinophytes_conc)
glimpse(Prasinophytes_conc)

Prasinophytes_conc_avg = Prasinophytes_conc %>%
  group_by(Group) %>%
  summarise(Concentration_avg = mean(Concentration))

Prasinophytes_biomass_all = ggplot() +
  geom_col(aes(x = fct_relevel(Group, "T_0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Concentration_avg),
           fill = "darkgrey",
           data = Prasinophytes_conc_avg) +
  xlab("Treatment (all bioassays together)") +
  ylab("Prasinophytes") +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1.65, label="Prasinophytes", color="black") +
  theme_classic(base_size = 12) 
ggsave(Prasinophytes_biomass_all, filename = "figures/Prasinophytes_biomass_all.png",
       device = "png", height = 7, width = 11)

#Prasinophytes concentration by bioassay

all_bioassay = read_excel("data/Chemtax_by_group.xlsx", sheet = "Prasinophytes_concentration")

summary(all_bioassay)
glimpse(all_bioassay)

variable_names = list("1" = "May", "2" = "June", "3" = "July", "4" = "September")

variable_labeller = function(variable,value){
  return(variable_names[value])
}

Prasinophytes_individual_bioassays = ggplot(data = all_bioassay, 
                                              aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=Concentration_avg)) +
  geom_bar(stat='identity', fill="darkgrey") +
  xlab("Treatment") +
  ylab("Prasinophytes") +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN+LP", "DIN_HP" = "DIN+HP")) +
  facet_wrap(~factor(Bioassay, levels=c('1', '2', '3', '4')),  ncol=2, labeller=variable_labeller) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0)) + 
  ylim(0, 2.6)
ggsave(Prasinophytes_individual_bioassays, filename = "figures/Prasinophytes_individual_bioassays.png",
       device = "png", height = 7, width = 11)

################################Euglenophytes

#import data

Euglenophytes = read_excel("data/Chemtax_by_group.xlsx", sheet = "Euglenophytes_percents")

summary(Euglenophytes)
glimpse(Euglenophytes)

Euglenophytes_avg = Euglenophytes %>%
  group_by(Treatment) %>%
  summarise(Percent_of_control_avg = mean(Percent_of_control),
            Percent_dif_from_control_avg = mean(Percent_dif_from_control))

#Euglenophytes percent of control

Euglenophytes_percent_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Percent_of_control_avg),
           fill = "darkgrey",
           data = Euglenophytes_avg) +
  xlab("Treatment") +
  ylab("Percent of Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=425, label="Euglenophytes", color="black") +
  theme_classic(base_size = 12) 
ggsave(Euglenophytes_percent_control, filename = "figures/Euglenophytes_percent_control.png",
       device = "png", height = 7, width = 11)

#Euglenophytes percent difference from control

Euglenophytes_percent_change_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Percent_dif_from_control_avg),
           fill = "darkgrey",
           data = Euglenophytes_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=325, label="Euglenophytes", color="black") +
  theme_classic(base_size = 12) 
ggsave(Euglenophytes_percent_change_control, filename = "figures/Euglenophytes_percent_change_control.png",
       device = "png", height = 7, width = 11)

#Euglenophytes mean concentration all bioassays

Euglenophytes_conc = read_excel("data/Chemtax_by_group.xlsx", sheet = "Euglenophytes_general")

summary(Euglenophytes_conc)
glimpse(Euglenophytes_conc)

Euglenophytes_conc_avg = Euglenophytes_conc %>%
  group_by(Group) %>%
  summarise(Concentration_avg = mean(Concentration))

Euglenophytes_biomass_all = ggplot() +
  geom_col(aes(x = fct_relevel(Group, "T_0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Concentration_avg),
           fill = "darkgrey",
           data = Euglenophytes_conc_avg) +
  xlab("Treatment (all bioassays together)") +
  ylab("Euglenophytes") +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=0.7, label="Euglenophytes", color="black") +
  theme_classic(base_size = 12) 
ggsave(Euglenophytes_biomass_all, filename = "figures/Euglenophytes_biomass_all.png",
       device = "png", height = 7, width = 11)

#Euglenophytes concentration by bioassay

all_bioassay = read_excel("data/Chemtax_by_group.xlsx", sheet = "Euglenophytes_concentration")

summary(all_bioassay)
glimpse(all_bioassay)

variable_names = list("1" = "May", "2" = "June", "3" = "July", "4" = "September")

variable_labeller = function(variable,value){
  return(variable_names[value])
}

Euglenophytes_individual_bioassays = ggplot(data = all_bioassay, 
                                            aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=Concentration_avg)) +
  geom_bar(stat='identity', fill="darkgrey") +
  xlab("Treatment") +
  ylab("Euglenophytes") +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN+LP", "DIN_HP" = "DIN+HP")) +
  facet_wrap(~factor(Bioassay, levels=c('1', '2', '3', '4')),  ncol=2, labeller=variable_labeller) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0)) + 
  ylim(0, 1.15)
ggsave(Euglenophytes_individual_bioassays, filename = "figures/Euglenophytes_individual_bioassays.png",
       device = "png", height = 7, width = 11)

################################Haptophytes

#import data

Haptophytes = read_excel("data/Chemtax_by_group.xlsx", sheet = "Haptophytes_percents")

summary(Haptophytes)
glimpse(Haptophytes)

Haptophytes_avg = Haptophytes %>%
  group_by(Treatment) %>%
  summarise(Percent_of_control_avg = mean(Percent_of_control),
            Percent_dif_from_control_avg = mean(Percent_dif_from_control))

#Haptophytes percent of control

Haptophytes_percent_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Percent_of_control_avg),
           fill = "darkgrey",
           data = Haptophytes_avg) +
  xlab("Treatment") +
  ylab("Percent of Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1100, label="Haptophytes", color="black") +
  theme_classic(base_size = 12) 
ggsave(Haptophytes_percent_control, filename = "figures/Haptophytes_percent_control.png",
       device = "png", height = 7, width = 11)

#Haptophytes percent difference from control

Haptophytes_percent_change_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Percent_dif_from_control_avg),
           fill = "darkgrey",
           data = Haptophytes_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1000, label="Haptophytes", color="black") +
  theme_classic(base_size = 12) 
ggsave(Haptophytes_percent_change_control, filename = "figures/Haptophytes_percent_change_control.png",
       device = "png", height = 7, width = 11)

#Haptophytes mean concentration all bioassays

Haptophytes_conc = read_excel("data/Chemtax_by_group.xlsx", sheet = "Haptophytes_general")

summary(Haptophytes_conc)
glimpse(Haptophytes_conc)

Haptophytes_conc_avg = Haptophytes_conc %>%
  group_by(Group) %>%
  summarise(Concentration_avg = mean(Concentration))

Haptophytes_biomass_all = ggplot() +
  geom_col(aes(x = fct_relevel(Group, "T_0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Concentration_avg),
           fill = "darkgrey",
           data = Haptophytes_conc_avg) +
  xlab("Treatment (all bioassays together)") +
  ylab("Haptophytes") +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1.6, label="Haptophytes", color="black") +
  theme_classic(base_size = 12) 
ggsave(Haptophytes_biomass_all, filename = "figures/Haptophytes_biomass_all.png",
       device = "png", height = 7, width = 11)

#Haptophytes concentration by bioassay

all_bioassay = read_excel("data/Chemtax_by_group.xlsx", sheet = "Haptophytes_concentration")

summary(all_bioassay)
glimpse(all_bioassay)

variable_names = list("1" = "May", "2" = "June", "3" = "July", "4" = "September")

variable_labeller = function(variable,value){
  return(variable_names[value])
}

Haptophytes_individual_bioassays = ggplot(data = all_bioassay, 
                                            aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=Concentration_avg)) +
  geom_bar(stat='identity', fill="darkgrey") +
  xlab("Treatment") +
  ylab("Haptophytes") +
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN+LP", "DIN_HP" = "DIN+HP")) +
  facet_wrap(~factor(Bioassay, levels=c('1', '2', '3', '4')),  ncol=2, labeller=variable_labeller) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0)) + 
  ylim(0, 3.4)
ggsave(Haptophytes_individual_bioassays, filename = "figures/Haptophytes_individual_bioassays.png",
       device = "png", height = 7, width = 11)
