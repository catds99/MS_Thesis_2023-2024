#2024-02-16
#barplots for all bioassay with and without transformed data

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)


pc_abundances = read_excel("C:/Users/cathe/Desktop/MS Thesis/MS_Thesis_2023-2024/data/PhytoClass_data.xlsx", sheet = "Absolute_Abundances")

glimpse(pc_abundances)

########################################total chl a

summary_data = pc_abundances %>%
  group_by(Bioassay, Treatment) %>%
  summarise(
    mean = mean(Total_Chl_a),
    sd = sd(Total_Chl_a, na.rm = TRUE),
  )

summary_data

summary_data$Bioassay = factor(summary_data$Bioassay, levels=c("May", "June", "July", "September")) 

all_bioassay_total_chl_a = ggplot(data = summary_data, 
       aes(x = mean, y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = mean,
               y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = summary_data) +  
  geom_errorbar(aes(x = mean,
                    y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    xmin = mean-sd, 
                    xmax = mean+sd,
                    width = 0.25),
                data = summary_data) +
  xlab(expression(paste("Total Chl ", italic("a "), "(", mu, "g", l^-1,")", sep=""))) +
  ylab("Treatment") +
  scale_y_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  facet_wrap(~Bioassay, nrow = 1)

ggsave(all_bioassay_total_chl_a, filename = "figures/Biomass_horizontal/all_bioassay_total_chl_a.png",
       device = "png", height = 12, width = 15)

#######################################abundances of algal groups

abundances_summary = pc_abundances %>%
  group_by(Bioassay, Treatment) %>%
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

glimpse(abundances_summary)

abundances_summary$Bioassay = factor(abundances_summary$Bioassay, levels=c("May", "June", "July", "September")) 

#####cyano

all_bioassay_cyano = ggplot(data = abundances_summary, 
                                  aes(x = cyano, y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = cyano,
               y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = abundances_summary) +  
  geom_errorbar(aes(x = cyano,
                    y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    xmin = cyano-cyano_sd, 
                    xmax = cyano+cyano_sd,
                    width = 0.25),
                data = abundances_summary) +
  xlab(expression(paste("Cyanobacteria (", mu, "g", l^-1,")", sep=""))) +
  ylab("Treatment") +
  scale_y_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  facet_wrap(~Bioassay, nrow = 1)

ggsave(all_bioassay_cyano, filename = "figures/Biomass_horizontal/all_bioassay_cyano.png",
       device = "png", height = 12, width = 15)

#####ga

all_bioassay_ga = ggplot(data = abundances_summary, 
                            aes(x = ga, y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = ga,
               y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = abundances_summary) +  
  geom_errorbar(aes(x = ga,
                    y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    xmin = ga-ga_sd, 
                    xmax = ga+ga_sd,
                    width = 0.25),
                data = abundances_summary) +
  xlab(expression(paste("Green Algae (", mu, "g", l^-1,")", sep=""))) +
  ylab("Treatment") +
  scale_y_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  facet_wrap(~Bioassay, nrow = 1)

ggsave(all_bioassay_ga, filename = "figures/Biomass_horizontal/all_bioassay_ga.png",
       device = "png", height = 12, width = 15)

#####crypto

all_bioassay_crypto = ggplot(data = abundances_summary, 
                            aes(x = crypto, y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = crypto,
               y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = abundances_summary) +  
  geom_errorbar(aes(x = crypto,
                    y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    xmin = crypto-crypto_sd, 
                    xmax = crypto+crypto_sd,
                    width = 0.25),
                data = abundances_summary) +
  xlab(expression(paste("Cryptophytes (", mu, "g", l^-1,")", sep=""))) +
  ylab("Treatment") +
  scale_y_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  facet_wrap(~Bioassay, nrow = 1)

ggsave(all_bioassay_crypto, filename = "figures/Biomass_horizontal/all_bioassay_crypto.png",
       device = "png", height = 12, width = 15)

#####cyano

all_bioassay_dia = ggplot(data = abundances_summary, 
                            aes(x = dia, y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = dia,
               y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = abundances_summary) +  
  geom_errorbar(aes(x = dia,
                    y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    xmin = dia-dia_sd, 
                    xmax = dia+dia_sd,
                    width = 0.25),
                data = abundances_summary) +
  xlab(expression(paste("Diatoms (", mu, "g", l^-1,")", sep=""))) +
  ylab("Treatment") +
  scale_y_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  facet_wrap(~Bioassay, nrow = 1)

ggsave(all_bioassay_dia, filename = "figures/Biomass_horizontal/all_bioassay_dia.png",
       device = "png", height = 12, width = 15)

#####dino

all_bioassay_dino = ggplot(data = abundances_summary, 
                            aes(x = dino, y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = dino,
               y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = abundances_summary) +  
  geom_errorbar(aes(x = dino,
                    y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    xmin = dino-dino_sd, 
                    xmax = dino+dino_sd,
                    width = 0.25),
                data = abundances_summary) +
  xlab(expression(paste("Dinoflagellates (", mu, "g", l^-1,")", sep=""))) +
  ylab("Treatment") +
  scale_y_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  facet_wrap(~Bioassay, nrow = 1)

ggsave(all_bioassay_dino, filename = "figures/Biomass_horizontal/all_bioassay_dino.png",
       device = "png", height = 12, width = 15)

#####hapto

all_bioassay_hapto = ggplot(data = abundances_summary, 
                            aes(x = hapto, y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = hapto,
               y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = abundances_summary) +  
  geom_errorbar(aes(x = hapto,
                    y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    xmin = hapto-hapto_sd, 
                    xmax = hapto+hapto_sd,
                    width = 0.25),
                data = abundances_summary) +
  xlab(expression(paste("Haptophytes (", mu, "g", l^-1,")", sep=""))) +
  ylab("Treatment") +
  scale_y_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  facet_wrap(~Bioassay, nrow = 1)

ggsave(all_bioassay_hapto, filename = "figures/Biomass_horizontal/all_bioassay_hapto.png",
       device = "png", height = 12, width = 15)

####################################as percent change

percent_change = read_excel("data/PhytoClass_data.xlsx", sheet = "percent_change")

glimpse(percent_change)

percent_change$Bioassay = factor(percent_change$Bioassay, levels=c("May", "June", "July", "September")) 


#####total chl a

percent_change_total_chl_a = ggplot(data = percent_change, 
                                  aes(x = Total_Chl_a, y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = Total_Chl_a,
               y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = percent_change) +  
  xlab("Percent Change from Control") +
  ylab("Treatment") +
  scale_y_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  facet_wrap(~Bioassay, nrow = 1)

ggsave(percent_change_total_chl_a, filename = "figures/Percent_Change_horizontal/percent_change_total_chl_a.png",
       device = "png", height = 12, width = 15)

#####Cyanobacteria

percent_change_cyano = ggplot(data = percent_change, 
                                    aes(x = Cyanobacteria, y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = Cyanobacteria,
               y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = percent_change) +  
  xlab("Percent Change from Control") +
  ylab("Treatment") +
  scale_y_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  facet_wrap(~Bioassay, nrow = 1)

ggsave(percent_change_cyano, filename = "figures/Percent_Change_horizontal/percent_change_cyano.png",
       device = "png", height = 12, width = 15)

#####Green Algae

percent_change_green_algae = ggplot(data = percent_change, 
                              aes(x = Green_Algae, y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = Green_Algae,
               y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = percent_change) +  
  xlab("Percent Change from Control") +
  ylab("Treatment") +
  scale_y_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  facet_wrap(~Bioassay, nrow = 1)

ggsave(percent_change_green_algae, filename = "figures/Percent_Change_horizontal/percent_change_green_algae.png",
       device = "png", height = 12, width = 15)

#####Cryptophytes

percent_change_crypto = ggplot(data = percent_change, 
                                    aes(x = Cryptophytes, y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = Cryptophytes,
               y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = percent_change) +  
  xlab("Percent Change from Control") +
  ylab("Treatment") +
  scale_y_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  facet_wrap(~Bioassay, nrow = 1)

ggsave(percent_change_crypto, filename = "figures/Percent_Change_horizontal/percent_change_crypto.png",
       device = "png", height = 12, width = 15)

#####Diatoms

percent_change_diatoms = ggplot(data = percent_change, 
                               aes(x = Diatoms, y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = Diatoms,
               y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = percent_change) +  
  xlab("Percent Change from Control") +
  ylab("Treatment") +
  scale_y_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  facet_wrap(~Bioassay, nrow = 1)

ggsave(percent_change_diatoms, filename = "figures/Percent_Change_horizontal/percent_change_diatoms.png",
       device = "png", height = 12, width = 15)

#####Dinoflagellates

percent_change_dino = ggplot(data = percent_change, 
                               aes(x = Dinoflagellates, y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = Dinoflagellates,
               y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = percent_change) +  
  xlab("Percent Change from Control") +
  ylab("Treatment") +
  scale_y_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  facet_wrap(~Bioassay, nrow = 1)

ggsave(percent_change_dino, filename = "figures/Percent_Change_horizontal/percent_change_dino.png",
       device = "png", height = 12, width = 15)

#####Haptophytes

percent_change_hapto = ggplot(data = percent_change, 
                               aes(x = Haptophytes, y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = Haptophytes,
               y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = percent_change) +  
  xlab("Percent Change from Control") +
  ylab("Treatment") +
  scale_y_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  facet_wrap(~Bioassay, nrow = 1)

ggsave(percent_change_hapto, filename = "figures/Percent_Change_horizontal/percent_change_hapto.png",
       device = "png", height = 12, width = 15)
