#2024-02-16
#barplots for all bioassay with and without transformed data

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)


pc = read_excel("data/2024-02-20_Percent_change_data.xlsx", sheet = "2")

pc_dh = read_excel("data/2024-03-12_percent_change_data_with_dh.xlsx", sheet = "Sheet2")

glimpse(pc)

glimpse(pc_dh)

summary = pc %>%
  group_by(Bioassay_3, Treatment_3) %>%
  summarise(
    mean_tca = mean(Total_Chl_a_4),
    sd_tca = sd(Total_Chl_a_4, na.rm = TRUE),
    mean_cyano = mean(Cyanobacteria_4),
    sd_cyano = sd(Cyanobacteria_4, na.rm = TRUE),
    mean_crypto = mean(Cryptophytes_4),
    sd_crypto = sd(Cryptophytes_4, na.rm = TRUE),
    mean_diatom = mean(Diatoms_4),
    sd_diatom = sd(Diatoms_4, na.rm = TRUE),
    mean_dino = mean(Dinoflagellates_4),
    sd_dino = sd(Dinoflagellates_4, na.rm = TRUE),
    mean_ga = mean(Green_Algae_4),
    sd_ga = sd(Green_Algae_4, na.rm = TRUE),
    mean_hapto = mean(Haptophytes_4),
    sd_hapto = sd(Haptophytes_4, na.rm = TRUE),
  )

summary = summary[-c(21), ]

summary$Bioassay_3 = factor(summary$Bioassay_3, levels=c("May", "June", "July", "September")) 

print(summary)

summary_dh = pc_dh %>%
  group_by(Bioassay_3, Treatment_3) %>%
  summarise(
    mean_dh = mean(dh_4),
    sd_dh = sd(dh_4, na.rm = TRUE),
  )

summary_dh = summary_dh[-c(21), ]

summary_dh$Bioassay_3 = factor(summary_dh$Bioassay_3, levels=c("May", "June", "July", "September")) 

print(summary_dh)

######################################## total chl a

tca_pc = ggplot(data = summary, 
                aes(x = mean_tca, y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = mean_tca,
               y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = summary) +  
  geom_errorbar(aes(x = mean_tca,
                    y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    xmin = mean_tca-sd_tca, 
                    xmax = mean_tca+sd_tca,
                    width = 0.25),
                data = summary) +
  xlab("Percent Change") +
  ylab("Treatment") +
  scale_y_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 25) +
  facet_wrap(~Bioassay_3, nrow = 1)

ggsave(tca_pc, filename = "figures/Percent_Change_horizontal/tca_pc.png",
       device = "png", height = 12, width = 15)










######################################## cyanobacteria

cyano_pc = ggplot(data = summary, 
                        aes(x = mean_cyano, y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = mean_cyano,
               y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = summary) +  
  geom_errorbar(aes(x = mean_cyano,
                    y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    xmin = mean_cyano-sd_cyano, 
                    xmax = mean_cyano+sd_cyano,
                    width = 0.25),
                data = summary) +
  xlab("Percent Change") +
  ylab("Treatment") +
  scale_y_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 25) +
  facet_wrap(~Bioassay_3, nrow = 1)

ggsave(cyano_pc, filename = "figures/Percent_Change_horizontal/cyano_pc.png",
       device = "png", height = 12, width = 15)











######################################## cryptophytes

crypto_pc = ggplot(data = summary, 
                  aes(x = mean_crypto, y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = mean_crypto,
               y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = summary) +  
  geom_errorbar(aes(x = mean_crypto,
                    y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    xmin = mean_crypto-sd_crypto, 
                    xmax = mean_crypto+sd_crypto,
                    width = 0.25),
                data = summary) +
  xlab("Percent Change") +
  ylab("Treatment") +
  scale_y_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 25) +
  facet_wrap(~Bioassay_3, nrow = 1)

ggsave(crypto_pc, filename = "figures/Percent_Change_horizontal/crypto_pc.png",
       device = "png", height = 12, width = 15)











######################################## diatoms

diatom_pc = ggplot(data = summary, 
                   aes(x = mean_diatom, y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = mean_diatom,
               y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = summary) +  
  geom_errorbar(aes(x = mean_diatom,
                    y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    xmin = mean_diatom-sd_diatom, 
                    xmax = mean_diatom+sd_diatom,
                    width = 0.25),
                data = summary) +
  xlab("Percent Change") +
  ylab("Treatment") +
  scale_y_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 25) +
  facet_wrap(~Bioassay_3, nrow = 1)

ggsave(diatom_pc, filename = "figures/Percent_Change_horizontal/diatom_pc.png",
       device = "png", height = 12, width = 17)











######################################## dinoflagellates

dino_pc = ggplot(data = summary, 
                   aes(x = mean_dino, y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = mean_dino,
               y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = summary) +  
  geom_errorbar(aes(x = mean_dino,
                    y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    xmin = mean_dino-sd_dino, 
                    xmax = mean_dino+sd_dino,
                    width = 0.25),
                data = summary) +
  xlab("Percent Change") +
  ylab("Treatment") +
  scale_y_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 25) +
  facet_wrap(~Bioassay_3, nrow = 1)

ggsave(dino_pc, filename = "figures/Percent_Change_horizontal/dino_pc.png",
       device = "png", height = 12, width = 15)













######################################## green algae

ga_pc = ggplot(data = summary, 
                 aes(x = mean_ga, y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = mean_ga,
               y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = summary) +  
  geom_errorbar(aes(x = mean_ga,
                    y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    xmin = mean_ga-sd_ga, 
                    xmax = mean_ga+sd_ga,
                    width = 0.25),
                data = summary) +
  xlab("Percent Change") +
  ylab("Treatment") +
  scale_y_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 25) +
  facet_wrap(~Bioassay_3, nrow = 1)

ggsave(ga_pc, filename = "figures/Percent_Change_horizontal/ga_pc.png",
       device = "png", height = 12, width = 15)













######################################## haptophytes

hapto_pc = ggplot(data = summary, 
               aes(x = mean_hapto, y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = mean_hapto,
               y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = summary) +  
  geom_errorbar(aes(x = mean_hapto,
                    y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    xmin = mean_hapto-sd_hapto, 
                    xmax = mean_hapto+sd_hapto,
                    width = 0.25),
                data = summary) +
  xlab("Percent Change") +
  ylab("Treatment") +
  scale_y_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 25) +
  facet_wrap(~Bioassay_3, nrow = 1)

ggsave(hapto_pc, filename = "figures/Percent_Change_horizontal/hapto_pc.png",
       device = "png", height = 12, width = 15)

















######################################## diatoms/haptophytes

dh_pc = ggplot(data = summary_dh, 
                  aes(x = mean_dh, y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = mean_dh,
               y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = summary_dh) +  
  geom_errorbar(aes(x = mean_dh,
                    y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    xmin = mean_dh-sd_dh, 
                    xmax = mean_dh+sd_dh,
                    width = 0.25),
                data = summary_dh) +
  xlab("Percent Change") +
  ylab("Treatment") +
  scale_y_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 25) +
  facet_wrap(~Bioassay_3, nrow = 1)

ggsave(dh_pc, filename = "figures/Percent_Change_horizontal/dh_pc.png",
       device = "png", height = 12, width = 15)
