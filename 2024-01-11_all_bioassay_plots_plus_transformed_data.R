#2024-01-11
#barplots for all bioassay with and without transformed data

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)

#reference code:

pc_mean_abundances = read_excel("data/PhytoClass_data.xlsx", sheet = "mean_abundances")

summary(pc_mean_abundances)
glimpse(pc_mean_abundances)

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

pc_percent_change = read_excel("data/PhytoClass_data.xlsx", sheet = "percent_change")

summary(pc_percent_change)
glimpse(pc_percent_change)

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
  geom_text(aes(label = c("565.57%", "779.14%", "693.61%", "24.37%", "46.03%"), 
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


###################################################################################


B1 = read_excel("data/all_bioassays_percents.xlsx", sheet = "B1")

glimpse(B1)

B1_summary = B1 %>%
  group_by(Treatment) %>%
  summarise(
    mean = mean(Concentration),
    mean_ln = mean(ln_concentration),
    sd = sd(Concentration, na.rm = TRUE),
    sd_ln = sd(ln_concentration, na.rm = TRUE),
  )
B1_summary

B1_plot = ggplot(data = B1_summary, 
       aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=mean)) +
  geom_col(aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = mean),
           fill = "darkgrey",
           data = B1_summary) +
  geom_errorbar(aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    y = mean,
                    ymin = mean-sd, 
                    ymax = mean+sd,
                    width = 0.25),
                data = B1_summary) +
  xlab("Treatment") +
  ylab(expression(paste("Total Chl", italic("a"), "(", mu, "g", l^-1,")", sep="")))+
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  theme(axis.title.x=element_blank())+
  geom_text(aes(label = c("3.52", "21.4", "28.9", "22.4", "4.39", "5.12", "5.18"), 
               x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = -5), 
           vjust = -0.5,
           data = B1_summary) +
  ylim(-5, 60)

B2 = read_excel("data/all_bioassays_percents.xlsx", sheet = "B2")

glimpse(B2)

B2_summary = B2 %>%
  group_by(Treatment) %>%
  summarise(
    mean = mean(Concentration),
    mean_ln = mean(ln_concentration),
    sd = sd(Concentration, na.rm = TRUE),
    sd_ln = sd(ln_concentration, na.rm = TRUE),
  )
B2_summary

B2_plot = ggplot(data = B2_summary, 
       aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=mean)) +
  geom_col(aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = mean),
           fill = "darkgrey",
           data = B2_summary) +
  geom_errorbar(aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    y = mean,
                    ymin = mean-sd, 
                    ymax = mean+sd,
                    width = 0.25),
                data = B2_summary) +
  xlab("Treatment") +
  ylab(expression(paste("Total Chl", italic("a"), "(", mu, "g", l^-1,")", sep="")))+
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  theme(axis.title.y=element_blank(), axis.title.x=element_blank())+
  geom_text(aes(label = c("3.38", "25.7", "51.2", "49.1", "4.70", "5.06", "5.38"), 
                x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                y = -5), 
            vjust = -0.5,
            data = B2_summary) +
  ylim(-5, 60)

B3 = read_excel("data/all_bioassays_percents.xlsx", sheet = "B3")

glimpse(B3)

B3_summary = B3 %>%
  group_by(Treatment) %>%
  summarise(
    mean = mean(Concentration),
    mean_ln = mean(ln_concentration),
    sd = sd(Concentration, na.rm = TRUE),
    sd_ln = sd(ln_concentration, na.rm = TRUE),
  )
B3_summary

B3_plot = ggplot(data = B3_summary, 
       aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=mean)) +
  geom_col(aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = mean),
           fill = "darkgrey",
           data = B3_summary) +
  geom_errorbar(aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    y = mean,
                    ymin = mean-sd, 
                    ymax = mean+sd,
                    width = 0.25),
                data = B3_summary) +
  xlab("Treatment") +
  ylab(expression(paste("Total Chl", italic("a"), "(", mu, "g", l^-1,")", sep="")))+
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  geom_text(aes(label = c("4.15", "26.5", "27.4", "23.7", "4.08", "5.24", "4.86"), 
                x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                y = -5), 
            vjust = -0.5,
            data = B3_summary) +
  ylim(-5, 60)

B4 = read_excel("data/all_bioassays_percents.xlsx", sheet = "B4")

glimpse(B4)

B4_summary = B4 %>%
  group_by(Treatment) %>%
  summarise(
    mean = mean(Concentration),
    mean_ln = mean(ln_concentration),
    sd = sd(Concentration, na.rm = TRUE),
    sd_ln = sd(ln_concentration, na.rm = TRUE),
  )
B4_summary

B4_plot = ggplot(data = B4_summary, 
       aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=mean)) +
  geom_col(aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = mean),
           fill = "darkgrey",
           data = B4_summary) +
  geom_errorbar(aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    y = mean,
                    ymin = mean-sd, 
                    ymax = mean+sd,
                    width = 0.25),
                data = B4_summary) +
  xlab("Treatment") +
  ylab(expression(paste("Total Chl", italic("a"), "(", mu, "g", l^-1,")", sep="")))+
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  theme(axis.title.y=element_blank())+
  geom_text(aes(label = c("7.07", "46.3", "36.8", "36.6", "9.59", "11.5", "14.1"), 
                x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                y = -5), 
            vjust = -0.5,
            data = B4_summary) +
  ylim(-5, 60)


biomass_plot = ggarrange(B1_plot, B2_plot, B3_plot, B4_plot,
                              labels = c("A", "B", "C", "D"),
                              ncol = 2, nrow = 2)

ggsave(biomass_plot, filename = "figures/biomass_plot.png",
       device = "png", height = 10, width = 11)

#####################################################same but ln

B1 = read_excel("data/all_bioassays_percents.xlsx", sheet = "B1")

glimpse(B1)

B1_summary = B1 %>%
  group_by(Treatment) %>%
  summarise(
    mean = mean(Concentration),
    mean_ln = mean(ln_concentration),
    sd = sd(Concentration, na.rm = TRUE),
    sd_ln = sd(ln_concentration, na.rm = TRUE),
  )
B1_summary

B1_plot_ln = ggplot(data = B1_summary, 
                 aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=mean_ln)) +
  geom_col(aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = mean_ln),
           fill = "darkgrey",
           data = B1_summary) +
  geom_errorbar(aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    y = mean_ln,
                    ymin = mean_ln-sd_ln, 
                    ymax = mean_ln+sd_ln,
                    width = 0.25),
                data = B1_summary) +
  xlab("Treatment") +
  ylab(expression(paste("ln(total Chl", italic("a"), "(", mu, "g", l^-1,"))", sep="")))+
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  theme(axis.title.x=element_blank())+
  geom_text(aes(label = c("1.25", "3.04", "3.36", "3.08", "1.48", "1.62", "1.64"), 
                x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                y = -0.25), 
            vjust = -0.5,
            data = B1_summary) +
  ylim(-0.25, 5)

B2 = read_excel("data/all_bioassays_percents.xlsx", sheet = "B2")

glimpse(B2)

B2_summary = B2 %>%
  group_by(Treatment) %>%
  summarise(
    mean = mean(Concentration),
    mean_ln = mean(ln_concentration),
    sd = sd(Concentration, na.rm = TRUE),
    sd_ln = sd(ln_concentration, na.rm = TRUE),
  )
B2_summary

B2_plot_ln = ggplot(data = B2_summary, 
                 aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=mean_ln)) +
  geom_col(aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = mean_ln),
           fill = "darkgrey",
           data = B2_summary) +
  geom_errorbar(aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    y = mean_ln,
                    ymin = mean_ln-sd_ln, 
                    ymax = mean_ln+sd_ln,
                    width = 0.25),
                data = B2_summary) +
  xlab("Treatment") +
  ylab(expression(paste("ln(total Chl", italic("a"), "(", mu, "g", l^-1,"))", sep="")))+
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  theme(axis.title.y=element_blank(), axis.title.x=element_blank())+
  geom_text(aes(label = c("1.21", "3.25", "3.94", "3.89", "1.54", "1.61", "1.68"), 
                x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                y = -0.25), 
            vjust = -0.5,
            data = B2_summary) +
  ylim(-0.25, 5)

B3 = read_excel("data/all_bioassays_percents.xlsx", sheet = "B3")

glimpse(B3)

B3_summary = B3 %>%
  group_by(Treatment) %>%
  summarise(
    mean = mean(Concentration),
    mean_ln = mean(ln_concentration),
    sd = sd(Concentration, na.rm = TRUE),
    sd_ln = sd(ln_concentration, na.rm = TRUE),
  )
B3_summary

B3_plot_ln = ggplot(data = B3_summary, 
                 aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=mean_ln)) +
  geom_col(aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = mean_ln),
           fill = "darkgrey",
           data = B3_summary) +
  geom_errorbar(aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    y = mean_ln,
                    ymin = mean_ln-sd_ln, 
                    ymax = mean_ln+sd_ln,
                    width = 0.25),
                data = B3_summary) +
  xlab("Treatment") +
  ylab(expression(paste("ln(total Chl", italic("a"), "(", mu, "g", l^-1,"))", sep="")))+
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  geom_text(aes(label = c("1.42", "3.27", "3.31", "3.14", "1.41", "1.65", "1.58"), 
                x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                y = -0.25), 
            vjust = -0.5,
            data = B3_summary) +
  ylim(-0.25, 5)

B4 = read_excel("data/all_bioassays_percents.xlsx", sheet = "B4")

glimpse(B4)

B4_summary = B4 %>%
  group_by(Treatment) %>%
  summarise(
    mean = mean(Concentration),
    mean_ln = mean(ln_concentration),
    sd = sd(Concentration, na.rm = TRUE),
    sd_ln = sd(ln_concentration, na.rm = TRUE),
  )
B4_summary

B4_plot_ln = ggplot(data = B4_summary, 
                 aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=mean_ln)) +
  geom_col(aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = mean_ln),
           fill = "darkgrey",
           data = B4_summary) +
  geom_errorbar(aes(x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    y = mean_ln,
                    ymin = mean_ln-sd_ln, 
                    ymax = mean_ln+sd_ln,
                    width = 0.25),
                data = B4_summary) +
  xlab("Treatment") +
  ylab(expression(paste("ln(total Chl", italic("a"), "(", mu, "g", l^-1,"))", sep="")))+
  scale_x_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  theme(axis.title.y=element_blank())+
  geom_text(aes(label = c("1.95", "3.83", "3.60", "3.59", "2.26", "2.42", "2.64"), 
                x = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                y = -0.25), 
            vjust = -0.5,
            data = B4_summary) +
  ylim(-0.25, 5)


biomass_plot_ln = ggarrange(B1_plot_ln, B2_plot_ln, B3_plot_ln, B4_plot_ln,
                         labels = c("A", "B", "C", "D"),
                         ncol = 2, nrow = 2)

ggsave(biomass_plot_ln, filename = "figures/biomass_plot_ln.png",
       device = "png", height = 10, width = 11)
