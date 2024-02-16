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

summary_data = pc_abundances %>%
  group_by(Treatment) %>%
  summarise(
    mean = mean(Concentration),
    mean_ln = mean(ln_concentration),
    sd = sd(Concentration, na.rm = TRUE),
    sd_ln = sd(ln_concentration, na.rm = TRUE),
  )

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

ggsave(all_bioassay_total_chl_a, filename = "C:/Users/cathe/Desktop/MS Thesis/MS_Thesis_2023-2024/figures/all_bioassay_total_chl_a.png",
       device = "png", height = 12, width = 15)





