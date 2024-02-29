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

glimpse(pc)

########################################total chl a

pc_1 = pc %>%
  select("Bioassay_3", "Treatment_3", "Total_Chl_a_4")

glimpse(pc_1)

summary_data = pc_1 %>%
  group_by(Bioassay_3, Treatment_3) %>%
  summarise(
    mean = mean(Total_Chl_a_4),
    sd = sd(Total_Chl_a_4, na.rm = TRUE),
  ) %>%
  na.omit()

print(summary_data, n = 22)

summary_data$Bioassay_3 = factor(summary_data$Bioassay_3, levels=c("May", "June", "July", "September")) 

total_chl_a_pc = ggplot(data = summary_data, 
                                  aes(x = mean, y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = mean,
               y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = summary_data) +  
  geom_errorbar(aes(x = mean,
                    y = fct_relevel(Treatment_3, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    xmin = mean-sd, 
                    xmax = mean+sd,
                    width = 0.25),
                data = summary_data) +
  xlab("Percent Change") +
  ylab("Treatment") +
  scale_y_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 14) +
  facet_wrap(~Bioassay_3, nrow = 1)

ggsave(total_chl_a_pc, filename = "figures/Percent_Change_horizontal/total_chl_a_pc.png",
       device = "png", height = 12, width = 15)


aov1 = aov(Total_Chl_a_4 ~ factor(Treatment_3) + factor(Bioassay_3), data = pc_1)
summary(aov1)

aov1.2 = aov(Total_Chl_a_4 ~ factor(Treatment_3) + factor(Bioassay_3) + Treatment_3:Bioassay_3, data = pc_1)
summary(aov1.2)
