#2023-11-16
#combined bar chart for chl a for all months

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)

all_bioassay = read_excel("data/2023-11-17_All_Bioassay_Chl_a.xlsx", sheet = "Sheet1")

summary(all_bioassay)
glimpse(all_bioassay)

variable_names = list("May" = "A", "June" = "B", "July" = "C", "September" = "D")

variable_labeller = function(variable,value){
  return(variable_names[value])
  }

All_Chl_a_bargraph = ggplot(data = all_bioassay, 
       aes(x = fct_relevel(Treatment, "T_0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), y=Chl_a)) +
  geom_bar(stat='identity', fill="darkgrey") +
  xlab("Treatment") +
  ylab(expression(paste("Chl a \u03BCg", l^-1))) +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN+LP", "DIN_HP" = "DIN+HP")) +
  facet_wrap(~factor(Bioassay, levels=c('May', 'June', 'July', 'September')),  ncol=2, labeller=variable_labeller) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0)) + 
  ylim(0, 60)
ggsave(All_Chl_a_bargraph, filename = "figures/All_Chl_a_bargraph.png",
       device = "png", height = 7, width = 11)
