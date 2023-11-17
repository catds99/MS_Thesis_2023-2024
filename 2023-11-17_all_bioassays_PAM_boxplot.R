#2023-11-17
#combined bar chart for Fv/Fm for all months

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)

all_bioassay = read_excel("data/2023-11-17_All_Bioassay_PAM.xlsx", sheet = "Sheet2")

summary(all_bioassay)
glimpse(all_bioassay)

variable_names = list("June" = "A", "July" = "B", "September" = "C")

variable_labeller = function(variable,value){
  return(variable_names[value])
}

All_PAM_boxplot = ggplot() +
  geom_boxplot(aes(x = fct_relevel(Treatment, "T_0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                   y = Fv_Fm),
               fill = "darkgrey",
               data = all_bioassay) +
  xlab("Treatment") +
  ylab(expression(F[v]/F[m])) +
  scale_x_discrete(labels= c("T_0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN+LP", "DIN_HP" = "DIN+HP")) +
  facet_wrap(~factor(Date, levels=c('June', 'July', 'September')),  ncol=3, labeller=variable_labeller) +
  theme_classic(base_size = 11) +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0))
ggsave(All_PAM_boxplot, filename = "figures/All_PAM_boxplot.png",
       device = "png", height = 7, width = 11)
