#2023-12-22 
#MANOVAS for community composition with PhytoClass Data

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)
library(ggpubr)
library(broom)

#import data:

hapto_pigments = read_excel("data/hapto-pigments.xlsx", sheet = "Sheet2")

summary(hapto_pigments)
glimpse(hapto_pigments)


aov.1 = aov(HexFuc ~ Treatment + factor(Bioassay), data = hapto_pigments)

summary(aov.1)

summary = hapto_pigments %>%
  group_by(Bioassay, Treatment) %>%
  summarise(
    mean = mean(HexFuc),
    sd = sd(HexFuc, na.rm = TRUE),
  )

print(summary)

hexfuc = ggplot(data = summary, 
                aes(x = mean, y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"))) + 
  geom_col(aes(x = mean,
               y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP")),
           fill = "darkgrey",
           data = summary) +  
  geom_errorbar(aes(x = mean,
                    y = fct_relevel(Treatment, "T0", "Control", "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
                    xmin = mean-sd, 
                    xmax = mean+sd,
                    width = 0.25),
                data = summary) +
  xlab("Concentration") +
  ylab("Treatment") +
  scale_y_discrete(labels= c("T0" = "Time Zero", "Control" = "Control", "DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  theme_classic(base_size = 25) +
  facet_wrap(~Bioassay, nrow = 1)

june = hapto_pigments %>%
  filter(Bioassay == "June")

july = hapto_pigments %>%
  filter(Bioassay == "July")


aov.june = aov(HexFuc ~ Treatment, data = june)
summary(aov.june)

REGW_june = REGW.test(y = aov.june, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
print(REGW_june)


aov.july = aov(HexFuc ~ Treatment, data = july)
summary(aov.july)

REGW_july = REGW.test(y = aov.july, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
print(REGW_july)
