#graphing percent of control chl a nd percent change from control for total chl a
#2023-10-16

library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(car)
library(rstatix)

install.packages("AICcmodavg")
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

install.packages("agricolae")
library(agricolae)

#import data

all_bioassay = read_excel("data/all_bioassays_percents.xlsx", sheet = 1)

glimpse(all_bioassay)

all_avg = all_bioassay %>%
  group_by(Treatment) %>%
  summarise(Percent_of_control_avg = mean(Percent_of_control),
            Percent_dif_from_control_avg = mean(Percent_dif_from_control))

#percent of control

chl_a_percent_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Percent_of_control_avg),
           fill = "darkgrey",
           data = all_avg) +
  xlab("Treatment") +
  ylab("Percent of Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1100, label="Total Chl a", color="black") +
  theme_classic(base_size = 12) 
ggsave(chl_a_percent_control, filename = "figures/chl_a_percent_control.png",
       device = "png", height = 7, width = 11)

#all chl a percent difference from control

chl_a_percent_change_control = ggplot() +
  geom_col(aes(x = fct_relevel(Treatment, "DIN", "LP", "HP", "DIN_LP", "DIN_HP"), 
               y = Percent_dif_from_control_avg),
           fill = "darkgrey",
           data = all_avg) +
  xlab("Treatment") +
  ylab("Percent Change from Control") +
  scale_x_discrete(labels= c("DIN" = "DIN", "LP" = "LP", "HP" = "HP", "DIN_LP" = "DIN + LP", "DIN_HP" = "DIN + HP")) +
  annotate(geom="text", x=0.9, y=1000, label="Total Chl a", color="black") +
  theme_classic(base_size = 12) 
ggsave(chl_a_percent_change_control, filename = "figures/chl_a_percent_change_control.png",
       device = "png", height = 7, width = 11)


####################################running stats for percent change from control

#import data:

all_bioassay = read_excel("data/all_bioassays_percents.xlsx", sheet = 1)

glimpse(all_bioassay)

all_avg = all_bioassay %>%
  group_by(Treatment) %>%
  summarise(Percent_of_control_avg = mean(Percent_of_control),
            Percent_dif_from_control_avg = mean(Percent_dif_from_control))

#Check for normality

model = lm(Percent_dif_from_control ~ Treatment, data = all_bioassay)

ggqqplot(residuals(model))

shapiro_test(residuals(model))

  #not normal (either way), so use non-parametric

#check for homogeneity of variances

leveneTest(all_bioassay$Percent_dif_from_control ~ all_bioassay$Treatment)
  #this passed, so we could potentially use a parametric test

#Parametric ANOVA:

all_bioassay_percent_change_ANOVA = oneway.test(Percent_dif_from_control ~ Treatment,
                        data = all_bioassay,
                        var.equal = TRUE)

#Non-parametric ANOVA (Welch's) 

oneway.test(Percent_dif_from_control ~ Treatment,
            data = all_bioassay,
            var.equal = FALSE)

#Another parametric ANOVA:

all_percent_change_ANOVA = aov(Percent_dif_from_control~Treatment, data=all_bioassay)

summary(all_percent_change_ANOVA)

print(all_percent_change_ANOVA)

#Tukey post-hoc testing (parametric)

all_percent_change_Tukey = tukey_hsd(all_percent_change_ANOVA, conf.level = 0.95)

TukeyHSD(all_percent_change_ANOVA)

#Games-Howell post-hoc testing (non-parametric)

all_percent_change_GH = games_howell_test(formula = Percent_dif_from_control ~ Treatment,
                                        data = all_bioassay, 
                                        conf.level = 0.95, 
                                        detailed = FALSE)

print(all_percent_change_GH, n = 22)

#Another non-parametric version (kruskal-wallis for the ANOVA then Mann Whitney U test fpr post-hoc):

kruskal.test(Percent_dif_from_control ~ Treatment, data = all_bioassay)

wilcox.test(Percent_dif_from_control ~ Treatment, data = all_bioassay, exact = FALSE)

post_hoc = REGW.test(y = all_percent_change_ANOVA, "Treatment", alpha = 0.05, group = FALSE, main = NULL, console = FALSE)
  #to get groups of treatments, use group = TRUE, for comparisons and p-values of all treatments use group = FALSE
print(post_hoc)
