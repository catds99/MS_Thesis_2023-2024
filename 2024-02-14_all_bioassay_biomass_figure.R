#2024-02-14
#biomass plot code 


B1 = read_excel("C:/Users/cathe/Desktop/MS Thesis/MS_Thesis_2023-2024/data/all_bioassays_percents.xlsx", sheet = "B1")

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
