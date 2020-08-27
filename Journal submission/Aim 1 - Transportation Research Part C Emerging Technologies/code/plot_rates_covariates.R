pacman::p_load(data.table, magrittr, ggplot2, dplyr)
d = fread("data/Aim1_crash_SCE.csv") %>%
  .[,`:=`(sce_CM = sce_CM*10000/distance,
          sce_HB = sce_HB*10000/distance,
          sce_HW = sce_HW*10000/distance,
          sce_RS = sce_RS*10000/distance,
          sce_N = sce_N*10000/distance,
          rate_crash = n_crash*10000/distance)] %>%
  .[!(driver %in%
        c("abad", "beaj10", "beni0", "bonr7", "booa0", "brad6", "bror7",
          "busk2", "carr5", "clec25", "cunj19", "cure", "dund52", "gar15r",
          "grit21", "gros", "guar", "gulc1", "gurj", "hal166", "harj2",
          "higt12", "host", "johj48", "johw9", "jons86", "kena", "kesk",
          "kisi", "kopc", "lamr27", "lasr2", "leeb1", "lege3", "manr4",
          "mezj0", "mitm3", "monr83", "ouza1", "pert22", "phij2", "rivl13",
          "ruba0", "ruic4", "salm37", "sanm13", "shar3", "sopp2", "thog50",
          "thog76", "thom12", "thori1", "upsj")),] %>% # "denw17"
  .[rate_crash<1000,]

######## Figure: crash_SCE.pdf #####
p_SCE = d %>%
  ggplot(aes(sce_N, rate_crash)) +
  geom_point(alpha = 0.3) + geom_smooth(span = 0.3) +
  labs(x = "any type of SCEs per 10,000 miles",
       y = "crashes per 10,000 miles",
       subtitle = "SCEs overall") + theme_classic() +
  theme(plot.subtitle = element_text(size=12, hjust=0.5,
                                     face="bold", color="black"))
p_HB = d %>%
  ggplot(aes(sce_HB, rate_crash)) +
  geom_point(alpha = 0.3) + geom_smooth(span = 0.3) +
  labs(x = "hard brakes per 10,000 miles",
       y = NULL, subtitle = "hard brake") + theme_classic() +
  theme(plot.subtitle = element_text(size=12, hjust=0.5,
                                     face="bold", color="black"))
p_HW = d %>%
  ggplot(aes(sce_HW, rate_crash)) +
  geom_point(alpha = 0.3) + geom_smooth(span = 0.3) +
  labs(x = "headways per 10,000 miles",
       y = NULL, subtitle = "headway") + theme_classic() +
  theme(plot.subtitle = element_text(size=12, hjust=0.5,
                                     face="bold", color="black"))
p_RS = d %>%
  ggplot(aes(sce_RS, rate_crash)) +
  geom_point(alpha = 0.3) + geom_smooth(span = 0.3) +
  labs(x = "rolling stability per 10,000 miles",
       y = NULL, subtitle = "rolling stability") + theme_classic() +
  theme(plot.subtitle = element_text(size=12, hjust=0.5,
                                     face="bold", color="black"))
p_CM = d %>%
  ggplot(aes(sce_CM, rate_crash)) +
  geom_point(alpha = 0.3) + geom_smooth(span = 0.3) +
  labs(x = "collision mitigation per 10,000 miles",
       y = NULL, subtitle = "collision mitigation") + theme_classic() +
  theme(plot.subtitle = element_text(size=12, hjust=0.5,
                                     face="bold", color="black"))

fig1 = ggpubr::ggarrange(
  p_SCE,
  ggpubr::ggarrange(p_HB, p_HW, p_RS, p_CM, ncol = 2, nrow = 2),
  nrow = 2
)

ggsave("figs/crash_SCE.pdf", fig1, width = 6, height = 6)
ggsave("figs/crash_SCE.jpeg", fig1, width = 6, height = 6, dpi = 300)
ggsave("figs/crash_SCE.bmp", fig1, width = 6, height = 6, dpi = 300)
ggsave("figs/crash_SCE.wmf", fig1, width = 6, height = 6, dpi = 300)




######## Figure: other_covariate.pdf #####
p_age1 = d %>%
  ggplot(aes(age, rate_crash)) +
  geom_point(alpha = 0.3) + geom_smooth(span = 0.3) +
  labs(x = "driver age",
       y = "crash rate",
       subtitle = "age") + theme_classic() +
  theme(plot.subtitle = element_text(size=12, hjust=0.5,
                                     face="bold", color="black"))
p_age2 = d %>%
  mutate(age_cat = case_when(age <= 30 ~ "<=30",
                             age > 30 & age <= 40 ~ "30-40",
                             age > 40 & age <= 50 ~ "40-50",
                             age > 50 & age <= 60 ~ "50-60",
                             age > 60 & age <= 70 ~ "60-70",
                             age > 70 ~ ">70")) %>%
  mutate(age_cat = factor(age_cat, levels =
                            c("<=30", "30-40", "40-50", "50-60",
                              "60-70", ">70"))) %>%
  ggplot(aes(age_cat, rate_crash)) +
  geom_boxplot() +
  labs(x = "age category",
       y = NULL,
       subtitle = "age") + theme_classic() +
  theme(plot.subtitle = element_text(size=12, hjust=0.5,
                                     face="bold", color="black"))

p_speed1 = d %>%
  ggplot(aes(ping_speed, rate_crash)) +
  geom_point(alpha = 0.3) + geom_smooth(span = 0.3) +
  labs(x = "mean speed",
       y = "crash rate",
       subtitle = "mean speed") + theme_classic() +
  theme(plot.subtitle = element_text(size=12, hjust=0.5,
                                     face="bold", color="black"))

p_speed2 = d %>%
  mutate(speed_cat = case_when(ping_speed <= 10 ~ "<= 10",
                               ping_speed > 10 & ping_speed <= 20 ~ "10-20",
                               ping_speed > 20 & ping_speed <= 30 ~ "20-30",
                               ping_speed > 30 & ping_speed <= 40 ~ "30-40",
                               ping_speed > 40 & ping_speed <= 50 ~ "40-50",
                               ping_speed > 50 & ping_speed <= 60 ~ "50-60",
                               ping_speed > 60 ~ ">60")) %>%
  mutate(speed_cat = factor(speed_cat, levels =
                              c("<= 10", "10-20", "20-30", "30-40",
                                "40-50", "50-60", ">60"))) %>%
  ggplot(aes(speed_cat, rate_crash)) +
  geom_boxplot() +
  labs(x = "mean speed",
       y = NULL,
       subtitle = "mean speed") + theme_classic() +
  theme(plot.subtitle = element_text(size=12, hjust=0.5,
                                     face="bold", color="black"))

p_male = d %>%
  mutate(gender = case_when(gender == "M" ~ "Male",
                            gender == "F" ~ "Female",
                            gender == "U" ~ "Unknown")) %>%
  ggplot(aes(gender, rate_crash)) +
  geom_boxplot() +
  labs(x = "Gender",
       y = "crash rate",
       subtitle = "Gender") + theme_classic() +
  theme(plot.subtitle = element_text(size=12, hjust=0.5,
                                     face="bold", color="black"))

p_bus = d %>%
  ggplot(aes(bus_unit, rate_crash)) +
  geom_boxplot() +
  labs(x = "Business units",
       y = NULL,
       subtitle = "business unit") + theme_classic() +
  theme(plot.subtitle = element_text(size=12, hjust=0.5,
                                     face="bold", color="black"))
p_type = d %>%
  filter(!(bus_unit == "VAN00" & d_type == "LOC")) %>%
  ggplot(aes(d_type, rate_crash)) +
  geom_boxplot() +
  labs(x = "Driver types",
       y = "crash rate",
       subtitle = "driver type") + theme_classic() +
  theme(plot.subtitle = element_text(size=12, hjust=0.5,
                                     face="bold", color="black"))

p_fig = ggpubr::ggarrange(p_age1, p_age2,
                          p_speed1, p_speed2,
                          p_male, p_bus, p_type,
                          ncol = 2, nrow = 4)
p_fig
ggsave("figs/other_covariate.pdf", p_fig, width = 6, height = 6)
ggsave("figs/other_covariate.jpeg", p_fig, width = 6, height = 6, dpi = 300)
ggsave("figs/other_covariate.bmp", p_fig, width = 6, height = 6, dpi = 300)
ggsave("figs/other_covariate.png", p_fig, width = 6, height = 6, dpi = 300)

