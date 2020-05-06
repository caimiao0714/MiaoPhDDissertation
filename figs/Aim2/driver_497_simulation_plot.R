pacman::p_load(broom, dplyr, ggplot2, ggthemes, tidyr, cowplot)
extrafont::fonts()
windowsFonts(LMRoman = windowsFont("LM Roman 10")) # For windows computer
default_font_family = "LMRoman"

# ------- Hierarchical Logit model -------
lme_logit = readRDS("fit/logit_lme4.rds")
n_rep = 500
ndriver = 497

sim_nova = lme_logit %>%
  tidy(effects = "fixed") %>%
  select(term, estimate) %>%
  filter(term %in% c("(Intercept)", "cumdrive")) %>%
  mutate(term = case_when(term == "(Intercept)" ~ "intercept",
                          TRUE ~ term)) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  slice(rep(1:n(), each = n_rep)) %>%
  mutate(x_cumdrive = seq(0, 11, by = 11/(n_rep - 1))) %>%
  mutate(p_SCE = boot::inv.logit(intercept + cumdrive*x_cumdrive))


sim_vary = coef(lme_logit)$driver %>%
  select(intercept = `(Intercept)`, cumdrive) %>%
  tibble::rownames_to_column("driver") %>%
  tibble::as_tibble() %>%
  #filter(!(driver %in% c("kisi", "grad01"))) %>%
  slice(rep(1:n(), each = n_rep)) %>%
  mutate(x_cumdrive = rep(seq(0, 11, by = 11/(n_rep - 1)), ndriver)) %>%
  mutate(p_SCE = boot::inv.logit(intercept + cumdrive*x_cumdrive))

p = ggplot() +
  geom_line(data = sim_vary,
            aes(x = x_cumdrive, y = p_SCE, group = driver),
            color = "grey", alpha = 0.3) +
  geom_line(data = sim_nova,
            aes(x = x_cumdrive, y = p_SCE),
            size = 1, color = "blue") +
  labs(x = "Cumulative driving time (hours)",
       y = "Estimated probability of having SCEs") +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 11),
                     expand = c(0, 0)) +
  scale_y_continuous(trans = "log10") +
  theme_test() +
  theme(text = element_text(family = default_font_family),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 20))

ggsave("figures/driver_497_Freq_log10_logit.png", p, width = 10, height = 6.18, dpi = 600)


# ------- Hierarchical Negative Binomial model -------
lme_nb = readRDS("fit/nb_lme4_497.rds")
n_rep = 500
ndriver = 497

sim_nova = lme_nb %>%
  tidy(effects = "fixed") %>%
  select(term, estimate) %>%
  filter(term %in% c("(Intercept)", "cumdrive")) %>%
  mutate(term = case_when(term == "(Intercept)" ~ "intercept",
                          TRUE ~ term)) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  slice(rep(1:n(), each = n_rep)) %>%
  mutate(x_cumdrive = seq(0, 11, by = 11/(n_rep - 1))) %>%
  mutate(p_SCE = 30*exp(intercept + cumdrive*x_cumdrive))


sim_vary = coef(lme_nb)$driver %>%
  select(intercept = `(Intercept)`, cumdrive) %>%
  tibble::rownames_to_column("driver") %>%
  tibble::as_tibble() %>%
  #filter(!(driver %in% c("kisi", "grad01"))) %>%
  slice(rep(1:n(), each = n_rep)) %>%
  mutate(x_cumdrive = rep(seq(0, 11, by = 11/(n_rep - 1)), ndriver)) %>%
  mutate(p_SCE = 30*exp(intercept + cumdrive*x_cumdrive))

p_nb = ggplot() +
  geom_line(data = sim_vary,
            aes(x = x_cumdrive, y = p_SCE, group = driver),
            color = "grey", alpha = 0.3) +
  geom_line(data = sim_nova,
            aes(x = x_cumdrive, y = p_SCE),
            size = 1, color = "blue") +
  labs(x = "Cumulative driving time (hours)",
       y = "Estimated number of SCEs per hour") +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 11),
                     expand = c(0, 0)) +
  scale_y_continuous(trans = "log10") +
  theme_test() +
  theme(text = element_text(family = default_font_family),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 20))

ggsave("figures/driver_497_Freq_log10_nb.png", p_nb, width = 10, height = 6.18, dpi = 600)

