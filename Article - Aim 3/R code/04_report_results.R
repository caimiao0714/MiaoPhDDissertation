pacman::p_load(dplyr, data.table, ggplot2, patchwork, 
               lubridate, rstan, broom, latex2exp, hrbrthemes)

fit_NHPP = readRDS('Fit/fit_NHPP_all_drivers.rds')
fit_JPLP = readRDS('Fit/fit_JPLP_all_drivers.rds')

est_NHPP = broom::tidy(fit_NHPP, conf.int = T, rhat = T, ess = T)
est_JPLP = broom::tidy(fit_JPLP, conf.int = T, rhat = T, ess = T)

# ********************************************************
# *******************    Histogram    ********************
comb_r0 = rbind(est_NHPP %>% 
  filter(grepl('R0_true', term)) %>% 
  mutate(type = 'PLP'),
  est_JPLP %>% 
    filter(grepl('R0_true', term)) %>% 
    mutate(type = 'JPLP')) %>% 
  mutate(type = factor(type, levels = c('PLP', 'JPLP')))

mu0_NHPP = filter(est_NHPP, term == 'mu0_true') %>% pull(estimate) %>% round(3)
mu0_JPLP = filter(est_JPLP, term == 'mu0_true') %>% pull(estimate) %>% round(3)

p = comb_r0 %>% 
  ggplot(aes(x=estimate, fill=type)) +
  geom_histogram( color="white", alpha=0.6, position = 'identity', bins = 45) +
  scale_fill_manual(values=c("#404080", "#69b3a2")) +
  # geom_vline(xintercept = mu0_NHPP, color = "#404080", size = 1) + 
  # annotate("text", x=mu0_NHPP, y=36, color = "#404080", label=mu0_NHPP, size = 5) +
  # geom_vline(xintercept = mu0_JPLP, color = "#69b3a2", size = 1) + 
  # annotate("text", x=mu0_JPLP, y=36, color = "#69b3a2", label=mu0_JPLP, size = 5) +
  #theme_ipsum(grid = "Y", base_size = 20) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal(base_size = 18) +
  labs(fill="", x = TeX('Random intercepts $\\gamma_{0d}$'), y = '') + 
  theme(legend.position = c(0.12, 0.8),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave('Figures/Aim3_histogram_mu0.png', p, width = 10, height = 6.18, dpi = 600)


# ********************************************************
# *******************   Trace plot    ********************
t11 = stan_trace(fit_NHPP, pars = 'mu0') + ylab(TeX('$\\mu_{0}$'))
t21 = stan_trace(fit_NHPP, pars = 'sigma0') + ylab(TeX('$\\sigma_{0}$'))
t31 = stan_trace(fit_NHPP, pars = 'beta') + ylab(TeX('$\\beta$'))
t41 = plot_spacer()

t12 = stan_trace(fit_JPLP, pars = 'mu0') + ylab(TeX('$\\mu_{0}$'))
t22 = stan_trace(fit_JPLP, pars = 'sigma0') + ylab(TeX('$\\sigma_{0}$'))
t32 = stan_trace(fit_JPLP, pars = 'beta') + ylab(TeX('$\\beta$'))
t42 = stan_trace(fit_JPLP, pars = 'kappa') + ylab(TeX('$\\kappa$'))

trace_all = (t11|t12)/(t21|t22)/(t31|t32)/(t41|t42) + 
  plot_layout(ncol = 1, nrow = 4, guides = "collect") & 
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_text(vjust = 1, angle = 0))
ggsave('Figures/Aim3_trace_plot.png', trace_all, width = 10, height = 6.18, dpi = 600)

# ********************************************************
# ***********    Regression coefficients    **************
r3 = function(x) format(round(x, 3), nsmall = 3)
tab_NHPP = est_NHPP %>% 
  filter(grepl('mu0_true|sigma0|beta|kappa|R1_K', term)) %>% 
  mutate(`95% CI` = paste0('(', r3(conf.low), ', ', r3(conf.high), ')')) %>% 
  select(term, estimate, `95% CI`, rhat, ess)
tab_JPLP = est_JPLP %>% 
  filter(grepl('mu0_true|sigma0|beta|kappa|R1_K', term)) %>% 
  mutate(`95% CI` = paste0('(', r3(conf.low), ', ', r3(conf.high), ')')) %>% 
  select(term, estimate, `95% CI`, rhat, ess)
tab_est = full_join(tab_NHPP, tab_JPLP, by = 'term')

tab_est %>% 
  arrange(match(term, c('mu0_true', 'sigma0', 'beta', 'kappa'))) %>% 
  mutate(term = case_when(
    term == 'R1_K[1]' ~ 'Age',
    term == 'R1_K[2]' ~ 'Race: black',
    term == 'R1_K[3]' ~ 'Race: other',
    term == 'R1_K[4]' ~ 'Gender: female',
    term == 'R1_K[5]' ~ 'Mean speed',
    term == 'R1_K[6]' ~ 'Speed variation',
    term == 'R1_K[7]' ~ 'Precipitation intensity',
    term == 'R1_K[8]' ~ 'Precipitation probability',
    term == 'R1_K[9]' ~ 'Wind speed',
    term == 'mu0_true' ~ 'mu0',
    TRUE ~ term
  )) %>% 
  knitr::kable(format = 'latex', digits = 3, booktabs = T,
               caption = 'Parameter estimates, Rhat, and effective sample size (ESS) for PLP and JPLP on 496 truck drivers',
               label = 'Aim3realdataestimate',
               align = 'lcccrcccr', linesep = "",
               format.args = list(big.mark = ","))
