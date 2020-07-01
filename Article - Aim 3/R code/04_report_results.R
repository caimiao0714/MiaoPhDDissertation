pacman::p_load(dplyr, data.table, ggplot2, patchwork, kableExtra,
               lubridate, rstan, broom, latex2exp, ggthemes)
options(knitr.kable.NA = '')

# **********************************************************
# **************   Part 1: NHPP and JPLP   *****************
fit_NHPP = readRDS('Fit/fit_NHPP_496.rds')
fit_JPLP = readRDS('Fit/fit_JPLP_496_kappa02.rds')
est_NHPP = broom::tidy(fit_NHPP, conf.int = T, rhat = T, ess = T)
est_JPLP = broom::tidy(fit_JPLP, conf.int = T, rhat = T, ess = T)

# *********   1.1: Parameter estimate results    ***********
r3 = function(x) format(round(x, 3), nsmall = 3)
getfit0 = function(fit){
  fit1 = fit %>% 
    filter(grepl('mu0_true|sigma0|beta|kappa|R1_K', term)) %>% 
    mutate(CI = paste0(' (', r3(conf.low), ', ', r3(conf.high), ')')) %>% 
    select(term, estimate, CI, rhat, ess) #, rhat, ess
  return(fit1)
}

# Export \LaTeX code
full_join(getfit0(est_NHPP), getfit0(est_JPLP), by = 'term') %>% 
  arrange(match(term, c('beta', 'kappa', 'mu0_true', 'sigma0'))) %>% 
  mutate(term = case_when(
    term == 'beta' ~ 'beta',
    term == 'kappa' ~ 'kappa',
    term == 'mu0_true' ~ 'mu0',
    term == 'sigma0' ~ 'sigma0',
    term == 'R1_K[1]' ~ 'Age',
    term == 'R1_K[2]' ~ 'Race: black',
    term == 'R1_K[3]' ~ 'Race: other',
    term == 'R1_K[4]' ~ 'Gender: female',
    term == 'R1_K[5]' ~ 'Mean speed',
    term == 'R1_K[6]' ~ 'Speed variation',
    term == 'R1_K[7]' ~ 'Preci. intensity',
    term == 'R1_K[8]' ~ 'Preci. prob.',
    term == 'R1_K[9]' ~ 'Wind speed',
    term == 'mu0_true' ~ 'mu0',
    TRUE ~ term
  )) %>% 
  `colnames<-`(c('Parameters', 'PLP', '95% CI', 'Rhat', 'ESS', 'JPLP', '95% CI', 'Rhat', 'ESS')) %>% 
  knitr::kable(format = 'latex', digits = 3, booktabs = T,
               caption = 'Parameter estimates, Rhat, and effective sample size (ESS) for PLP and JPLP on 496 truck drivers',
               label = 'Aim3realdataestimate',
              linesep = "",
               format.args = list(big.mark = ",")) 



# *****************    1.2: Histogram    *******************
comb_r0 = rbind(est_NHPP %>% 
                  filter(grepl('R0_true', term)) %>% 
                  mutate(type = 'PLP'),
                est_JPLP %>% 
                  filter(grepl('R0_true', term)) %>% 
                  mutate(type = 'JPLP')) %>% 
  mutate(type = factor(type, levels = c('PLP', 'JPLP')))

mu0_NHPP = filter(est_NHPP, term == 'mu0_true') %>% pull(estimate) %>% round(3)
mu0_JPLP = filter(est_JPLP, term == 'mu0_true') %>% pull(estimate) %>% round(3)

(p = comb_r0 %>% 
  ggplot(aes(x=estimate, fill=type)) +
  geom_histogram( color="white", alpha=0.6, position = 'identity', bins = 45) +
  #scale_fill_manual(values=c("#F2300F", "#0B775E")) +
    scale_fill_grey() +
  scale_y_continuous(expand = c(0, 0.1)) +
  labs(x = TeX('Random intercepts $\\gamma_{0d}$'), y = '') + 
  theme_grey(base_size = 20) +
  theme(legend.position = c(0.32, 0.72),
        legend.title = element_blank(),
        panel.background = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray90", size = 0.5), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()))
ggsave('Figures/Aim3_histogram_mu0_grey.jpeg', p, width = 10, height = 6.18, dpi = 1200)
ggsave('Figures/Aim3_histogram_mu0_grey.pdf', p, width = 10, height = 6.18)

(p = comb_r0 %>% 
    ggplot(aes(x=estimate, fill=type)) +
    geom_histogram( color="white", alpha=0.6, position = 'identity', bins = 45) +
    scale_fill_manual(values=c("#F2300F", "#0B775E")) +
    scale_y_continuous(expand = c(0, 0.1)) +
    labs(x = TeX('Random intercepts $\\gamma_{0d}$'), y = '') + 
    theme_grey(base_size = 20) +
    theme(legend.position = c(0.32, 0.72),
          legend.title = element_blank(),
          panel.background = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "gray90", size = 0.5), 
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()))
ggsave('Figures/Aim3_histogram_mu0_color.jpeg', p, width = 10, height = 6.18, dpi = 1200)
ggsave('Figures/Aim3_histogram_mu0_color.pdf', p, width = 10, height = 6.18)



# ******************   1.3: Trace plot    *******************
t11 = stan_trace(fit_NHPP, pars = 'mu0') + ylab(TeX('$\\mu_{0}$')) + ggtitle('PLP')
t21 = stan_trace(fit_NHPP, pars = 'sigma0') + ylab(TeX('$\\sigma_{0}$'))
t31 = stan_trace(fit_NHPP, pars = 'beta') + ylab(TeX('$\\beta$'))
t41 = plot_spacer()

t12 = stan_trace(fit_JPLP, pars = 'mu0') + ylab(TeX('$\\mu_{0}$')) + ggtitle('JPLP')
t22 = stan_trace(fit_JPLP, pars = 'sigma0') + ylab(TeX('$\\sigma_{0}$'))
t32 = stan_trace(fit_JPLP, pars = 'beta') + ylab(TeX('$\\beta$'))
t42 = stan_trace(fit_JPLP, pars = 'kappa') + ylab(TeX('$\\kappa$'))

trace_all = (t11|t12)/(t21|t22)/(t31|t32)/(t41|t42) + 
  plot_layout(ncol = 1, nrow = 4, guides = "collect") & 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_text(vjust = 1, angle = 0))
ggsave('Figures/Aim3_trace_plot.jpeg', trace_all, width = 10, height = 6.18, dpi = 1200)
ggsave('Figures/Aim3_trace_plot.eps', trace_all, width = 10, height = 6.18)
ggsave('Figures/Aim3_trace_plot.pdf', trace_all, width = 10, height = 6.18)



# **********************************************************
# *********   Part 2: Stratified by SCE types   ************
fit_HW = readRDS('Fit/fit_JPLP_496_HW_kappa02.rds')
fit_HB = readRDS('Fit/fit_JPLP_496_HB_kappa02.rds')
fit_CM = readRDS('Fit/fit_JPLP_496_CM_kappa02.rds')
fit_CMRS = readRDS('Fit/fit_JPLP_496_CM_RS_kappa02.rds')

est_HW = broom::tidy(fit_HW, conf.int = T, rhat = T, ess = T)
est_HB = broom::tidy(fit_HB, conf.int = T, rhat = T, ess = T)
est_CM = broom::tidy(fit_CM, conf.int = T, rhat = T, ess = T)
est_CMRS = broom::tidy(fit_CMRS, conf.int = T, rhat = T, ess = T)



r3 = function(x) format(round(x, 3), nsmall = 3)
getfit = function(fit){
  fit1 = fit %>% 
    filter(grepl('mu0_true|sigma0|beta|kappa|R1_K', term)) %>% 
    mutate(estimate = paste0( r3(estimate), ' (', r3(conf.low), ', ', r3(conf.high), ')')) %>% 
    select(term, estimate) #, rhat, ess
  return(fit1)
}

getfit(est_HW) %>% 
  left_join(getfit(est_HB), by = 'term') %>% 
  left_join(getfit(est_CM), by = 'term') %>% 
  #left_join(getfit(est_CMRS), by = 'term')  %>% 
  arrange(match(term, c('beta', 'kappa', 'mu0_true', 'sigma0'))) %>% 
  mutate(term = case_when(
    term == 'beta' ~ 'beta',
    term == 'kappa' ~ 'kappa',
    term == 'mu0_true' ~ 'mu0',
    term == 'sigma0' ~ 'sigma0',
    term == 'R1_K[1]' ~ 'Age',
    term == 'R1_K[2]' ~ 'Race: black',
    term == 'R1_K[3]' ~ 'Race: other',
    term == 'R1_K[4]' ~ 'Gender: female',
    term == 'R1_K[5]' ~ 'Mean speed',
    term == 'R1_K[6]' ~ 'Speed variation',
    term == 'R1_K[7]' ~ 'Preci. intensity',
    term == 'R1_K[8]' ~ 'Preci. prob.',
    term == 'R1_K[9]' ~ 'Wind speed',
    term == 'mu0_true' ~ 'mu0',
    TRUE ~ term
  )) %>% 
  `colnames<-`(c('Parameters', 'Headway', 'Hard brake', 'Collision mitigation and Rolling stability')) %>% 
  knitr::kable(format = 'latex', booktabs = TRUE, digits = 3, linesep = "", align = 'c',
               caption = 'Parameter estimates, Rhat, and effective sample size (ESS) for JPLP on 496 truck drivers, stratified by different types of safety-critical events',
               label = 'Aim3realdataestimatestratified')
getfit(est_HW)
getfit(est_HB)
getfit(est_CM)
getfit(est_CMRS)


# Check the number of SCEs in each group
sce = fst::read_fst('Data/aim3_data/sce.fst') %>% 
  dplyr::select(driver_id, shift_id_num, trip_id_num, t_trip_start, 
                t_trip_end, T2SCE_trip, event_type) %>% 
  as.data.table()

sce[,.N]
sce[,.N,event_type]
sce[,round(.N*100/sce[,.N], 1),event_type]
















