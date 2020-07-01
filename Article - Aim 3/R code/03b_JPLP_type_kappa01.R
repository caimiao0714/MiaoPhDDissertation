pacman::p_load(dplyr, data.table, ggplot2, fst, lubridate, rstan)

# *********  Run Stan with some simulated data  **********
source('Functions/JPLP_functions.R')
sim_df = sim_hier_JPLP(D = 10, beta = 1.2)
fit0 = stan("Stan/jplp_hierarchical_kappa01.stan",
            chains = 1, iter = 1000, data = sim_df$stan_dt, refresh = 1)
broom::tidy(fit0, conf.int = T, rhat = T, ess = T)

# ******************   Read in data    *******************
djplp = as.data.table(read_fst('Data/aim3_data/djplp.fst'))
sce = read_fst('Data/aim3_data/sce.fst') %>% 
  dplyr::select(driver_id, shift_id_num, trip_id_num, t_trip_start,
                t_trip_end, T2SCE_trip, event_type) %>% 
  mutate(T2SCE = t_trip_start + T2SCE_trip) %>% 
  mutate(T2SCE = fifelse(T2SCE == 0, 0.1, T2SCE)) %>% 
  as.data.table()



# ********************************************************
# ******************   Hard breaks    *******************
dt_JPLP = list(
  N = sce[event_type == 'HB',.N], # need to alter for different SCEs
  K = 9,
  S = djplp[,.N],
  D = djplp[,.N,driver_id][,.N],
  id = djplp[,driver_id],
  r_trip = djplp[,trip_id_num],
  t_trip_start = djplp[,t_trip_start/60],
  t_trip_end = djplp[,t_trip_end/60],
  event_time = sce[event_type == 'HB',T2SCE/60], # need to alter for different SCEs
  group_size = djplp[,N_HB], # need to alter for different SCEs
  X_predictors = djplp[,.(age, Black, Other_Race, Female, speed_mean, 
                          speed_sd, prep_inten,  prep_prob, wind_speed)]
)

start_time = Sys.time()
fit_JPLP = stan("Stan/jplp_hierarchical_kappa01.stan", data = dt_JPLP, seed = 123,
                chains = 4, cores = 4, iter = 5000, 
                warmup = 1000, refresh = 1)
(Time_diff = Sys.time() - start_time)
broom::tidy(fit_JPLP, conf.int = T, rhat = T, ess = T)
saveRDS(fit_JPLP, 'Fit/fit_JPLP_496_HB_kappa01.rds')




# ********************************************************
# ********************   Headways    *********************
dt_JPLP = list(
  N = sce[event_type == 'HW',.N], # need to alter for different SCEs
  K = 9,
  S = djplp[,.N],
  D = djplp[,.N,driver_id][,.N],
  id = djplp[,driver_id],
  r_trip = djplp[,trip_id_num],
  t_trip_start = djplp[,t_trip_start/60],
  t_trip_end = djplp[,t_trip_end/60],
  event_time = sce[event_type == 'HW',T2SCE/60], # need to alter for different SCEs
  group_size = djplp[,N_HW], # need to alter for different SCEs
  X_predictors = djplp[,.(age, Black, Other_Race, Female, speed_mean, 
                          speed_sd, prep_inten,  prep_prob, wind_speed)]
)

start_time = Sys.time()
fit_JPLP = stan("Stan/jplp_hierarchical_kappa01.stan", data = dt_JPLP, seed = 123,
                chains = 4, cores = 4, iter = 5000, 
                warmup = 1000, refresh = 1)
(Time_diff = Sys.time() - start_time)
broom::tidy(fit_JPLP, conf.int = T, rhat = T, ess = T)
saveRDS(fit_JPLP, 'Fit/fit_JPLP_496_HW_kappa01.rds')



# ********************************************************
# **************   Collision mitigation    ***************
dt_JPLP = list(
  N = sce[event_type == 'CM',.N], # need to alter for different SCEs
  K = 9,
  S = djplp[,.N],
  D = djplp[,.N,driver_id][,.N],
  id = djplp[,driver_id],
  r_trip = djplp[,trip_id_num],
  t_trip_start = djplp[,t_trip_start/60],
  t_trip_end = djplp[,t_trip_end/60],
  event_time = sce[event_type == 'CM',T2SCE/60], # need to alter for different SCEs
  group_size = djplp[,N_CM], # need to alter for different SCEs
  X_predictors = djplp[,.(age, Black, Other_Race, Female, speed_mean, 
                          speed_sd, prep_inten,  prep_prob, wind_speed)]
)

start_time = Sys.time()
fit_JPLP = stan("Stan/jplp_hierarchical_kappa01.stan", data = dt_JPLP, seed = 123,
                chains = 4, cores = 4, iter = 5000, 
                warmup = 1000, refresh = 1)
(Time_diff = Sys.time() - start_time)
broom::tidy(fit_JPLP, conf.int = T, rhat = T, ess = T)
saveRDS(fit_JPLP, 'Fit/fit_JPLP_496_CM_kappa01.rds')





# *********************************************************
# ******   Collision mitigation/Rolling stability    ******
dt_JPLP = list(
  N = sce[event_type %in% c('CM', 'RS'),.N], # need to alter for different SCEs
  K = 9,
  S = djplp[,.N],
  D = djplp[,.N,driver_id][,.N],
  id = djplp[,driver_id],
  r_trip = djplp[,trip_id_num],
  t_trip_start = djplp[,t_trip_start/60],
  t_trip_end = djplp[,t_trip_end/60],
  event_time = sce[event_type %in% c('CM', 'RS'),T2SCE/60], # alter for different SCEs
  group_size = djplp[,N_RS_CM], # need to alter for different SCEs
  X_predictors = djplp[,.(age, Black, Other_Race, Female, speed_mean, 
                          speed_sd, prep_inten,  prep_prob, wind_speed)]
)

start_time = Sys.time()
fit_JPLP = stan("Stan/jplp_hierarchical_kappa01.stan", data = dt_JPLP, seed = 123,
                chains = 4, cores = 4, iter = 5000, 
                warmup = 1000, refresh = 1)
(Time_diff = Sys.time() - start_time)
broom::tidy(fit_JPLP, conf.int = T, rhat = T, ess = T)
saveRDS(fit_JPLP, 'Fit/fit_JPLP_496_CM_RS_kappa01.rds')









