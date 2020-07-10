pacman::p_load(dplyr, data.table, ggplot2, fst, lubridate, rstan)

# ********************************************************
# *********  Run Stan with some simulated data  **********
source('Functions/JPLP_functions.R')
sim_df = sim_hier_JPLP(D = 10, beta = 1.2)
fit0 = stan("Stan/jplp_hierarchical.stan",
            chains = 1, iter = 1000, data = sim_df$stan_dt, refresh = 1)
broom::tidy(fit0)

# ********************************************************
# ******************   Read in data    *******************
djplp = as.data.table(read_fst('Data/djplp.fst')) 
sce = read_fst('Data/sce.fst') %>% 
  dplyr::select(driver_id, shift_id_num, trip_id_num, t_trip_start,
                t_trip_end, T2SCE_trip, event_type) %>% 
  mutate(T2SCE = t_trip_start + T2SCE_trip) %>% 
  mutate(T2SCE = fifelse(T2SCE == 0, 0.1, T2SCE)) %>% 
  as.data.table()

# ********************************************************
# ************  Create a list data for stan  *************
dt_JPLP = list(
  N = sce[,.N],
  K = 9,
  S = djplp[,.N],
  D = djplp[,.N,driver_id][,.N],
  id = djplp[,driver_id],
  r_trip = djplp[,trip_id_num],
  t_trip_start = djplp[,t_trip_start/60],
  t_trip_end = djplp[,t_trip_end/60],
  event_time = sce[,T2SCE/60], 
  group_size = djplp[,N_SCE],
  X_predictors = djplp[,.(age, Black, Other_Race, Female, speed_mean, 
                          speed_sd, prep_inten,  prep_prob, wind_speed)]
)

# ********************************************************
# *********  Run Stan with real data  **********
nchain = 4
n_iter = 5000

start_time = Sys.time()
fit_JPLP = stan("Stan/jplp_hierarchical.stan", data = dt_JPLP, seed = 123,
                chains = nchain, cores = nchain, iter = n_iter, 
                warmup = 1000, refresh = 1)
(Time_diff = Sys.time() - start_time)
broom::tidy(fit_JPLP)
saveRDS(fit_JPLP, 'Fit/fit_JPLP.rds')