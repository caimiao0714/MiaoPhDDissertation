pacman::p_load(dplyr, data.table, ggplot2, fst, lubridate, rstan)

# ********************************************************
# *********  Run Stan with some simulated data  **********
source('Functions/NHPP_functions.R')
df = sim_hier_nhpp(D = 5, beta = 1.2)
fit0 = stan("Stan/nhpp_plp_hierarchical.stan",
            chains = 1, iter = 1000, data = df$hier_dat, refresh = 1)
broom::tidy(fit0)

# ********************************************************
# ******************   Read in data    *******************
dnhpp = as.data.table(read_fst('Data/dnhpp.fst'))
djplp = as.data.table(read_fst('Data/djplp.fst'))
sce = read_fst('Data/sce.fst') %>% 
  dplyr::select(driver_id, shift_id_num, trip_id_num, t_trip_start, 
                t_trip_end, T2SCE_trip, event_type) %>% 
  left_join(djplp[,.(driver_id, shift_id_num, trip_id_num, tau)],
            by = c('driver_id', 'shift_id_num', 'trip_id_num')) %>% 
  mutate(T2SCE = T2SCE_trip + t_trip_start) %>% 
  mutate(T2SCE = fifelse(T2SCE == 0, 0.1, T2SCE)) %>% 
  as.data.table()

# ********************************************************
# ************  Create a list data for stan  *************
dt_nhpp = list(
  N = sce[,.N],
  K = 9,
  S = dnhpp[,.N],
  D = dnhpp[,.N,driver_id][,.N],
  id = dnhpp[,driver_id],
  tau = dnhpp[,tau/60],
  event_time = sce[,T2SCE/60], 
  group_size = dnhpp[,N_SCE],
  X_predictors = dnhpp[,.(age, Black, Other_Race, Female, speed_mean, 
                          speed_sd, prep_inten,  prep_prob, wind_speed)]
)

# ********************************************************
# *********  Run Stan with real data  **********
start_time = Sys.time()
fit_NHPP = stan("Stan/nhpp_plp_hierarchical.stan", data = dt_nhpp, seed = 123, 
                chains = 4, cores = 4, iter = 5000, warmup = 1000, refresh = 1)
(Time_diff = Sys.time() - start_time)
broom::tidy(fit_NHPP)
saveRDS(fit_NHPP, 'Fit/fit_NHPP.rds')


