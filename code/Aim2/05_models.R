pacman::p_load(data.table, dplyr, ggplot2, fst)
d = fst::read_fst("data/cleaned/31interval30_CE.fst") %>% as.data.table()

z = d %>% 
  .[,cumdrive := cumdrive/60] %>% 
  .[,maxdrive := max(cumdrive), .(driver, shift_id)] %>% 
  .[maxdrive <= 11] %>% 
  .[,`:=`(maxdrive = NULL, CE_binary = fifelse(nCE > 0, 1, 0),
          race = factor(race, levels = c("White", "Black", "Other")))]

f_logit = glm(CE_binary ~ cumdrive + speed_mean + speed_sd + age + race + 
                prep_inten + prep_prob + wind_speed + visibility + interval_time, 
              family = "binomial", data = z)
summary(f_logit)
saveRDS(f_logit, "fit/f_logit.rds")

f_poisson = glm(nCE ~ cumdrive + speed_mean + speed_sd + age + race + 
                prep_inten + prep_prob + wind_speed + visibility, 
                offset = log(interval_time),
                family = "poisson", data = z)
summary(f_poisson)
saveRDS(f_poisson, "fit/f_poisson.rds")



stargazer::stargazer(f_logit, f_poisson, 
                     title = "Logistic regression and Poisson regression models", 
                     header = FALSE, align = T, 
                     dep.var.labels = c("At least one SCE occurred or not", 
                                        "The number of SCEs in the interval"))






# Bayesian inference with INLA
pacman::p_load(INLA)

zz = z[1:100000,] %>% 
  .[,driver1 := paste(driver, 1, sep = "_")]
fit  = INLA::inla(nCE ~  cumdrive + 
                    f(driver, model = 'iid') + 
                    f(driver1, cumdrive, model = 'iid'), 
                  E = interval_time,
                  family = "nbinomial", data = zz)


# check original ping data
p = fst::read_fst("data/cleaned/11ping_add_trip_shift_id.fst") %>% as.data.table()
d %>% 
  .[,.(maxdrive = max(cumdrive)), .(driver, shift_id)] %>% 
  .[between(maxdrive, 11*60, 1100*60),] %>% 
  .[order(driver, shift_id)] %>% 
  .[,.(driver, shift_id, maxdrive = round(maxdrive/60, 2))] %>% 
  .[1:20]

test_ping = p %>% 
  .[driver == "acot" & between(shift_id, 5, 33),] %>% 
  .[,.(driver, ping_time, speed, trip_id, shift_id)]
write_fst(test_ping, "data/test_ping.fst")

