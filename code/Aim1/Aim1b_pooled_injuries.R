pacman::p_load(MASS, data.table, magrittr, rstanarm, dplyr, ggplot2, gridExtra, brms)
d = fread("data/Aim1_crash_SCE.csv") %>% 
  .[,`:=`(sce_CM = sce_CM*10000/distance,
          sce_HB = sce_HB*10000/distance,
          sce_HW = sce_HW*10000/distance,
          sce_RS = sce_RS*10000/distance,
          sce_N = sce_N*10000/distance)]

# kick out outliers
d1 = d %>% 
  filter(!(driver %in% 
    c("abad", "beaj10", "beni0", "bonr7", "booa0", "brad6", "bror7", 
      "busk2", "carr5", "clec25", "cunj19", "cure", "dund52", "gar15r", 
      "grit21", "gros", "guar", "gulc1", "gurj", "hal166", "harj2", 
      "higt12", "host", "johj48", "johw9", "jons86", "kena", "kesk", 
      "kisi", "kopc", "lamr27", "lasr2", "leeb1", "lege3", "manr4", 
      "mezj0", "mitm3", "monr83", "ouza1", "pert22", "phij2", "rivl13",
      "ruba0", "ruic4", "salm37", "sanm13", "shar3", "sopp2", "thog50",
      "thog76", "thom12", "thori1", "upsj")))

injury_all = brm(injuries ~ sce_N + age + ping_speed + gender + 
                   bus_unit + d_type + offset(log(distance/10000)),
                 family=negbinomial(), data=d1,
                 prior=set_prior("normal(0,5)"), 
                 iter = 4000, chains = 1, cores = 1, seed = 123)

injury_sub = brm(injuries ~ sce_HW + sce_HB + sce_RS + sce_CM + 
                   age + ping_speed + gender + 
                   bus_unit + d_type + offset(log(distance/10000)),
                 family=negbinomial(), data=d1,
                 prior=set_prior("normal(0,5)"), 
                 iter = 4000, chains = 1, cores = 1, seed = 123)

fatal_all = brm(fatalities ~ sce_N + 
                 age + ping_speed + gender + 
                 bus_unit + d_type + offset(log(distance/10000)),
               family=negbinomial(), data=d1,
               prior=set_prior("normal(0,5)"), 
               iter = 4000, chains = 1, cores = 1, seed = 123)
fatal_sub = brm(fatalities ~ sce_HW + sce_HB + sce_RS + sce_CM + 
                  age + ping_speed + gender + 
                  bus_unit + d_type + offset(log(distance/10000)),
                family=negbinomial(), data=d1,
                prior=set_prior("normal(0,5)"), 
                iter = 4000, chains = 1, cores = 1, seed = 123)

loo_injury_all = loo(injury_all)
loo_injury_sub = loo(injury_sub)
loo_fatal_all = loo(fatal_all)
loo_fatal_all = loo(fatal_sub)

saveRDS(list(injury_all, injury_sub, fatal_all, fatal_sub,
             loo_injury_all, loo_injury_sub, loo_fatal_all, loo_fatal_all),
        file = "fit/Aim1/injury_fatality_fit.rds")





































































