pacman::p_load(MASS, data.table, magrittr, rstanarm, dplyr)
d = fread("data/Aim1_crash_SCE.csv") %>% 
  .[,`:=`(sce_CM = sce_CM*10000/distance,
          sce_HB = sce_HB*10000/distance,
          sce_HW = sce_HW*10000/distance,
          sce_RS = sce_RS*10000/distance,
          sce_N = sce_N*10000/distance)] %>% 
  .[!(driver %in% 
        c("abad", "beaj10", "beni0", "bonr7", "booa0", "brad6", "bror7", 
            "busk2", "carr5", "clec25", "cunj19", "cure", "dund52", "gar15r", 
            "grit21", "gros", "guar", "gulc1", "gurj", "hal166", "harj2", 
            "higt12", "host", "johj48", "johw9", "jons86", "kena", "kesk", 
            "kisi", "kopc", "lamr27", "lasr2", "leeb1", "lege3", "manr4", 
            "mezj0", "mitm3", "monr83", "ouza1", "pert22", "phij2", "rivl13", 
            "ruba0", "ruic4", "salm37", "sanm13", "shar3", "sopp2", "thog50", 
            "thog76", "thom12", "thori1", "upsj")),]

dmodel = function(data = d, business_unit, driver_type, n_iter = 2000,
                  n_chains = 4, n_cores = 4){
  dz = data %>% 
    filter(bus_unit == business_unit, d_type == driver_type)
  
  f_all <- stan_glm(n_crash ~ sce_N + age + ping_speed + gender,
                      offset = log(distance/1000),
                      data = dz, family = neg_binomial_2, 
                      prior = normal(0,2.5), 
                      prior_intercept = normal(0,5),
                      iter = n_iter, chains = n_chains, cores = n_cores, seed = 123)
  f_sub <- stan_glm(n_crash ~ sce_HW + sce_HB + sce_RS + sce_CM + 
                          age + ping_speed + gender,
                        offset = log(distance/1000),
                        data = dz, family = neg_binomial_2, 
                        prior = normal(0,2.5), 
                        prior_intercept = normal(0,5),
                        iter = n_iter, chains = n_chains, cores = n_cores, seed = 123)
  f_HW <- stan_glm(n_crash ~ sce_HW + age + ping_speed + gender,
                       offset = log(distance/1000),
                       data = dz, family = neg_binomial_2, 
                       prior = normal(0,2.5), 
                       prior_intercept = normal(0,5),
                       iter = n_iter, chains = n_chains, cores = n_cores, seed = 123)
  f_HB <- stan_glm(n_crash ~ sce_HB + age + ping_speed + gender,
                       offset = log(distance/1000),
                       data = dz, family = neg_binomial_2, 
                       prior = normal(0,2.5), 
                       prior_intercept = normal(0,5),
                       iter = n_iter, chains = n_chains, cores = n_cores, seed = 123)
  f_RS <- stan_glm(n_crash ~ sce_RS + age + ping_speed + gender,
                       offset = log(distance/1000),
                       data = dz, family = neg_binomial_2, 
                       prior = normal(0,2.5), 
                       prior_intercept = normal(0,5),
                       iter = n_iter, chains = n_chains, cores = n_cores, seed = 123)
  f_CM <- stan_glm(n_crash ~ sce_CM + age + ping_speed + gender,
                       offset = log(distance/1000),
                       data = dz, family = neg_binomial_2, 
                       prior = normal(0,2.5), 
                       prior_intercept = normal(0,5),
                       iter = n_iter, chains = n_chains, cores = n_cores, seed = 123)
  loo_all = loo(f_all); loo_sub = loo(f_sub); loo_HW = loo(f_HW)
  loo_HB = loo(f_HB); loo_RS = loo(f_RS); loo_CM = loo(f_CM)
  
  saveRDS(list(f_all, f_sub, f_HW, f_HB, f_RS, f_CM, 
               loo_all, loo_sub, loo_HW, loo_HB, loo_RS, loo_CM),
          paste0("fit/Aim1/", business_unit, driver_type, '.rds'))
}

type_list = d[,.N,.(bus_unit, d_type)] %>% 
  .[order(bus_unit, d_type)] %>% 
  .[N>=500,]

# Run Bayesian NB models
for (i in 1:nrow(type_list)) {
      print(paste0("i = ", i))
      dmodel(business_unit = type_list$bus_unit[i], 
             driver_type = type_list$d_type[i], 
             n_iter = 4000, n_chains = 1)
}


prop_zero <- function(y) mean(y == 0)
ppc =function(fit = z1, index = 1){
  yrep <- posterior_predict(fit[[index]])
  return(pp_check(fit[[index]], plotfun = "stat", stat = "prop_zero"))
}








#### CHECK OUTLIERS - START
pacman::p_load(rstanarm, loo, broom, dplyr)
files_path = list.files("fit/Aim1", full.names = TRUE)
files_name = list.files("fit/Aim1")
black_list = list()

for (ind in 1:length(files_name)) {
  z = readRDS(files_path[ind])
  k_id = list()
  for (i in 7:12) {
    k_id[[i]] = pareto_k_ids(z[[i]], threshold = 0.5)
  }
  sort(unique(unlist(k_id)))
  
  b = d %>% 
    .[bus_unit == substr(files_name[ind], 1, 5) & 
        d_type == substr(files_name[ind], 6, 8),] %>% 
    .[sort(unique(unlist(k_id))), driver]
  
  black_list[[ind]] <- b
}
black_list[[length(files_name) + 1]] = 
  c("salm37", "thori1", "sanm13", "gros", "manr4", 
  "kisi","johw9", "monr83", "rivl13", "kena", "lasr2", "kopc")

dput(unique(unlist(black_list)))
### CHECK OUTLIERS - END

z = readRDS(files_path[8])
for (i in 7:12) {
  print(pareto_k_table(z[[i]]))
}



# get posteror estimates 
tidy(z1[[1]])

# return psis-loo statistics
loo1 = loo(z[[1]], save_psis = TRUE, cores = 4, k_threshold = 0.7)
loo2 = loo(z[[2]])#, save_psis = TRUE, cores = 4, k_threshold = 0.7
loo3 = loo(z1[[3]], save_psis = TRUE, cores = 4)
loo4 = loo(z1[[4]], save_psis = TRUE, cores = 4)
loo5 = loo(z1[[5]], save_psis = TRUE, cores = 4)
loo6 = loo(z1[[6]], save_psis = TRUE, cores = 4)
compare_models(loo1, loo2)

# find outliers
loo1 = loo(z[[2]])#save_psis = TRUE, 
plot(loo1, label_points = TRUE)
plot(loo2, label_points = TRUE)



# PPC:
# posterior predictive check

ppc(z, 2)




# LOO-PIT: 
# leave-one-out cross-validation marginal posterior predictive checks
# NOT useful for count variable with few categories.
ovl = function(fit = z1, index = 1, loofit = loo1){
  loo1 = loo(fit[[index]], save_psis = TRUE)
  return(bayesplot::ppc_loo_pit_overlay(
    y = d[bus_unit == "JBI00" & d_type == "REG",n_crash],
    yrep = posterior_predict(fit[[index]]),
    lw = weights(loofit$psis_object)
  ))
}
ovl(z, 2)











































































































