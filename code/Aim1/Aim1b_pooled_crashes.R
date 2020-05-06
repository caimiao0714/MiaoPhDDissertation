pacman::p_load(MASS, data.table, magrittr, rstanarm, dplyr, ggplot2, gridExtra)
d = fread("data/Aim1_crash_SCE.csv") %>% 
  .[,`:=`(sce_CM = sce_CM*10000/distance,
          sce_HB = sce_HB*10000/distance,
          sce_HW = sce_HW*10000/distance,
          sce_RS = sce_RS*10000/distance,
          sce_N = sce_N*10000/distance)]

# find out outliers
# z = readRDS("fit/Aim1/zzDCS00OTR.rds")
# loo2 = loo(z[[2]])
# plot(loo2, label_points = TRUE)

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


f_all <- stan_glm(n_crash ~ sce_N + age + ping_speed + gender + bus_unit + d_type,
                    offset = log(distance/1000),
                    data = d1, family = neg_binomial_2, 
                    prior = normal(0,10), 
                    prior_intercept = normal(0,10), QR = TRUE,
                    iter = 4000, chains = 1, cores = 1, seed = 123)
f_sub <- stan_glm(n_crash ~ sce_HW + sce_HB + sce_RS + sce_CM + 
                    age + ping_speed + gender + bus_unit + d_type,
                  offset = log(distance/1000),
                  data = d1, family = neg_binomial_2, 
                  prior = normal(0,10), 
                  prior_intercept = normal(0,10), QR = TRUE,
                  iter = 4000, chains = 1, cores = 1, seed = 123)
f_HW <- stan_glm(n_crash ~ sce_HW + age + ping_speed + gender + bus_unit + d_type,
                 offset = log(distance/1000),
                 data = d1, family = neg_binomial_2, 
                 prior = normal(0,10), 
                 prior_intercept = normal(0,10), QR = TRUE,
                 iter = 4000, chains = 1, cores = 1,  seed = 123)
f_HB <- stan_glm(n_crash ~ sce_HB + age + ping_speed + gender + bus_unit + d_type,
                 offset = log(distance/1000),
                 data = d1, family = neg_binomial_2, 
                 prior = normal(0,10), 
                 prior_intercept = normal(0,10), QR = TRUE,
                 iter = 4000, chains = 1, cores = 1,  seed = 123)
f_RS <- stan_glm(n_crash ~ sce_RS + age + ping_speed + gender + bus_unit + d_type,
                 offset = log(distance/1000),
                 data = d1, family = neg_binomial_2, 
                 prior = normal(0,10), 
                 prior_intercept = normal(0,10), QR = TRUE,
                 iter = 4000, chains = 1, cores = 1,  seed = 123)
f_CM <- stan_glm(n_crash ~ sce_CM + age + ping_speed + gender + bus_unit + d_type,
                 offset = log(distance/1000),
                 data = d1, family = neg_binomial_2, 
                 prior = normal(0,10), 
                 prior_intercept = normal(0,10), QR = TRUE,
                 iter = 4000, chains = 1, cores = 1,  seed = 123)

(loo_all = loo(f_all))
(loo_sub = loo(f_sub))
(loo_HW = loo(f_HW))
(loo_HB = loo(f_HB))
(loo_RS = loo(f_RS))
(loo_CM = loo(f_CM))

saveRDS(list(f_all, f_sub, f_HW, f_HB, f_RS, f_CM, 
             loo_all, loo_sub, loo_HW, loo_HB, loo_RS, loo_CM),
        "fit/Aim1/a0_pooled_fit.rds")


z = readRDS("fit/Aim1/a0_pooled_fit.rds")
model_names = c("Pooled model", "Four SCEs", "Headways", "Hard brakes",
                "Rolling stability", "Collision mitigation")
fig_list1 = list()
prop_zero <- function(x) mean(x == 0)
for (i in seq_along(model_names)) {
  fig_list1[[i]] = pp_check(z[[i]], plotfun = "stat", 
                            stat = "prop_zero") +
    labs(subtitle = model_names[i])
}
#p1 = grid.arrange(fig_list1[[1]], fig_list1[[2]], fig_list1[[3]],
#                  fig_list1[[4]], fig_list1[[5]], fig_list1[[6]],
#                  ncol = 2, nrow = 3,
#                  layout_matrix = rbind(1:2, 3:4, 5:6))
library(ggpubr)
p1 = ggarrange(fig_list1[[1]], fig_list1[[2]], fig_list1[[3]],
               fig_list1[[4]], fig_list1[[5]], fig_list1[[6]], 
               ncol=3, nrow=2, common.legend = TRUE, legend="bottom")
p1

ggsave("figs/loo_pooled.pdf", p1, width = 10, height = 6.18)


