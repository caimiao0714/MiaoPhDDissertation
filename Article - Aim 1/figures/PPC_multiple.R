pacman::p_load(data.table, magrittr, rstanarm, dplyr, ggplot2, purrr, cowplot, ggpubr, gridExtra, extrafont)

# https://medium.com/@fulowa/latex-font-in-a-ggplot-9120caaa5250
extrafont::font_import(pattern = "lmroman*", prompt = FALSE)
# y
default_font_family = "LM Roman 10"

prop_zero <- function(y) mean(y == 0)
ppc =function(fit, index = 1){
  yrep <- posterior_predict(fit[[index]])
  return(pp_check(fit[[index]], plotfun = "stat", stat = "prop_zero", bins = 30))
}

# Pooled analysis
z = readRDS("fit/a0_pooled_fit.rds")
model_names = c("Pooled model", "Four SCEs", "Headways", "Hard brakes",
                "Rolling stability", "Collision mitigation")
fig_list1 = list()
for (i in seq_along(model_names)) {
  fig_list1[[i]] = ppc(z, i) +
    labs(subtitle = model_names[i]) + xlim(c(0.5, 0.65))+
    theme(text = element_text(family = default_font_family))
}
p1 = ggarrange(fig_list1[[1]], fig_list1[[2]], fig_list1[[3]],
               fig_list1[[4]], fig_list1[[5]], fig_list1[[6]],
               ncol=3, nrow=2, common.legend = TRUE, legend="bottom")
p1
ggsave("figs/loo_pooled_limit_x.pdf", p1, width = 10, height = 6.18)

# Stratified analysis
pacman::p_load(purrr)
sub_files = list.files("fit", full.names = TRUE, pattern = ".{3}00.{3}.rds")
driver_comb = list.files("fit", pattern = ".{3}00.{3}.rds") %>%
  gsub(".rds", "", .) %>%
  gsub("00", " ", .)

fig_list2 = list()
for (i in seq_along(sub_files)) {
  z = readRDS(sub_files[i])
  fig_list2[[i]] = ppc(z, 2) +
    labs(subtitle = driver_comb[i] %>%
           gsub("DCS", "Dedicated,", .) %>%
           gsub("VAN", "Final-mile,", .) %>%
           gsub("JBI", "Intermodal,", .) %>%
           gsub("LOC", "Local", .) %>%
           gsub("OTR", "Over-the-road", .) %>%
           gsub("REG", "Regional", .))  +
    xlim(c(0.45, 0.7))+
    theme(text = element_text(family = default_font_family))
}

p2 = ggarrange(fig_list2[[1]], fig_list2[[2]],
               fig_list2[[3]], fig_list2[[4]],
               fig_list2[[5]], fig_list2[[6]],
               fig_list2[[7]],
               ncol=2, nrow = 4,
               common.legend = TRUE, legend = "bottom")
p2
ggsave("figs/loo_stratified_limit_x_20200227.pdf", p2, width = 10, height = 6.18)



