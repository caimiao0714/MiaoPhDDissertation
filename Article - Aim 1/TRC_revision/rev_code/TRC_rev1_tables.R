pacman::p_load(huxtable, broom, dplyr, stringr)

z = readRDS("fit/a0_pooled_fit.rds")

inj = readRDS("fit/injury_fatality_fit.rds")

pad_est = function(x, digits = 3, exponentiate = TRUE){
  if(exponentiate){
    z = round(x, digits)
    z = ifelse(z == 0, "0.000", str_pad(z, digits + 2, "right", "0"))
  }else{
    z = ifelse(x < 0,
               round(x, digits) %>%
                 str_pad(digits + 3, "right", "0"),
               round(x, digits) %>%
                 str_pad(digits + 2, "right", "0") %>%
                 str_pad(digits + 3, "left", ' '))
  }
  return(z)
}

getfit = function(fit, model_name = "model_estimate", digits = 3, exponentiate = TRUE){
  z = tidy(fit, intervals = TRUE, prob = 0.95) %>%
    {if(exponentiate == TRUE){
      mutate(.,
             estimate = exp(estimate),
             lower = exp(lower),
             upper = exp(upper))
    }else{
      .
    }} %>%
    mutate(estimate = pad_est(estimate),
           ci = paste0("(", pad_est(lower), ", ",
                       pad_est(upper), ")")) %>%
    mutate(zz = paste0(estimate, " ", ci)) %>%
    select(term, zz) %>%
    magrittr::set_colnames(c("variables", model_name))
  return(z)
}

getloo = function(fit){
  d = fit$estimates %>%
    as.data.frame() %>%
    mutate(Estimate = round(Estimate, 1),
           SE = round(SE, 1)) %>%
    mutate(values = paste0(Estimate, " (", SE, ")")) %>%
    select(values)
  return(rbind(length(fit$diagnostics$pareto_k),
               d))
}


#######
# \newcolumntype{P}[1]{>{\centering\arraybackslash}p{#1}}

# table 1
p2 = getfit(inj[[1]], "Injuries: pooled") %>%
  full_join(getfit(inj[[2]], "Injuries: four SCEs")) %>%
  full_join(getfit(inj[[3]], "Fatalities: pooled")) %>%
  full_join(getfit(inj[[4]], "Fatalities: four SCEs")) %>%
  mutate(variables = gsub(pattern = '^b_', '', variables)) %>%
  mutate(variables = gsub(pattern = '^b_', '', variables, fixed = T))

tab_fit = getfit(z[[1]], "Crashes: pooled") %>%
  full_join(getfit(z[[2]], "Crashes: four SCEs")) %>%
  mutate(variables = gsub(pattern = '\\(|\\)', '', variables)) %>%
  full_join(p2) %>%
  mutate(variables = gsub("^b_", "", variables)) %>%
  filter(!(variables %in% c("shape", "lp__"))) %>%
  arrange(match(variables,
                c("Intercept", "sce_N", "sce_HW", "sce_HB", "sce_RS", "sce_CM",
                  "age", "ping_speed", "genderM", "genderU",
                  "bus_unitJBI00", "bus_unitVAN00",
                  "d_typeOTR", "d_typeREG"))) %>%
  mutate(variables =
           recode(variables,
                  "sce_N" = "All SCEs",
                  "sce_HW" = "Headways",
                  "sce_HB" = "Hard brakes",
                  "sce_RS" = "Rolling stability",
                  "sce_CM" = "Collision mitigation",
                  "age" = "Age",
                  "ping_speed" = "Mean speed",
                  "genderM" = "Gender: male",
                  "genderU" = "Gender: unknown",
                  "bus_unitJBI00" = "Business unit: Inter-modal",
                  "bus_unitVAN00" = "Business unit: Final-mile",
                  "d_typeOTR" = "Type: Over-the-road",
                  "d_typeREG" = "Type: Regional",
                  "Intercept" = "Intercept")) %>%
  add_row(variables = "Fit statistics: ")

tab_loo = data.frame(c("sample size", "elpd_loo", "p_loo", "looic")) %>%
  cbind(getloo(z[[7]])) %>%
  cbind(getloo(z[[8]])) %>%
  cbind(getloo(inj[[5]])) %>%
  cbind(getloo(inj[[6]])) %>%
  cbind(getloo(inj[[7]])) %>%
  cbind(getloo(inj[[8]])) %>%
  magrittr::set_colnames(names(tab_fit))


capture.output({
  stargazer::stargazer(rbind(tab_fit, tab_loo),
                       summary=FALSE, rownames=FALSE,
                       align=FALSE,  float = TRUE, table.placement = '!htbp',
                       title = "Bayesian NB regressions with the rate of SCEs predicting crashes, injuries, and fatalities",
                       font.size = "small",
                       single.row=FALSE, header=FALSE,
                       notes=c("The SCEs were measured as the number of events per 10,000 miles driven.",
                               "IRRs and associated 95\\% credible intervals were reported."),
                       label="tab:injuries")
}) %>%
  gsub("(", "\\newline (", ., fixed = T) %>%
  gsub("@{\\extracolsep{5pt}} ccccccc","p{0.15\\linewidth} P{0.12\\linewidth} P{0.12\\linewidth} P{0.12\\linewidth} P{0.12\\linewidth} P{0.12\\linewidth} P{0.12\\linewidth}", ., fixed = T) %>%
  cat()






# Second table
tab_fit = getfit(z[[1]], "Pooled model") %>%
  full_join(getfit(z[[2]], "Four SCEs")) %>%
  full_join(getfit(z[[3]], "Headways")) %>%
  full_join(getfit(z[[4]], "Hard brakes")) %>%
  full_join(getfit(z[[5]], "Rolling stability")) %>%
  full_join(getfit(z[[6]], "Collision mitigation")) %>%
  arrange(match(variables,
                c("(Intercept)", "sce_N", "sce_HW", "sce_HB", "sce_RS", "sce_CM",
                  "age", "ping_speed", "genderM", "genderU"))) %>%
  mutate(variables =
           recode(variables,
                  "sce_N" = "All SCEs",
                  "sce_HW" = "Headways",
                  "sce_HB" = "Hard brakes",
                  "sce_RS" = "Rolling stability",
                  "sce_CM" = "Collision mitigation",
                  "age" = "Age",
                  "ping_speed" = "Mean speed",
                  "genderM" = "Gender: male",
                  "genderU" = "Gender: unknown",
                  "bus_unitJBI00" = "Business unit: Inter-modal",
                  "bus_unitVAN00" = "Business unit: Final-mile",
                  "d_typeOTR" = "Type: Over-the-road",
                  "d_typeREG" = "Type: Regional",
                  "(Intercept)" = "Intercept")) %>%
  add_row(variables = "Fit statistics: ")

tab_loo = data.frame(c("sample size", "elpd_loo", "p_loo", "looic")) %>%
  cbind(getloo(z[[7]])) %>%
  cbind(getloo(z[[8]])) %>%
  cbind(getloo(z[[9]])) %>%
  cbind(getloo(z[[10]])) %>%
  cbind(getloo(z[[11]])) %>%
  cbind(getloo(z[[12]])) %>%
  magrittr::set_colnames(names(tab_fit))

capture.output({
  stargazer::stargazer(rbind(tab_fit, tab_loo),
                       summary=FALSE, rownames=FALSE,
                       #align=TRUE,
                       float = TRUE,
                       table.placement = '!htbp',
                       title = "Bayesian NB regressions with the rate of SCEs predicting crashes, non-stratified models",
                       font.size = "small",
                       single.row=FALSE, header=FALSE,
                       notes=c("The SCEs were measured as the number of events per 10,000 miles driven.",
                               "IRRs and associated 95\\% credible intervals were reported."),
                       label="tab:pooled")
}) %>%
  # gsub("\\begin{tabular}","\\resizebox{0.8\\paperheight}{!}{\\begin{tabular}", ., fixed=T) %>%
  # gsub("\\end{tabular}","\\end{tabular}}", ., fixed=T) %>%
  gsub("(", "\\newline (", ., fixed = T) %>%
  gsub("@{\\extracolsep{5pt}} ccccccc","p{0.15\\linewidth} P{0.12\\linewidth} P{0.12\\linewidth} P{0.12\\linewidth} P{0.12\\linewidth} P{0.12\\linewidth} P{0.12\\linewidth}", ., fixed = T) %>%
  cat()















# Third table
pacman::p_load(purrr)
sub_files = list.files("fit", full.names = TRUE, pattern = ".{3}00.{3}.rds")
driver_comb = list.files("fit", pattern = ".{3}00.{3}.rds") %>%
  gsub(".rds", "", .) %>%
  gsub("00", "-", .)

fit_list = list()
for (i in seq_along(sub_files)) {
  fit_list[[i]] = readRDS(sub_files[i]) %>%
    .[[2]] %>%
    getfit()
}

tab_fit1 = fit_list %>%
  purrr::reduce(full_join, by = "variables") %>%
  magrittr::set_colnames(c("variables", driver_comb)) %>%
  arrange(match(variables,
                c("(Intercept)", "sce_N", "sce_HW", "sce_HB", "sce_RS", "sce_CM",
                  "age", "ping_speed", "genderM", "genderU"))) %>%
  mutate(variables =
           recode(variables,
                  "sce_HW" = "Headways",
                  "sce_HB" = "Hard brakes",
                  "sce_RS" = "Rolling stability",
                  "sce_CM" = "Collision mitigation",
                  "age" = "Age",
                  "ping_speed" = "Mean speed",
                  "genderM" = "Gender: male",
                  "genderU" = "Gender: unknown",
                  "bus_unitJBI00" = "Business unit: Inter-modal",
                  "bus_unitVAN00" = "Business unit: Final-mile",
                  "d_typeOTR" = "Type: Over-the-road",
                  "d_typeREG" = "Type: Regional",
                  " Intercept " = "(Intercept)")) %>%
  add_row(variables = "Fit statistics: ")
names(tab_fit1) %<>%
  stringr::str_replace_all(., "-", " ") %>%
  stringr::str_replace_all(., "DCS", "Dedicated") %>%
  stringr::str_replace_all(., "JBI", "Inter-modal") %>%
  stringr::str_replace_all(., "VAN", "Final-mile") %>%
  stringr::str_replace_all(., "LOC", "Local") %>%
  stringr::str_replace_all(., "REG", "Regional") %>%
  stringr::str_replace_all(., "OTR", "Over-the-road")

loo_list = list()
for (i in seq_along(sub_files)) {
  loo_list[[i]] = readRDS(sub_files[i]) %>%
    .[[8]] %>%
    getloo()
}

tab_loo1 = data.frame(c("sample size", "elpd_loo", "p_loo", "looic")) %>%
  cbind(loo_list[[1]]) %>%
  cbind(loo_list[[2]]) %>%
  cbind(loo_list[[3]]) %>%
  cbind(loo_list[[4]]) %>%
  cbind(loo_list[[5]]) %>%
  cbind(loo_list[[6]]) %>%
  cbind(loo_list[[7]]) %>%
  magrittr::set_colnames(names(tab_fit1))

capture.output({
  stargazer::stargazer(rbind(tab_fit1, tab_loo1),
                       summary=FALSE, rownames=FALSE,
                       align=FALSE, float = TRUE, table.placement = 'h',
                       title = "Bayesian NB regressions with the rate of SCEs predicting crashes, stratified by business units and driver types",
                       font.size = "small",
                       single.row=FALSE,
                       header=FALSE,
                       notes=c("The SCEs were measured as the number of events per 10,000 miles driven.",
                               "IRR and associated 95\\% credible intervals were reported."),
                       label="tab:question3")
}) %>%
  # gsub("\\begin{tabular}","\\resizebox{0.8\\paperheight}{!}{\\begin{tabular}", ., fixed=T) %>%
  # gsub("\\end{tabular}","\\end{tabular}}", ., fixed=T) %>%
  gsub("10000","1.000", ., fixed=T) %>%
  gsub("(", "\\newline (", ., fixed = T) %>%
  gsub("@{\\extracolsep{5pt}} ccccccc","p{0.15\\linewidth} P{0.12\\linewidth} P{0.12\\linewidth} P{0.12\\linewidth} P{0.12\\linewidth} P{0.12\\linewidth} P{0.12\\linewidth}", ., fixed = T) %>%
  cat()














