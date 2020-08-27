pacman::p_load(data.table, dplyr, corrplot, DataExplorer, ggplot2, summarytools)
extrafont::font_import(pattern = "lmroman*", prompt = FALSE)
# y
default_font_family = "LM Roman 10"


d = fread("data/Aim1_crash_SCE.csv") %>%
  .[!(driver %in%
             c("abad", "beaj10", "beni0", "bonr7", "booa0", "brad6", "bror7",
               "busk2", "carr5", "clec25", "cunj19", "cure", "dund52", "gar15r",
               "grit21", "gros", "guar", "gulc1", "gurj", "hal166", "harj2",
               "higt12", "host", "johj48", "johw9", "jons86", "kena", "kesk",
               "kisi", "kopc", "lamr27", "lasr2", "leeb1", "lege3", "manr4",
               "mezj0", "mitm3", "monr83", "ouza1", "pert22", "phij2", "rivl13",
               "ruba0", "ruic4", "salm37", "sanm13", "shar3", "sopp2", "thog50",
               "thog76", "thom12", "thori1", "upsj"))] %>%
  .[,`:=`(gender = case_when(gender == "F" ~ "Female",
                             gender == "M" ~ "Male",
                             gender == "U" ~ "Unknown"),
          bus_unit = case_when(bus_unit == "DCS00" ~ "Dedicated",
                               bus_unit == "JBI00" ~ "Intermodal",
                               bus_unit == "VAN00" ~ "Final-mile"),
          d_type = case_when(d_type == "LOC" ~ "Local",
                             d_type == "OTR" ~ "Over-the-road",
                             d_type == "REG" ~ "Regional"))]

fwrite(d, "Github_SCE_crash/private/truck_crash_SCE.csv")
fwrite(d, "data/Aim1_crash_SCE_TRC_1rev.csv")



########### Read in data ###########
d = fread("data/Aim1_crash_SCE_TRC_1rev.csv") %>%
  .[,`:=`(bus_unit = factor(bus_unit,
             levels = c('Dedicated', 'Intermodal', 'Final-mile')),
          d_type = factor(d_type,
             levels = c('Local', 'Regional', 'Over-the-road')))]

# updated miles
m = fread("data/alldrivers_mile_driven.csv") %>%
  .[,.(Driver_ID, time_Mohammad = `total_triptime(min)`, mile_Mohammad = total_miles)]

nd = d %>%
  .[,.(driver, mile_Miao = distance, time_Miao = travel_time)] %>%
  merge(m, by.x = 'driver', by.y = 'Driver_ID', all.x = TRUE)



# Details on SCEs
sce = fread("data/CRITICAL_EVENT_QUERY2016-09-30 10-58-28.csv") %>%
  .[,EMPLID := as.numeric(EMPLID)] %>%
  .[EMPLID %in% d[,EMPLID]] %>%
  unique(by = c("EMPLID", "EVENT_HOUR"))
sce[,.N]


k = d %>%
  select(EMPLID, driver, sce_N) %>%
  left_join(sce[,.(nSCE_total = .N),EMPLID], by = "EMPLID") %>%
  mutate(nSCE_total = tidyr::replace_na(nSCE_total, 0)) %>%
  mutate(diff = nSCE_total - sce_N) %>%
  arrange(desc(diff)) %>%
  as.data.table()

sce[,.N,EVT_TYP]


d[,`:=`(sce_N = sce_N*10000/distance,
        sce_CM = sce_CM*10000/distance,
        sce_HB = sce_HB*10000/distance,
        sce_HW = sce_HW*10000/distance,
        sce_RS = sce_RS*10000/distance)]


#########################################
############ Correlation plot ###########

# First version of correlation plot ----
pacman::p_load(corrgram)
# plot_correlation(na.omit(d[,.(`Headway` = sce_HW,
#                               `Hard_brake` = sce_HB,
#                               `Rolling_stability` = sce_RS,
#                               `Collision_mitigation` = sce_CM,
#                               Age = age,
#                               Ping_speed = ping_speed,
#                               Gender = gender,
#                               `Business_unit` = bus_unit,
#                               `Driver_type` = d_type)]),
#                  maxcat = 20L,
#                  geom_text_args = list(family = "LM Roman 10"),
#                  cor_args = list("use" = "pairwise.complete.obs"),
#                  ggtheme = theme_minimal(),
#                  theme_config = list(legend.position = "top",
#                                      axis.text.x = element_text(angle = 45,
#                                                                 family = "LM Roman 10"),
#                                      axis.text.y = element_text(angle = 0,
#                                                                 family = "LM Roman 10")))
#
# pdf(width = 10, height = 6.18, file = "figs/correlation_plot.pdf")
# #par(mai = rep(5, 4), mar = rep(5, 4), xpd = NA)
# corrplot(d[,.(`Headway` = sce_HW,
#               `Hard brake` = sce_HB,
#               `Rolling stability` = sce_RS,
#               `Collision mitigation` = sce_CM,
#               Age = age,
#               `Ping speed` = ping_speed,
#               Dedicated = ifelse(bus_unit == "Dedicated", 1, 0),
#               Intermodal = ifelse(bus_unit == "Intermodal", 1, 0),
#               `Final-mile` = ifelse(bus_unit == "Final-mile", 1, 0),
#               Local = ifelse(d_type == "Local", 1, 0),
#               Regional = ifelse(d_type == "Regional", 1, 0),
#               `Over-the-road` = ifelse(d_type == "Over-the-road", 1, 0))] %>%
#            cor(),
#          method="color",
#          col = rev(RColorBrewer::brewer.pal(n = 8, name = "RdYlBu")),
#          type="lower",number.font = 7,
#          addCoef.col = "black", # Add coefficient of correlation
#          tl.col="black", tl.srt = 25,
#          diag=FALSE,
#          mar = rep(0, 4),
#          xpd = NA
# )
# dev.off()


# Second version of correlation plot
pacman::p_load(psych, corrplot)
r0 = d[,.(`Headway` = sce_HW,
     `Hard brake` = sce_HB,
     `Rolling stability` = sce_RS,
     `Collision mitigation` = sce_CM,
     Age = age,
     Gender = as.integer(factor(gender)),
     `Ping speed` = ping_speed,
     `Business unit` = as.integer(factor(bus_unit,
        levels = c('Dedicated', 'Intermodal', 'Final-mile'))),
     `Driver type` = as.integer(factor(d_type,
        levels = c('Local', 'Regional', 'Over-the-road'))))] %>%
  mixed.cor()
pdf(width = 8, height = 6.18, file = "figs/correlation_plot.pdf")
corrplot(r0$rho,
         method="color",
         col = rev(RColorBrewer::brewer.pal(n = 8, name = "RdYlBu")),
         type="lower",number.font = 7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt = 25,
         diag=FALSE,
         mar = rep(0, 4),
         xpd = NA)
dev.off()




#########################################
############    VIF Test   ##############
v_crash = lm(n_crash ~ sce_HW + sce_HB + sce_RS + sce_CM +
           age + ping_speed + gender + bus_unit + d_type, data = d)
v_fatal = lm(fatalities ~ sce_HW + sce_HB + sce_RS + sce_CM +
             age + ping_speed + gender + bus_unit + d_type, data = d)
v_injury = lm(injuries ~ sce_HW + sce_HB + sce_RS + sce_CM +
             age + ping_speed + gender + bus_unit + d_type, data = d)
v_ded_loc = lm(n_crash ~ sce_HW + sce_HB + sce_RS + sce_CM +
                 age + ping_speed + gender,
               data = d[bus_unit == 'Dedicated' & d_type == 'Local',])
v_ded_reg = lm(n_crash ~ sce_HW + sce_HB + sce_RS + sce_CM +
                 age + ping_speed + gender,
               data = d[bus_unit == 'Dedicated' & d_type == 'Regional',])
v_ded_otr = lm(n_crash ~ sce_HW + sce_HB + sce_RS + sce_CM +
                 age + ping_speed + gender,
               data = d[bus_unit == 'Dedicated' & d_type == 'Over-the-road',])
v_int_reg = lm(n_crash ~ sce_HW + sce_HB + sce_RS + sce_CM +
                 age + ping_speed + gender,
               data = d[bus_unit == 'Intermodal' & d_type == 'Regional',])
v_int_loc = lm(n_crash ~ sce_HW + sce_HB + sce_RS + sce_CM +
                 age + ping_speed + gender,
               data = d[bus_unit == 'Intermodal' & d_type == 'Local',])
v_fim_otr = lm(n_crash ~ sce_HW + sce_HB + sce_RS + sce_CM +
                 age + ping_speed + gender,
               data = d[bus_unit == 'Final-mile' & d_type == 'Over-the-road',])
v_fim_reg = lm(n_crash ~ sce_HW + sce_HB + sce_RS + sce_CM +
                 age + ping_speed + gender,
               data = d[bus_unit == 'Final-mile' & d_type == 'Regional',])

getvif = function(fit, fit_name){
  out = car::vif(fit) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Variables") %>%
    select(1, 4) %>%
    mutate(`GVIF^(1/(2*Df))` = round(`GVIF^(1/(2*Df))`, 3)) %>%
    `colnames<-`(c('Variables', fit_name))
  return(out)
}
getvif1 = function(fit, fit_name){
  out = car::vif(fit) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Variables") %>%
    `colnames<-`(c('Variables', 'VIF')) %>%
    mutate(VIF = round(VIF, 3)) %>%
    `colnames<-`(c('Variables', fit_name))
  return(out)
}


vif_d = getvif(v_crash, "Crash, all drivers") %>%
  left_join(getvif(v_fatal, "Fatality, all drivers")) %>%
  left_join(getvif(v_injury, "Injury, all drivers")) %>%
  left_join(getvif(v_ded_loc, "Crash, Dedicated & Local")) %>%
  left_join(getvif(v_ded_reg, "Crash, Dedicated & Regional")) %>%
  left_join(getvif(v_ded_otr, "Crash, Dedicated & Over-the-road")) %>%
  left_join(getvif(v_int_loc, "Crash, Intermodal & Local")) %>%
  left_join(getvif(v_int_reg, "Crash, Intermodal & Regional")) %>%
  left_join(getvif(v_fim_reg, "Crash, Final-mile & Regional")) %>%
  left_join(getvif1(v_fim_otr, "Crash, Final-mile & Over-the-road")) %>%
  mutate(Variables = c("Headways", "Hard brakes", "Rolling stability",
                       "Collision mitigation", "Age", "Ping speed", "Gender",
                       "Business unit", "Driver type"))



vif_d %>%
  knitr::kable("latex",
               align = rep(c("c"),10),
               booktabs = TRUE,
               label = "vif",
               caption = "Variance inflation factor test for multicollinearity.") %>%
  kableExtra::kable_styling(latex_options = c("scale_down"))





##############################################
#####   driver/trip characteristics   ########
# Output driver/trip characteristics
z = fread("data/Aim1_crash_SCE_TRC_1rev.csv") %>%
  .[,travel_time := travel_time/60]
z[,.(N_drivers = .N, # 31828 drivers
     N_trips = sum(n_trip), # 18740142 trips
     N_distance = sum(distance), # 2320967467 miles
     Travel_time = sum(travel_time))] # 65646731 hours

z[,.(TN = sum(sce_N),
     CM = sum(sce_CM),
     HB = sum(sce_HB),
     HW = sum(sce_HW),
     RS = sum(sce_RS))]
z[,.(n_crash = sum(n_crash),
     n_fatal = sum(fatalities),
     n_injur = sum(injuries))]

d[,freq(gender)] # gender
d[,descr(age)]
d[,freq(bus_unit)]
d[,freq(d_type)]


# add ping data description Table
options(scipen = 999)
readxl::read_excel("data/Tables.xlsx", sheet = "Sheet1") %>%
  knitr::kable("latex",
               align = rep(c("l", "r"), 3),
               booktabs = TRUE,
               label = "pingdata",
               format.args = list(big.mark = ','),
               caption = "A description of the data sources") %>%
  kableExtra::kable_styling(latex_options = c("scale_down")) %>%
  gsub("NA", " ", .) %>%
  gsub("Ping & ...2", "\\multicolumn{2}{c}{Ping}", ., fixed=T) %>%
  gsub("SCEs & ...4", "\\multicolumn{2}{c}{SCEs}", ., fixed=T) %>%
  gsub("Crashes & ...6", "\\multicolumn{2}{c}{Crashes}", ., fixed=T) %>%
  gsub("Trips & ...8", "\\multicolumn{2}{c}{Trips}", ., fixed=T)


# Add table 1 for drivers characteristics and predictor variables
pacman::p_load(tableone)
tmp1 = print(CreateContTable(vars = c('sce_HW', 'sce_HB', 'sce_RS', 'sce_CM',
                                        'age', 'ping_speed'),
                               data = d),
               printToggle = FALSE) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "variable") %>%
  slice(-1)
tmp2 = print(CreateCatTable(vars = c('gender', 'bus_unit', 'd_type'),
                             data = d),
              printToggle = FALSE) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "variable") %>%
  slice(-1)

bind_rows(tmp1, tmp2) %>%
  knitr::kable("latex",
               caption = "A summary of driver characteristics and predictor variables",
               align = c("l", "r"),
               booktabs = TRUE,
               label = "pingdata",
               format.args = list(big.mark = ','),
               linesep = "") %>%
  gsub("sce\\_HW", "Rate of headways", ., fixed = TRUE) %>%
  gsub("sce\\_HB", "Rate of hard brakes", ., fixed = TRUE) %>%
  gsub("sce\\_CM", "Rate of collision mitigation", ., fixed = TRUE) %>%
  gsub("sce\\_RS", "Rate of rolling stability", ., fixed = TRUE) %>%
  gsub("gender", "Gender", ., fixed = TRUE) %>%
  gsub("age", "Age", ., fixed = TRUE) %>%
  gsub("Ping\\_speed", "Ping speed", ., fixed = TRUE) %>%
  gsub("bus\\_unit", "Business unit", ., fixed = TRUE) %>%
  gsub("d\\_type", "Driver type", ., fixed = TRUE)







