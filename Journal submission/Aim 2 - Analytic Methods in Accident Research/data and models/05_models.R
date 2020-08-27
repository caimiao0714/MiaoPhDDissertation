pacman::p_load(data.table, dplyr, ggplot2, fst, lubridate)
d = fst::read_fst("data/cleaned/31interval30_CE.fst") %>% as.data.table()
# d[,min(start_time)] -> "2015-04-01 00:01:00 UTC"
# d[,max(end_time)]   -> "2016-03-30 UTC"

holiday_list = 
  ymd(c("2015-05-25", # Memorial Day
    "2015-07-03", # Independence Day
    "2015-09-07", # Labor Day
    "2015-11-11", # Veterans Day
    "2015-11-26", # Thanksgiving
    "2015-12-25", # Christmas Day
    "2016-01-01", # New Year's Day
    "2016-01-18" # Martin Luther King Jr. Day
))

z = d %>% 
  .[,cumdrive := cumdrive/60] %>% 
  .[,maxdrive := max(cumdrive), .(driver, shift_id)] %>% 
  .[maxdrive <= 11] %>% 
  .[,`:=`(maxdrive = NULL, 
          CE_binary = fifelse(nCE > 0, 1, 0),
          gender = ifelse(gender == "F", "F", "M"),
          day_of_week = lubridate::wday(start_time, label = TRUE),
          day_hour = lubridate::hour(start_time))] %>% 
  .[,`:=`(race = factor(race, levels = c("White", "Black", "Other")),
          gender = factor(gender, levels = c("M", "F")),
          weekend = if_else(day_of_week %in% c("Sat", "Sun"), 1, 0),
          holiday = if_else(lubridate::date(start_time) %in% holiday_list, 1, 0),
          hour_of_day = case_when(day_hour >= 21 | day_hour <=  5 ~ "21 p.m. -  5 a.m.",
                                  day_hour >=  6 & day_hour <= 10 ~ " 6 a.m. - 10 a.m.",
                                  day_hour >= 11 & day_hour <= 14 ~ "11 a.m. - 14 p.m.",
                                  day_hour >= 15 & day_hour <= 20 ~ "15 p.m. - 20 p.m."))] %>% 
  .[,hour_of_day := factor(hour_of_day, levels = c("11 a.m. - 14 p.m.", 
                                                   "15 p.m. - 20 p.m.",
                                                   " 6 a.m. - 10 a.m.",
                                                   "21 p.m. -  5 a.m."))]

# fst::write_fst(z, "data/cleaned/31interval30_holiday_weekend.fst")

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








