# Manage source data
# 1. original ping       -> d
# 2. weather             -> w
# 3. driver information  -> dinfo
# 4. SCEs                -> ce
pacman::p_load(data.table, dplyr, lubridate, fst)

# 1. original ping -> d
d = fread("data/original/Meeeting-11-16-18%2Fpings_500drivers_11.csv") %>% 
  .[,.(driver = gsub("\"", "", V7), ping_time = ymd_hms(V2), 
       speed = V3, lat = V4, lon = V6)]  %>% 
  .[!is.na(ping_time)] %>% 
  setkey(driver, ping_time) %>% 
  .[,diff := as.integer(difftime(ping_time, 
                shift(ping_time, type = "lag",
                      fill = 0), units = "mins")), driver] %>%
  .[,diff := {diff[1] = 0L; diff}, driver]
fst::write_fst(d, "data/cleaned/01a_ping_original_500drivers.fst", compress = 100)

# 1b. create driver list -> d_list
d_list = d[,.(n_ping = .N), driver]
fst::write_fst(d_list, "data/cleaned/00driver_list.fst", compress = 100)

# 2. weather -> w
w = vroom::vroom(dir("data/weather", "\\.csv$", full.names = T)) %>% 
  as.data.table() %>% 
  .[,DATIME := lubridate::mdy_hm(DATIME)] %>% 
  .[,.(DATIME, LATITUDE, LONGITUDE, PRECIP_INTENSITY, PRECIP_PROBABILITY, 
       WIND_SPEED, VISIBILITY, SUNRISE_TIME, SUNSET_TIME, 
       DURING_SUNSET, DURING_DUSK, DURING_SUNRISE, DURING_DAWN)]
fst::write_fst(w, "data/cleaned/02weather.fst", compress = 100)

# 3. driver demographic -> dinfo
alldr = fread("data/original/ALL_DRIVERS_DATA2016-09-30 10-53-42.csv") %>% 
  .[,EMPLID := stringr::str_replace_all(EMPLID, " ", "")]
d_list = fst::read_fst("data/cleaned/driver_list.fst")
dinfo = fread("data/original/ALPHA_TO_EMPLID2016-10-21 14-00-24.csv") %>% 
  .[,driver := tolower(ALPHA)] %>% 
  .[,`:=`(driver = stringr::str_replace_all(driver, " ", ""),
          EMPLID = stringr::str_replace_all(EMPLID, " ", ""))] %>% 
  .[,.(driver, EMPLID)] %>% 
  setkey(driver) %>% 
  merge(d_list, by = "driver", all.y = TRUE) %>% 
  merge(alldr, by = "EMPLID", all.x = TRUE) %>% 
  .[,BIRTHDATE := ymd(BIRTHDATE)] %>% 
  .[!is.na(BIRTHDATE),] %>% 
  setkey(driver) %>% 
  .[,n_missing := rowSums(is.na(.))] %>% 
  .[order(driver, -n_missing)] %>% 
  .[,head(.SD, 1), by = driver] %>% 
  .[, age := 2015 - year(BIRTHDATE)] %>% 
  #.[!(driver %in% c("kisi", "codc"))] %>% 
  .[,.(driver, EMPLID, age, race = ETHNIC_GROUP, gender = GENDER)] %>% 
  .[,race := case_when(race == "BLACK     " ~ "Black",
                       race == "WHITE     " ~ "White",
                       TRUE ~ "Other")]
fst::write_fst(dinfo, "data/cleaned/03driver_information.fst", compress = 100)

# 4. SCEs -> ce
dinfo = fst::read_fst("data/cleaned/03driver_information.fst")
ce = fread("data/original/CRITICAL_EVENT_QUERY2016-09-30 10-58-28.csv") %>% 
  .[,`:=`(EMPLID = stringr::str_replace_all(EMPLID, " ", ""),
          EVT_TYP = stringr::str_replace_all(EVT_TYP, " ", ""))] %>%
  .[,event_time := ymd_hms(paste(EVENT_DATE, EVENT_HOUR, sep = " "))] %>% 
  .[,.(EMPLID, event_time, event_type = EVT_TYP)] %>% 
  merge(dinfo, by = "EMPLID", all.y = TRUE) %>% 
  .[!is.na(event_time),.(driver, event_time, event_type)] %>% 
  .[,event_type := case_when(event_type == "HEADWAY" ~ "HW",
                             event_type == "HARD_BRAKING" ~ "HB",
                             event_type == "COLLISION_MITIGATION" ~ "CM",
                             event_type == "ROLL_STABILITY" ~ "RS")] %>% 
  unique()
fst::write_fst(ce, "data/cleaned/04safety_critical_events.fst", compress = 100)

