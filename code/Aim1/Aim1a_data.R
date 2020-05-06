pacman::p_load(data.table, dplyr, lubridate)
setDTthreads(parallel::detectCores())
d = data.table::fread("data/20190908_all_trips.csv") %>% 
  .[,`:=`(start_time = ymd_hms(start_time),
          end_time = ymd_hms(end_time))]

# Aggregate trips to drivers level
driver_trip = d[,.(n_trip = .N, 
             ping_speed = mean(ping_speed, na.rm = TRUE),
             distance = sum(distance, na.rm = TRUE),
             travel_time = sum(trip_time, na.rm = TRUE)), 
          driver]

# Merge crashes
crash = 'data/ALL_ACCIDENTS_DATA2016-10-21 14-02-03.csv' %>% 
  fread() %>% 
  .[,`:=`(OPEN_DATE = ymd(OPEN_DATE),
         OPERATOR_CODE = tolower(gsub("\\s", "", 
                                      OPERATOR_CODE)))] %>% 
  .[OPEN_DATE >= ymd('2015-04-01') & 
      OPEN_DATE <= ymd('2016-03-29') & 
      OPERATOR_CODE %in% driver_trip[,driver],
    .(driver = OPERATOR_CODE,FATALITIES, 
      NBR_OF_INJURIES, NON_DOT_INJURIES)] %>% 
  .[, .(n_crash = .N,
        fatalities = sum(FATALITIES), 
        injuries = sum(NBR_OF_INJURIES),
        non_dot_injuries = sum(NON_DOT_INJURIES)), driver]

# Merge SCEs
sce = "data/CRITICAL_EVENT_QUERY2016-09-30 10-58-28.csv" %>% 
  fread() %>% 
  .[,.(EMPLID, EVENT_DATE, EVENT_HOUR, EVT_TYP)] %>% 
  setkey(EMPLID, EVENT_DATE, EVENT_HOUR, EVT_TYP) %>% 
  unique() %>% 
  .[,`:=`(EMPLID = gsub("\\s", "", EMPLID),
          EVT_TYP = gsub("\\s", "", EVT_TYP),
          EVENT_DATE = ymd(EVENT_DATE))] %>%
  .[EVT_TYP != "" & EVENT_DATE <= ymd("2016-10-14")] %>% 
  .[,.(n_SCE = .N), .(EMPLID, EVT_TYP)] %>% 
  .[,EVT_TYP := case_when(EVT_TYP == "HEADWAY" ~ "sce_HW",
                          EVT_TYP == "HARD_BRAKING" ~ "sce_HB",
                          EVT_TYP == "COLLISION_MITIGATION" ~ "sce_CM",
                          EVT_TYP == "ROLL_STABILITY" ~ "sce_RS")] %>% 
  dcast(EMPLID ~ EVT_TYP, fill = 0, value.var = "n_SCE") %>% 
  .[,sce_N := sce_CM + sce_HB + sce_HW + sce_RS]


# Merge driver features
driver_tab = "data/ALL_DRIVERS_DATA2016-09-30 10-53-42.csv" %>% 
  data.table::fread() %>% 
  .[,.(EMPLID, BIRTHDATE, GENDER, YEARS_OF_EXP, CURRENT_HIRE_DATE, 
      TERM_DATE, BUSINESS_UNIT, DRIVER_TYPE)] %>%
  .[,`:=`(EMPLID = gsub("\\s", "", EMPLID),
          BIRTHDATE = ymd(BIRTHDATE),
          CURRENT_HIRE_DATE = ymd(CURRENT_HIRE_DATE),
          TERM_DATE = ymd(TERM_DATE))]

# Alpha to ID
alphaid = "data/ALPHA_TO_EMPLID2016-10-21 14-00-24.csv" %>% 
  data.table::fread() %>% 
  .[,`:=`(EMPLID = gsub("\\s", "", EMPLID), 
       driver = tolower(gsub("\\s", "", ALPHA)))] %>% 
  .[,.(EMPLID, driver)] %>% 
  .[driver_trip, on = "driver"] %>% # trips data
  merge(driver_tab, by = "EMPLID", all.x = TRUE) %>% # driver table
  .[,n_missing := rowSums(is.na(.))] %>% 
  .[order(driver, n_missing)] %>% 
  .[,.SD[1], driver] %>% 
  .[,age := 2015 - year(BIRTHDATE)] %>% 
  .[,.(EMPLID, driver, gender = GENDER, years_exp = YEARS_OF_EXP,
       bus_unit = BUSINESS_UNIT, d_type = DRIVER_TYPE, 
       age, n_trip, ping_speed, distance, travel_time)] %>% 
  merge(crash, by = "driver", all.x = TRUE) %>% 
  merge(sce, by = "EMPLID", all.x = TRUE) %>% 
  .[,`:=`(n_crash = ifelse(is.na(n_crash), 0, n_crash),
          fatalities = ifelse(is.na(fatalities), 0, fatalities),
          injuries = ifelse(is.na(injuries), 0, injuries),
          non_dot_injuries = ifelse(is.na(non_dot_injuries), 0,
                                    non_dot_injuries),
          sce_CM = ifelse(is.na(sce_CM), 0, sce_CM),
          sce_HB = ifelse(is.na(sce_HB), 0, sce_HB),
          sce_HW = ifelse(is.na(sce_HW), 0, sce_HW),
          sce_RS = ifelse(is.na(sce_RS), 0, sce_RS),
          sce_N = ifelse(is.na(sce_N), 0, sce_N))]

fwrite(alphaid, "data/Aim1_crash_SCE.csv")

zz = fread("data/Aim1_crash_SCE.csv")



