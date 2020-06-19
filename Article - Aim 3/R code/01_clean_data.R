pacman::p_load(dplyr, data.table, ggplot2, fst, lubridate, geosphere)

# 1. Remove drivers with less than 100 pings
# 2. Remove shifts less than 30 minutes of driving
# 3. Remove trips with 5 minutes of driving

# ******************************************************************
# *************************  Read in data  *************************
# ******************************************************************
d30 = read_fst('Data/cleaned/32interval30_CE_11hours_limit.fst') %>% 
  as.data.table()
# A list of useful shift ID and trip ID
dshift = d30 %>% 
  .[,.(start_time = start_time[1], end_time = end_time[.N]),.(driver, shift_id)]
dtrip = d30 %>% 
  .[,.(start_time = start_time[1], end_time = end_time[.N]),
    .(driver, shift_id, trip_id)] %>% 
  setkey(driver, start_time, end_time)

# Read ping
ping = read_fst('Data/cleaned/11ping_add_trip_shift_id.fst') %>% 
  as.data.table() %>% 
  .[,c('ping_id', 'trip_id', 'shift_id', 'distance', 'age', 'race') := NULL]
ddemo = read_fst('Data/cleaned/03driver_information.fst') %>% 
  as.data.table()


# ******************************************************************
# *****************  Keep the ping data for Aim 3  *****************
# ******************************************************************
pshift1 = ping %>% 
  .[driver != 'kisi'] %>% 
  # Remove drivers with less than 100 pings
  .[,ping_per_driver := .N, driver] %>% 
  .[ping_per_driver > 100,] %>% 
  merge(ddemo[,.(driver, age, race, gender)], all.x = TRUE) %>% 
  .[,dummy := ping_time] %>% 
  setkey(driver, ping_time, dummy) %>% 
  foverlaps(dtrip, mult = "all", type = "within", nomatch = NA) %>% 
  .[!is.na(shift_id)] %>% 
  .[,trip_time := as.integer(difftime(end_time, start_time, units = "mins"))] %>% 
  # 3. Remove trips with 5 minutes of driving
  .[trip_time >= 5,] %>% 
  .[,`:=`(lon1 = shift(lon, type = "lag", fill = NA),
          lat1 = shift(lat, type = "lag", fill = NA)),
    .(driver, shift_id, trip_id)] %>% 
  .[,distance := distHaversine(cbind(lon, lat), cbind(lon1, lat1))] %>% 
  .[,distance := round(distance/1609.344, 3)] %>% 
  .[,c('lon1', 'lat1', 'dummy', 'ping_per_driver') := NULL] %>% 
  .[,driver_id := as.integer(as.factor(driver))] %>% 
  setkey(driver, ping_time)


# ************************************************************
# **************  Aggregate to shifts for NHPP  **************
dnhpp = pshift1 %>% 
  .[,.(start_time = ping_time[1], end_time = ping_time[.N],
     start_lat = lat[1], start_lon = lon[1],
     end_lat = lat[.N], end_lon = lon[.N],
     speed_mean = mean(speed, na.rm = TRUE),
     speed_sd = sd(speed, na.rm = TRUE),
     N_ping = .N,
     distance = sum(distance, na.rm = TRUE),
     age = age[1], race = race[1], gender = gender[1],
     prep_inten = mean(PRECIP_INTENSITY, na.rm = TRUE),
     prep_prob = mean(PRECIP_PROBABILITY, na.rm = TRUE),
     wind_speed = mean(WIND_SPEED, na.rm = TRUE),
     visibility = mean(VISIBILITY, na.rm = TRUE),
     sunrise = fifelse(sum(DURING_SUNRISE == "Y", na.rm = TRUE) > 0, 1, 0),
     sunset = fifelse(sum(DURING_SUNSET == "Y", na.rm = TRUE) > 0, 1, 0),
     dusk = fifelse(sum(DURING_DUSK == "Y", na.rm = TRUE) > 0, 1, 0),
     dawn = fifelse(sum(DURING_DAWN == "Y", na.rm = TRUE) > 0, 1, 0)),
  .(driver, driver_id, shift_id)] %>%
  .[,`:=`(shift_time = as.integer(difftime(end_time, start_time,
                                          units = "mins")),
          speed_sd  = fifelse(is.na(speed_sd), 0, speed_sd),
          prep_inten = fifelse(is.na(prep_inten), 0, prep_inten),
          prep_prob = fifelse(is.na(prep_prob), 0, prep_prob),
          wind_speed = fifelse(is.na(wind_speed), mean(wind_speed, na.rm = TRUE),
                               wind_speed),
          visibility = fifelse(is.na(visibility), mean(visibility, na.rm = TRUE),
                               visibility))] %>%
  # 2. Remove shifts less than 30 minutes of driving
  .[shift_time >= 30] %>% 
  .[order(driver, start_time)] %>% 
  .[,shift_id_num := 1:.N, .(driver)]



# Merge shifts data back to ping to get shifts with more than 30 minutes
pshift2 = pshift1 %>% 
  left_join(dnhpp[,.(driver, shift_id, shift_id_num)], 
            by = c("driver", "shift_id")) %>% 
  filter(!is.na(shift_id_num)) %>% 
  as.data.table()


# ************************************************************
# **************   Aggregate to trips for PLP   **************
djplp = pshift2 %>% 
  .[order(driver, start_time)] %>% 
  .[,.(start_time = ping_time[1], end_time = ping_time[.N],
       start_lat = lat[1], start_lon = lon[1],
       end_lat = lat[.N], end_lon = lon[.N],
       speed_mean = mean(speed, na.rm = TRUE),
       speed_sd = sd(speed, na.rm = TRUE),
       N_ping = .N,
       distance = sum(distance, na.rm = TRUE),
       age = age[1], race = race[1], gender = gender[1],
       prep_inten = mean(PRECIP_INTENSITY, na.rm = TRUE),
       prep_prob = mean(PRECIP_PROBABILITY, na.rm = TRUE),
       wind_speed = mean(WIND_SPEED, na.rm = TRUE),
       visibility = mean(VISIBILITY, na.rm = TRUE),
       sunrise = fifelse(sum(DURING_SUNRISE == "Y", na.rm = TRUE) > 0, 1, 0),
       sunset = fifelse(sum(DURING_SUNSET == "Y", na.rm = TRUE) > 0, 1, 0),
       dusk = fifelse(sum(DURING_DUSK == "Y", na.rm = TRUE) > 0, 1, 0),
       dawn = fifelse(sum(DURING_DAWN == "Y", na.rm = TRUE) > 0, 1, 0)),
    .(driver, driver_id, shift_id, shift_id_num, trip_id)] %>%
  .[,`:=`(trip_time = as.integer(difftime(end_time, start_time,
                                           units = "mins")),
          speed_sd  = fifelse(is.na(speed_sd), 0, speed_sd),
          prep_inten = fifelse(is.na(prep_inten), 0, prep_inten),
          prep_prob = fifelse(is.na(prep_prob), 0, prep_prob),
          wind_speed = fifelse(is.na(wind_speed), mean(wind_speed, na.rm = TRUE),
                               wind_speed),
          visibility = fifelse(is.na(visibility), mean(visibility, na.rm = TRUE),
                               visibility))] %>%
  # Produce cumulative driving time for each trip within a shift
  .[,`:=`(t_trip_start = cumsum(trip_time) - trip_time,
          t_trip_end = cumsum(trip_time)), 
    .(driver, shift_id, shift_id_num)] %>% 
  .[order(driver, start_time)] %>% 
  .[,trip_id_num := 1:.N,.(driver, shift_id_num)]

# Produce tau for each shift
dtau = djplp %>% 
  .[,.(tau = sum(trip_time)),.(driver, shift_id_num)]

# ************************************************************
# **********************   Keep SCEs   ***********************
jplp_id = djplp %>% 
  .[,.(driver, driver_id, shift_id_num, trip_id_num, start_time, end_time, 
       t_trip_start, t_trip_end)] %>% 
  setkey(driver, start_time, end_time)

sce = read_fst('Data/cleaned/04safety_critical_events.fst') %>% 
  as.data.table() %>% 
  .[,dummy := event_time] %>%
  setkey(driver, event_time, dummy) %>% 
  foverlaps(jplp_id, mult = "all", type = "within", nomatch = NA) %>% 
  .[!is.na(shift_id_num)] %>% 
  .[,`:=`(T2SCE_trip = as.integer(difftime(event_time, start_time, units = "mins")),
          trip_time = as.integer(difftime(end_time, start_time, units = "mins")),
          dummy = NULL)] %>% 
  .[order(driver_id, shift_id_num, trip_id_num)]

# The number of SCEs in each interval
SCE_shift = sce[,.(N_SCE = .N),.(driver, shift_id_num)]
SCE_trip = sce[,.(N_SCE = .N),.(driver, shift_id_num, trip_id_num)]

# Join SCEs back to shifts and trips
dnhpp1 = dnhpp %>% 
  left_join(dtau, by = c('driver', 'shift_id_num')) %>% 
  left_join(SCE_shift, by = c('driver', 'shift_id_num')) %>% 
  mutate(N_SCE = fifelse(is.na(N_SCE), 0, N_SCE),
         Black = fifelse(race == 'Black', 1, 0),
         Other_Race = fifelse(race == 'Other', 1, 0),
         Female = fifelse(gender == 'M', 1, 0)) %>% 
  select(driver, shift_id_num, everything(), -shift_id) %>% 
  as.data.table()
djplp1 = djplp %>% 
  left_join(dtau, by = c('driver', 'shift_id_num')) %>%
  left_join(SCE_trip, by = c('driver', 'shift_id_num', 'trip_id_num')) %>% 
  mutate(N_SCE = fifelse(is.na(N_SCE), 0, N_SCE),
         Black = fifelse(race == 'Black', 1, 0),
         Other_Race = fifelse(race == 'Other', 1, 0),
         Female = fifelse(gender == 'M', 1, 0)) %>% 
  select(driver, shift_id_num, trip_id_num, everything(), -shift_id, -trip_id) %>% 
  as.data.table()

write_fst(pshift1, 'Data/aim3_data/Aim3_ping.fst')
write_fst(dnhpp1, 'Data/aim3_data/dnhpp.fst')
write_fst(djplp1, 'Data/aim3_data/djplp.fst')
write_fst(sce, 'Data/aim3_data/sce.fst')




# ************************************************************
# ***********   Export data for GitHub upload   **************
dnhpp = as.data.table(read_fst('Data/OSC_data/dnhpp.fst')) %>% 
  .[,.(driver_id, shift_id_num, shift_time, tau, N_SCE, 
       speed_mean, speed_sd, N_ping, distance, age, Black, Other_Race, 
       Female, prep_inten, prep_prob, wind_speed, visibility)]
djplp = as.data.table(read_fst('Data/OSC_data/djplp.fst')) %>% 
  .[,.(driver_id, shift_id_num, trip_id_num, t_trip_start, t_trip_end, 
       trip_time, tau, N_SCE, speed_mean, speed_sd, 
       N_ping, distance, age, Black, Other_Race, Female, prep_inten, 
       prep_prob, wind_speed, visibility)]
sce = as.data.table(read_fst('Data/OSC_data/sce.fst')) %>% 
  .[,.(driver_id, shift_id_num, trip_id_num, t_trip_start, t_trip_end, 
       event_type, T2SCE_trip, trip_time)]
write_fst(dnhpp, 'Data/dnhpp.fst')
write_fst(djplp, 'Data/djplp.fst')
write_fst(sce, 'Data/sce.fst')
