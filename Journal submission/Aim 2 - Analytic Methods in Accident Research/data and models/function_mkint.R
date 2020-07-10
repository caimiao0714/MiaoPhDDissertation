mkint = function(interval_length = 30, cumdrive_threshold = 14*60){
  # 30-minute intervals
  dint = dtrip %>% 
    .[,trip_units := ceiling(trip_time/interval_length)] %>% 
    .[rep(seq(.N), trip_units), !c("trip_time", "trip_units")] %>% 
    .[,add1 := 0:(.N-1), by = c("driver", "trip_id")] %>% 
    .[,start_time := start_time[1] + add1*interval_length*60, 
      .(driver, trip_id)] %>% 
    .[,end_time1 := shift(start_time, type = "lead"), .(driver, trip_id)] %>% 
    .[,end_time1 := {end_time1[.N] = end_time[.N]; end_time1}, .(driver, trip_id)] %>% 
    .[,c("end_time", "add1") := NULL] %>% 
    .[, interval_time := as.integer(difftime(end_time1, start_time, 
                                             units = "mins"))] %>% 
    setkey(driver, start_time, end_time1) %>% 
    .[, interval_id := 1:.N, driver] %>% 
    .[, .(interval_id, driver, start_time, end_time = end_time1, interval_time)] %>% 
    setkey(driver, start_time, end_time)
  
  # merge 30-minute intervals back to ping data
  d2_interval_id = d1 %>% 
    .[,dummy := ping_time] %>% 
    setkey(driver, ping_time, dummy) %>% 
    foverlaps(dint, type = "within",
              by.x = c("driver", "ping_time", "dummy"),
              mult = "first", nomatch = NA) %>% 
    .[, dummy := NULL]
  
  # aggregate ping to 30-minute intervals
  agg_int30 = d2_interval_id %>% 
    .[!is.na(interval_id) & trip_id != 0,] %>% 
    .[,.(trip_id = trip_id[1], shift_id = shift_id[1], 
         start_time = start_time[1], end_time = end_time[1],
         interval_time = interval_time[1], 
         n_ping = .N,
         speed_mean = mean(speed, na.rm = TRUE),
         speed_sd = sd(speed, na.rm = TRUE),
         distance = sum(distance, na.rm = TRUE),
         age = age[1], race = race[1],
         prep_inten = mean(PRECIP_INTENSITY, na.rm = TRUE),
         prep_prob = mean(PRECIP_PROBABILITY, na.rm = TRUE),
         wind_speed = mean(WIND_SPEED, na.rm = TRUE),
         visibility = mean(VISIBILITY, na.rm = TRUE),
         sunrise = fifelse(sum(DURING_SUNRISE == "Y", na.rm = TRUE) > 0, 1, 0),
         sunset = fifelse(sum(DURING_SUNSET == "Y", na.rm = TRUE) > 0, 1, 0),
         dusk = fifelse(sum(DURING_DUSK == "Y", na.rm = TRUE) > 0, 1, 0),
         dawn = fifelse(sum(DURING_DAWN == "Y", na.rm = TRUE) > 0, 1, 0)), 
      by = c("driver", "interval_id")] %>% 
    .[,speed_sd := fifelse(is.na(speed_sd), 0, speed_sd)] %>% 
    setkey(driver, interval_id) %>% 
    .[,cumdrive := cumsum(interval_time), .(driver, shift_id)] %>% 
    .[,`:=`(prep_inten = fifelse(is.na(prep_inten), 0, prep_inten),
            prep_prob = fifelse(is.na(prep_prob), 0, prep_prob),
            wind_speed = fifelse(is.na(wind_speed), mean(wind_speed, na.rm = TRUE),
                                 wind_speed),
            visibility = fifelse(is.na(visibility), mean(visibility, na.rm = TRUE),
                                 visibility))]
  
  tokeep = agg_int30 %>% 
    .[,.(maxdrive = max(cumdrive)), .(driver, shift_id)] %>% 
    .[maxdrive <= cumdrive_threshold]
  
  ce_tab = agg_int30 %>% 
    merge(tokeep, by = c("driver", "shift_id")) %>% 
    .[,maxdrive := NULL]
  
  return(ce_tab)
}


