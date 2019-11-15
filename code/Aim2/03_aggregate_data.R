pacman::p_load(data.table, dplyr, lubridate, fst)


d = fst::read_fst("data/cleaned/01c_ping_trip_id_500drivers.fst") %>% 
  as.data.table() %>% 
  setkey(driver, ping_time)
w = fst::read_fst("data/cleaned/02weather.fst") %>% as.data.table()
dinfo = fst::read_fst("data/cleaned/03driver_information.fst") %>% as.data.table()
ce = fst::read_fst("data/cleaned/04safety_critical_events.fst") %>% as.data.table()

# Merge weather & driver information to ping
d1 = d %>% 
  .[,`:=`(ping_id = 1:.N,
          DATIME = lubridate::floor_date(ping_time, "hours"),
          LATITUDE = as.numeric(gsub("([0-9]+\\.[0-9]{2})(.*)", "\\1", lat)),
          LONGITUDE = as.numeric(gsub("([0-9]+\\.[0-9]{2})(.*)", "\\1", lon)))] %>% 
  merge(w, by = c("DATIME", "LATITUDE", "LONGITUDE"), all.x = TRUE) %>% 
  merge(dinfo[,.(driver, age, race)], by = "driver", all.x = TRUE)


##########################
####       trip       ####
dtrip = d1 %>%
  .[trip_id != 0,] %>%
  setkey(driver, ping_time) %>% 
  .[,.(start_time = ping_time[1], end_time = ping_time[.N]),
    .(driver, shift_id, trip_id)] %>% 
  .[,trip_time := as.integer(difftime(end_time, start_time,
                                      units = "mins"))] %>%
  .[order(driver, trip_id)]
fst::write_fst(dtrip, "data/cleaned/12dtrip.fst", compress = 100)

##########################
####      shift       ####
dshift = d1 %>%
  .[,.(start_time = ping_time[1], end_time = ping_time[.N]),
    .(driver, shift_id)] %>%
  .[,shift_time := as.integer(difftime(end_time, start_time, 
                                       units = "mins"))] %>%
  .[order(driver, shift_id)]
fst::write_fst(dshift, "data/cleaned/13dshift.fst", compress = 100)

#############################
####     intervals       ####
dtrip = fst::read_fst("data/cleaned/12dtrip.fst") %>% as.data.table()
dshift = fst::read_fst("data/cleaned/13dshift.fst") %>% as.data.table()

source("data/function_mkint.R")
agg_int30 = mkint(30)
agg_int60 = mkint(60)

# add critical events
ce = fst::read_fst("data/cleaned/04safety_critical_events.fst") %>% as.data.table()
source("data/function_indexce.R")
ceindexed30 = indexce(agg_int30, ce)
ceindexed60 = indexce(agg_int60, ce)
fst::write_fst(ceindexed30, "data/cleaned/21CE_indexed30.fst", compress = 100)
fst::write_fst(ceindexed60, "data/cleaned/22CE_indexed60.fst", compress = 100)


ce_int30 = agg_int30 %>% 
  merge(ceindexed30[,.(nCE = .N), .(driver, interval_id)],
        by = c("driver", "interval_id"), all.x = TRUE) %>% 
  .[,nCE := fifelse(is.na(nCE), 0, nCE)]
ce_int60 = agg_int60 %>% 
  merge(ceindexed60[,.(nCE = .N), .(driver, interval_id)],
        by = c("driver", "interval_id"), all.x = TRUE) %>% 
  .[,nCE := fifelse(is.na(nCE), 0, nCE)]

fst::write_fst(ce_int30, "data/cleaned/31interval30_CE.fst", compress = 100)
fst::write_fst(ce_int60, "data/cleaned/32interval60_CE.fst", compress = 100)