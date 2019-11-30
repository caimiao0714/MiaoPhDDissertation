pacman::p_load(data.table, dplyr, lubridate, fst)
d = fst::read_fst("data/cleaned/01a_ping_original_500drivers.fst") %>% 
  as.data.table()

## PART 1: mark all ping with shift_id+
threshold_shift = 8*60
s1 = d %>% 
  .[diff >= threshold_shift, speed := 0] %>% 
  .[,rleid := rleid(speed != 0), driver] %>%
  .[,`:=`(rleid1 = rleid, speed1 = speed)] %>%
  .[,`:=`(sum_speed = sum(speed), sum_time = sum(diff)), .(driver, rleid)] %>%
  .[sum_speed == 0 & sum_time < threshold_shift, speed1 := 3] %>% 
  .[,`:=`(sum_speed = sum(speed1)), .(driver, rleid)] %>%
  .[,shift_id := data.table::fifelse(sum_speed == 0, 0, rleid(speed1 != 0)), driver] 

s1[shift_id == 0,.N] # 3,116,505 -> shift_id == 0
s1[,sum(shift_id == 0)/.N] # 23.633% 

s2 = s1 %>% 
  .[shift_id != 0] %>% 
  .[,shift_length := sum(diff), .(driver, shift_id)] %>% 
  .[,.(driver, ping_time, speed, lat, lon, shift_id, shift_length)] %>% 
  .[shift_length <= 14*60 & shift_length >= 30] %>% 
  .[,shift_length := NULL] %>% 
  setkey(driver, ping_time)

d[,.N] - 3116505 - s2[,.N] # 900395 -> ineligible pings
(d[,.N] - 3116505 - s2[,.N])/d[,.N] # 6.827752%
s2[,.N,.(driver, shift_id)][,.N] # 72665 shifts

fst::write_fst(s2, "data/cleaned/01b_ping_shift_id_500drivers.fst")





########################################
## PART 2: mark all ping with trip_id ##
threshold_trip = 30
t1 = s2 %>% 
  setkey(driver, ping_time) %>% 
  .[,diff := as.integer(difftime(ping_time, shift(ping_time, 
               type = "lag",fill = 0), units = "mins")), driver] %>%
  .[,diff := {diff[1] = 0L; diff}, driver] %>% 
  .[diff >= threshold_trip, speed := 0] %>% 
  .[,rleid := rleid(speed != 0), driver] %>%
  .[,`:=`(rleid1 = rleid, speed1 = speed)] %>%
  .[,`:=`(sum_speed = sum(speed), sum_time = sum(diff)), .(driver, rleid)] %>%
  .[sum_speed == 0 & sum_time < threshold_trip, speed1 := 3] %>% 
  .[,`:=`(sum_speed = sum(speed1)), .(driver, rleid)] %>%
  .[,trip_id := data.table::fifelse(sum_speed == 0, 0, 
              rleid(speed1 != 0)), driver] %>% 
  .[trip_id != 0,trip_length := sum(diff), .(driver, trip_id)] %>% 
  .[,.(driver, ping_time, speed, lat, lon, diff, shift_id, trip_id)]

t2 = t1 %>% 
  .[,`:=`(lon1 = shift(lon, type = "lag", fill = NA),
          lat1 = shift(lat, type = "lag", fill = NA)),
    by = .(driver, shift_id, trip_id)] %>%
  .[,distance := geosphere::distHaversine(cbind(lon, lat), cbind(lon1, lat1))] %>%
  .[,distance := round(distance/1609.344, 3)] %>%
  .[,distance := {distance[1] = 0; distance}, .(driver, shift_id, trip_id)] %>%
  .[,c("lon1", "lat1") := NULL] %>%
  setkey(driver, ping_time)

fst::write_fst(t2, "data/cleaned/01c_ping_trip_id_500drivers.fst")
    



### Aggregate data
# mark pings with trip_id and shift_id
# source("data/function_segment.R")
# d1 = d %>% 
#   segment(speed, diff, driver, "trip_id", 30) %>% 
#   segment(speed, diff, driver, "shift_id", 8*60) %>% 
#   .[,trip_id := data.table::fifelse(shift_id == 0, 0, trip_id)] %>% 
#   .[,`:=`(lon1 = shift(lon, type = "lag", fill = NA),
#           lat1 = shift(lat, type = "lag", fill = NA)), 
#     by = c("driver", "trip_id")] %>% 
#   .[,distance := geosphere::distHaversine(cbind(lon, lat), cbind(lon1, lat1))] %>% 
#   .[,distance := round(distance/1609.344, 3)] %>% 
#   .[,c("lon1", "lat1", "diff", "DATIME", "LATITUDE", "LONGITUDE") := NULL] %>% 
#   setkey(driver, ping_time)
# fst::write_fst(d1, "data/cleaned/11ping_add_trip_shift_id.fst", compress = 100)