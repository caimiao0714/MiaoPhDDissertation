# list.files(path = "data/agg_ping/", pattern = ".csv")
pacman::p_load(data.table, dplyr, lubridate)
setDTthreads(parallel::detectCores())

segment_0 = function(speed, threshold, time_diff) {
  ## Replace very long single points
  speed[time_diff >= threshold] <- 0
  ## First, replacing stretches of less than "threshold" consecutive 0 speeds by 1s
  r1 = rle(speed != 0)
  r1$values <- replicate(length(r1$values), 1)
  r1$values <- cumsum(r1$values)
  order_tmp <- inverse.rle(r1)
  dat_tmp1 <- data.table::data.table(speed, order_tmp, time_diff)
  dat_tmp2 <- dat_tmp1[,.(sumdiff = sum(time_diff)), by = order_tmp]
  r2 = rle(speed != 0)
  r2$values[r2$values == 0 & dat_tmp2$sumdiff < threshold] <- TRUE
  r2 <- inverse.rle(r2)
  r2 <- rle(r2)
  ## Then numbering consecutive stretches of non-zero values
  r2$values[r2$values] = cumsum(r2$values[r2$values])
  return(inverse.rle(r2))
}

segment_1 = function(data, speed, time_diff, threshold) {
  data$speed[data$time_diff >= threshold] <- 0
  r1 = rle(data$speed != 0)
  r1$values <- replicate(length(r1$values), 1)
  r1$values <- cumsum(r1$values)
  order_tmp <- inverse.rle(r1)
  dat_tmp1 <- data.table::data.table(speed = data$speed, order_tmp = order_tmp, time_diff = data$time_diff)
  dat_tmp2 <- dat_tmp1[,.(sumdiff = sum(time_diff)), by = order_tmp]
  r2 = rle(data$speed != 0)
  r2$values[r2$values == 0 & dat_tmp2$sumdiff < threshold] <- TRUE
  r2 <- inverse.rle(r2)
  r2 <- rle(r2)
  r2$values[r2$values] = cumsum(r2$values[r2$values])
  return(inverse.rle(r2))
}

# aggregate into trips
agg_ping_csv_file = list.files(path = "data/agg_ping", 
                          pattern = ".csv",
                          full.names=TRUE)
agg_ping_csv_name = gsub("^data/agg_ping/|_.csv$", "\\1", 
                         agg_ping_csv_file)
length(agg_ping_csv_name)

for(i in 1:67){
  print(i)
  d2 = agg_ping_csv_file[i] %>%
    fread() %>% 
    .[,V2 := ymd_hms(V2)] %>% 
    .[!is.na(V2),.(driver = V7, dtime = V2, speed = V3, 
                   lat = V4, lon = V6)] %>% 
    setkey(driver, dtime) %>% 
    .[,diff := as.integer(difftime(dtime, 
                                   shift(dtime, type = "lag", 
                                         fill = 0), 
                                   units = "mins")), driver] %>% 
    .[,diff := {diff[1] = 0L; diff}, driver] %>% 
    .[,trip_id := segment_0(speed = speed, 
                            threshold = 30, time_diff = diff),
      driver] %>% 
    .[trip_id != 0,] %>% 
    .[,`:=`(lon1 = shift(lon, type = "lag", fill = NA),
            lat1 = shift(lat, type = "lag", fill = NA)), 
      by = c("driver", "trip_id")] %>% 
    .[,distance := geosphere::distHaversine(cbind(lon, lat), 
                                            cbind(lon1, lat1))] %>%
    .[,distance := round(distance/1609.344, 3)] %>% 
    .[,.(start_time = dtime[1], end_time = dtime[.N],
         start_lat = lat[1], start_lon = lon[1],
         end_lat = lat[.N], end_lon = lon[.N],
         n_ping = .N,
         ping_speed = round(mean(speed, na.rm = TRUE), 3), 
         distance = sum(distance, na.rm = TRUE)),
      .(driver, trip_id)] %>% 
    .[,trip_time := round(as.numeric(difftime(end_time, start_time,
                                              units = "mins")), 3)] %>% 
    .[trip_time >= 3 & distance >= 0.5,]
  fwrite(d2, paste0("data/agg_trips/trip_", agg_ping_csv_name[i], ".csv"))
}


# read all .csv in "data/agg_trips/" folder and combine into one file
combined_trips = list.files(path = "data/agg_trips", 
                            pattern = "*.csv", full.names = TRUE) %>% 
  purrr::map_df(~fread(.))
fwrite(combined_trips, "data/20190908_all_trips.csv")










# EXAMPLE: separate into trips
d = fread("data/agg_ping/LOC_0_.csv") %>% 
  .[1:10000,]
.[,V2 := ymd_hms(V2)] %>% 
  .[!is.na(V2),.(driver = V7, dtime = V2, speed = V3, 
                 lat = V4, lon = V6)] %>% 
  setkey(driver, dtime) %>% 
  .[,diff := as.integer(difftime(dtime, 
                                 shift(dtime, type = "lag", 
                                       fill = 0), 
                                 units = "mins")), driver] %>% 
  .[,diff := {diff[1] = 0L; diff}, driver] %>% 
  .[,trip_id := segment_0(speed = speed, 
                          threshold = 30, time_diff = diff),
    driver] %>% 
  .[trip_id != 0,] %>% 
  .[,`:=`(lon1 = shift(lon, type = "lag", fill = NA),
          lat1 = shift(lat, type = "lag", fill = NA)), 
    by = c("driver", "trip_id")] %>% 
  .[,distance := geosphere::distHaversine(cbind(lon, lat), 
                                          cbind(lon1, lat1))] %>%
  .[,distance := round(distance/1609.344, 3)] %>% 
  .[,.(start_time = dtime[1], end_time = dtime[.N],
       start_lat = lat[1], start_lon = lon[1],
       end_lat = lat[.N], end_lon = lon[.N],
       n_ping = .N,
       ping_speed = round(mean(speed, na.rm = TRUE), 3), 
       distance = sum(distance, na.rm = TRUE)),
    .(driver, trip_id)] %>% 
  .[,trip_time := round(as.numeric(difftime(end_time, start_time,
                                            units = "mins")), 3)] %>% 
  .[trip_time >= 3 & distance >= 0.5,]

