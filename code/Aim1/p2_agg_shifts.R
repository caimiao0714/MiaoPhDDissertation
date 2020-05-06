pacman::p_load(data.table, dplyr, lubridate)
setDTthreads(parallel::detectCores())
d = data.table::fread("data/20190908_all_trips.csv") %>% 
  .[,`:=`(start_time = ymd_hms(start_time),
          end_time = ymd_hms(end_time))]

get_minutes <- function(start_time, end_time) {
  mins_in_range <- seq(start_time, end_time, by = "mins")
  h_between <- hour(mins_in_range)
  
  hours_day <- mins_in_range[h_between >= 6 &
                               h_between < 18]
  hours_night <- mins_in_range[h_between < 6 |
                                 h_between >= 18]
  minutes_day <- tryCatch(as.numeric(difftime(max(hours_day),
                                              min(hours_day),
                                              units = "mins")),
                          warning = function(w) {0})
  
  minutes_night <- tryCatch(as.numeric(difftime(max(hours_night),
                                                min(hours_night),
                                                units = "mins")),
                            warning = function(w) {0})
  return(list(minutes_day = minutes_day, 
              minutes_night = minutes_night))
}

start_time = Sys.time()
zz = d %>%
#  .[1:1000] %>% 
  rowwise() %>%
  mutate(temp = list(get_minutes(start_time, end_time))) %>%
  cbind(data.table::rbindlist(.$temp)) %>%
  select(-temp)
Sys.time() - start_time
fwrite(zz, "data/20190908_all_trips_night_drive.csv")
#  10,000  obs: 5.988038 secs
# 100,000 obs: 53.03819 secs
