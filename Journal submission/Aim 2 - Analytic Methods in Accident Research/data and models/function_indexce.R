indexce = function(data, ce, cumdrive_threshold = 14*60){
  tokeep = data %>% 
    .[,.(maxdrive = max(cumdrive)), .(driver, shift_id)] %>% 
    .[maxdrive <= cumdrive_threshold]
  
  ce_tab = data %>% 
    merge(tokeep, by = c("driver", "shift_id")) %>% 
    .[,.(driver, interval_id, start_time, end_time, cumdrive)] %>% 
    setkey(driver, start_time, end_time) 
  
  ce1 = ce %>% 
    .[,dummy := event_time] %>% 
    setkey(driver, event_time, dummy) %>% 
    foverlaps(ce_tab, mult = "all", type = "within", nomatch = NA) %>% 
    .[!is.na(interval_id),] %>% 
    .[,.(driver, interval_id, event_time, event_type)]
  
  return(ce1)
}


