pacman::p_load(data.table, parallel, benchmarkme, vroom)
n_cores = parallel::detectCores()

for(i in 1:77){
  k = 2*(i-1)*10^7
  print(paste(i, k, k + 2*10^7, sep = ":"))
  al2 = fread("data/test_shortend_version_data.csv", 
              nrows = 2*10^7, skip = k, showProgress = TRUE,
              quote = "", header = FALSE, nThread = n_cores)
  d2 = al2[,.N, V7][order(N)]
  fwrite(d2, paste0("data/driverlist/d", i, ".csv"))
}

## Read all .csv in a folder
pacman::p_load(dplyr, purrr, fs, data.table)
data_dir = "data/driverlist/"
dr = data_dir %>% 
  fs::dir_ls(regexp = "\\.csv$") %>% 
  purrr::map_dfr(fread, .id = "source") %>% 
  set_names(c("source", "driver", "n_ping")) %>% 
  .[,driver := gsub("\"", "", driver)] %>% 
  .[,driver := gsub(" ", "", driver)] %>% 
  .[,.(n_ping = sum(n_ping)), driver] %>% 
  .[!(driver %in% c("unavailable", "")),]

# driver information
alldr = fread("data/ALL_DRIVERS_DATA2016-09-30 10-53-42.csv") %>% 
  .[,EMPLID := stringr::str_replace_all(EMPLID, " ", "")]

# alpha to employee ID
alpha = fread("data/ALPHA_TO_EMPLID2016-10-21 14-00-24.csv") %>% 
  .[,driver := tolower(ALPHA)] %>% 
  .[,`:=`(driver = stringr::str_replace_all(driver, " ", ""),
          EMPLID = stringr::str_replace_all(EMPLID, " ", ""))] %>% 
  .[,.(driver, EMPLID)] %>% 
  setkey(driver)

d = alpha %>% 
  .[dr, on = "driver"] %>% 
  setkey(EMPLID)# there are duplicated EMPLID

all_drivers = alldr %>% 
  .[d, on = "EMPLID"] %>% 
  .[,BIRTHDATE := lubridate::ymd(BIRTHDATE)] %>% 
  .[!is.na(BIRTHDATE),] %>% 
  setkey(driver) %>% 
  .[,n_missing := rowSums(is.na(.))] %>% 
  .[order(driver, -n_missing)] %>% 
  .[,head(.SD, 1), by = driver] %>% 
  .[, age := 2015 - year(BIRTHDATE)] %>% 
  .[,.(driver, age, EMPLID, OPR_TYPE, GENDER, ETHNIC_GROUP, 
       YEARS_OF_EXP, BUSINESS_UNIT, DRIVER_TYPE, n_ping)] %>% 
  .[n_ping > 100,] %>% 
  setkey(DRIVER_TYPE, n_ping, driver)

fwrite(all_drivers, "data/all_drivers.csv")

# Create a loop group for each driver
driver_group = all_drivers %>% 
  .[,.(DRIVER_TYPE, driver, n_ping)] %>% 
  .[,cum_ping := cumsum(n_ping), DRIVER_TYPE] %>% 
  .[,gr := floor(cum_ping/(1.9*10^7))] %>% 
  .[,d_group := paste(DRIVER_TYPE, gr, sep = "_")] %>% 
  .[,.(V7 = driver, d_group)]


#### Start to get drivers ####
rm_quote = function(var){
  return(gsub("\"| ", "", var))
}

# Loop for each driver group
n_cores = parallel::detectCores()
for (i in 1:76) {#75
  k = 2*(i-1)*10^7
  print(paste(i, i/75, k, k + 2*10^7, sep = " "))
  al2 = fread("data/test_shortend_version_data.csv", 
              nrows = 2*10^7, skip = k, showProgress = TRUE,
              quote = "", header = FALSE, nThread = n_cores) %>% 
    .[, `:=`(V7 = rm_quote(V7), V1 = rm_quote(V1),
             V2 = gsub("\"", "", V2), V3 = rm_quote(V3),
             V4 = rm_quote(V4), V5 = rm_quote(V5),
             V6 = rm_quote(V6), V8 = rm_quote(V8),
             V9 = rm_quote(V9))] %>% 
    merge(driver_group, by = 'V7', all.x = TRUE) %>% 
    .[!is.na(d_group),] %>% 
    .[,aux := d_group]
  al2[, fwrite(.SD, paste0("data/temp_ping_sub/", .BY, "_", i, ".csv")), 
      by=.(aux)]
}


# aggregate files
pacman::p_load(dplyr, purrr, data.table)
csv_files = list.files(path = "data/temp_ping_sub/", pattern = ".csv")
csv_sub = list.files(path = "data/temp_ping_sub/", pattern = "REG_30_[0-9]+.csv")

csv_group = gsub("[0-9]+.csv$", "", csv_files) %>% 
  tibble::enframe(name = "id", value = "group") %>% 
  count(group)#67 group

for(i in 1:67){
  dt <- csv_group$group[i] %>% 
    list.files(path = "data/temp_ping_sub", pattern = ., full.names=TRUE) %>% 
    purrr::map_df(~fread(., colClasses = c(V1 = "chr")))
  fwrite(dt, paste0("data/agg_ping/", csv_group$group[i], ".csv"))
}

length(list.files(path = "data/agg_ping/", pattern = ".csv"))
