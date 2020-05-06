pacman::p_load(data.table, dplyr, lubridate)
d1 = fread("data/ALL_CURRENT_DRIVERS_DATA.csv") %>%
  .[,`:=`(EMPLID = stringr::str_replace_all(EMPLID, " ", ""),
          CURRENT_HIRE_DATE = mdy(CURRENT_HIRE_DATE),
          BIRTHDATE = mdy(BIRTHDATE))] %>%
  .[,`:=`(Date_diff = abs(as.integer(difftime(CURRENT_HIRE_DATE, ymd("20150330"),
                                              units = "days"))))] %>%
  .[is.na(Date_diff), Date_diff := 0] %>%
  .[,.(EMPLID, BIRTHDATE, GENDER, ETHNIC_GROUP, YEARS_OF_EXP,
       BUSINESS_UNIT, DRIVER_TYPE, STATUS, Date_diff)]

d2 = fread("data/ALL_DRIVERS_DATA2016-09-30 10-53-42.csv") %>%
  .[,`:=`(EMPLID = stringr::str_replace_all(EMPLID, " ", ""),
          CURRENT_HIRE_DATE = ymd(CURRENT_HIRE_DATE),
          TERM_DATE = ymd(TERM_DATE),
          BIRTHDATE = ymd(BIRTHDATE))] %>%
  .[,`:=`(Date_diff = abs(as.integer(difftime(TERM_DATE, ymd("20150330"),
                                              units = "days"))))] %>%
  .[is.na(Date_diff), Date_diff := 0] %>%
  .[,.(EMPLID, BIRTHDATE, GENDER, ETHNIC_GROUP, YEARS_OF_EXP,
       BUSINESS_UNIT, DRIVER_TYPE, STATUS, Date_diff)]

d3 = fread("data/ALPHA_TO_EMPLID2016-10-21 14-00-24.csv") %>%
  .[,ALPHA := tolower(ALPHA)] %>%
  .[,`:=`(ALPHA = stringr::str_replace_all(ALPHA, " ", ""),
          EMPLID = stringr::str_replace_all(EMPLID, " ", ""))]

dall = rbindlist(list(d1, d2)) %>%
  merge(., d3, by = "EMPLID", all.x = T) %>%
  .[ALPHA != ''] %>%
  .[order(ALPHA, Date_diff)] %>%
  .[,.SD[1], ALPHA] %>%
  .[,.(ALPHA, AGE = 2015L - as.integer(substr(BIRTHDATE, 1, 4)),
       GENDER, ETHNIC_GROUP, BUSINESS_UNIT, DRIVER_TYPE)] %>%
  .[order(ALPHA)]
fwrite(dall, "data/all_driver_cleaned.csv")




unk = fread("data/original_ping_analysis_Miao_driver_ping_list.csv")
z = merge(unk, dall, by.x = "LOGINDRIVER1", by.y = "ALPHA", all.x = TRUE)
z[,sum(is.na(BUSINESS_UNIT))]


24201405/36499610
0.0001*24201405
0.0001*808713908

35255 - 2925
