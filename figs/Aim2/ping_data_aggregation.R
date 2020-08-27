pacman::p_load(data.table, dplyr, ggplot2, lubridate)
d = fread("data/plot_ping_trip_agg.csv") %>% # path specify: ./data/
  .[,ping_time := ymd_hms(ping_time)]

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


d_shift = d %>%
  .[,diff := as.integer(difftime(ping_time, shift(ping_time, type = "lag", fill = 0),
                                 units = "mins")), driver] %>%
  .[,diff := {diff[1] = 0L; diff}, driver] %>%
  .[,shift_id := segment_0(speed = speed, threshold = 8*60, time_diff = diff), driver] %>%
  .[shift_id != 0,] %>%
  .[,.(start_time = ping_time[1], end_time = ping_time[.N]), .(driver, shift_id)] %>%
  .[,shift_length := as.numeric(difftime(end_time, start_time, units = "mins"))] %>%
  .[,shift_median := start_time + 60*shift_length/2]

d_trip = d %>%
  .[,diff := as.integer(difftime(ping_time, shift(ping_time, type = "lag", fill = 0),
                                 units = "mins")), driver] %>%
  .[,diff := {diff[1] = 0L; diff}, driver] %>%
  .[,trip_id := segment_0(speed = speed, threshold = 30, time_diff = diff), driver] %>%
  .[,shift_id := segment_0(speed = speed, threshold = 8*60, time_diff = diff), driver] %>%
  .[trip_id != 0,] %>%
  .[,.(start_time = ping_time[1], end_time = ping_time[.N]), .(driver, shift_id, trip_id)] %>%
  .[,trip_length := as.numeric(difftime(end_time, start_time, units = "mins"))] %>%
  .[,trip_median := start_time + 60*trip_length/2]


d_30int = d_trip %>%
  .[,trip_time := as.integer(difftime(end_time, start_time, units = "mins"))] %>%
  .[,trip_units := ceiling(trip_time/30)] %>%
  .[rep(seq(.N), trip_units), !c("trip_time", "trip_units")] %>%
  .[,add1 := 0:(.N-1), by = c("driver", "trip_id")] %>%
  .[,start_time := start_time[1] + add1*30*60, .(driver, trip_id)] %>%
  .[,`:=`(end_time = start_time + 30*60, temp_end = end_time), .(driver, trip_id)] %>%
  .[,end_time := {end_time[.N] <- temp_end[.N]; end_time}, .(driver, trip_id)] %>%
  .[,c("add1", "temp_end") := NULL] %>%
  .[, trip_time := as.integer(difftime(end_time, start_time, units = "mins"))] %>%
  setkey(driver, start_time, end_time) %>%
  .[, interval_id := .I] %>%
  .[, .(driver, shift_id, trip_id, interval_id, start_time, end_time, trip_time)]

d_plot_int30 = d_30int %>%
  select(trip_time = end_time) %>%
  bind_rows(select(d_30int, trip_time = start_time)) %>%
  arrange(trip_time)


p1 = d %>%
  .[,point_color := case_when(speed >= 50 ~ ">= 50 MPH",
                              speed >= 25 & speed < 50 ~ "(25, 50] MPH",
                              speed >   0 & speed < 25 ~ "(0, 25] MPH",
                              speed == 0 ~ "0 MPH")] %>%
  .[,point_color := factor(point_color, levels =
                             c("0 MPH", "(0, 25] MPH", "(25, 50] MPH", ">= 50 MPH"))] %>%
  ggplot(aes(ping_time, speed)) +
  geom_point(aes(color = point_color), size = 2) +
  scale_colour_manual(name = "speed category",
                      values = c("#636363", "#31a354", "#fb6a4a", "#a50f15")) +
  geom_line() +
  theme_bw()  +
  # shift
  geom_segment(data = d_shift, aes(x = start_time, xend = end_time,
                                   y = -3.2, yend = -3.2),
               arrow = arrow(length = unit(0.3, "cm")),
               lineend = 'butt', size = 1, color = "#0571b0") +
  geom_text(data = d_shift, aes(x = shift_median, y = -4.8,
                                label = "Shift"), color = "#0571b0", size = 5) +
  # trip
  geom_segment(data = d_trip, aes(x = start_time, xend = end_time,
                                  y = -9, yend = -9),
               arrow = arrow(length = unit(0.3, "cm")),
               lineend = 'butt', size = 1, color = "#7b3294") +
  geom_text(data = d_trip, aes(x = trip_median,
                               y = rep(-10.8, nrow(d_trip)),
                               label = paste(c("Trip", "Trip", "Trip", "Trip", "Trip", "Trip"),
                                             1:nrow(d_trip), " ")),
            color = "#7b3294", size = 4.5) +
  # 30 minute intervals
  geom_segment(data = d_trip, aes(x = start_time, xend = end_time,
                                  y = -15, yend = -15),
               arrow = arrow(length = unit(0.3, "cm")),
               lineend = 'butt', size = 0.8, color = "#008837", alpha = 0.4) +
  geom_point(data = d_plot_int30, aes(x = trip_time, y = -15),
             size = 2, color = "#008837", shape = 124) +
  geom_text(data = d_trip, aes(x = trip_median[1],
                               y = -18,
                               label = "30-minute intervals ..."),
            color = "#008837", size = 5) +
  labs(x = "date and time of ping", y = "ping speed (MPH)") +
  scale_y_continuous(breaks = c(0, 25, 50), limits = c(-18, 70)) +
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.justification = c(1, 1), legend.position = c(0.97, 1),
        legend.background = element_rect(fill=alpha('white', 0.1)),
        legend.direction="horizontal", text=element_text(family="Times"),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        axis.text=element_text(size=16, family="Times"),
        axis.title=element_text(size=20, family="Times"))
p1

ggsave("figures/ping_data_aggregation.pdf", p1,
       width = 10, height = 6.18)
