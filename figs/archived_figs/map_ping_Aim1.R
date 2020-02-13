pacman::p_load(dplyr, ggplot2, sf, data.table)
source("functions/theme_map.R")
# library(extrafont);font_import(pattern="Arial")y
default_font_family = "LM Roman 10"

d = data.table::fread("data/plot_ping_all.csv") # read ping
us_road = sf::st_read("map_data/roadtrl010g.shp_nt00920/roadtrl010g.shp") %>%
  filter(!(STATE %in% c("AK", "HI", "PR", "VI"))) # read major highway
us_border = sf::st_read("map_data/tl_2017_us_state/tl_2017_us_state.shp") %>%
  filter(!(STUSPS %in% c("AK", "HI", "PR", "VI", "MP", "GU", "AS"))) # read state borders

 
#### STOP pings ###
plot_stop = function(n_ping_threshold = 100){
  maxping = d[status == "stop", max(n_ping)]
  dstop = d %>%
    .[n_ping >= n_ping_threshold &
        status == "stop" &
        V4 < 50 & V4 > 23 &
        V6 < -65,] %>%
    .[,ping_quart := Hmisc::cut2(n_ping, g = 5)] %>%
    .[,ping_quart := sapply(stringr::str_extract_all(ping_quart, "\\d+"),
                            function(x) paste(x, collapse="-"))] %>%
    .[,ping_quart := case_when(
      grepl(maxping, ping_quart) ~ paste0(">=", gsub(paste0("-", maxping),
                                                     "", ping_quart)),
      TRUE ~ ping_quart)]

  correct_levels = dstop[,.N, ping_quart] %>%
    .[,ping_quart] %>%
    .[order(as.integer(gsub("(-\\d+)|(>=)", "", .)))]
  dstop[,ping_quart := factor(ping_quart, levels = correct_levels)]

  p = ggplot() +
    geom_sf(data = us_border, fill = NA, color = "grey5", size = 0.2) +
    geom_sf(data = us_road,
            fill = "white", color = "grey0", size = 0.1) + # , color = "grey"
    geom_point(data = dstop,
               aes(x=V6, y=V4, fill = ping_quart, color = ping_quart),
               shape = 21, alpha = 0.4, size = 0.2) +
    scale_color_manual(values = c('#eff3ff','#bdd7e7','#6baed6','#3182bd','#08519c'),
                       guide = FALSE) +
    scale_fill_manual(values = c('#eff3ff','#bdd7e7','#6baed6','#3182bd','#08519c')) +
    coord_sf(xlim = c(-123, -68), ylim = c(24, 50)) +
    guides(fill = guide_legend(title = "Number of pings",
                               override.aes = list(alpha = 1, size = 2))) +
    theme_map(legend.justification = c(1, 1),
              legend.position = c(0.2, 0.25),
              legend.direction = "vertical",
              legend.spacing.x = unit(0.5, 'cm'),
              legend.spacing.y = unit(0.2, 'cm'),
              legend.key.size = unit(0.8, "lines")
    ) +
    labs(x = NULL, y = NULL,
         title = "Geographical distribution of stopped pings",
         subtitle = "A large commercial truck NDS data set in USA, 2015-2016",
         caption = paste("The grey line are major highways in the USA. Only locations with at least", n_ping_threshold, "pings were shown."))
  return(p)
}
p = plot_stop(1)
ggsave("figs/map_stop_grey_1.png", p, width = 10, height = 6.18, dpi = 300)

p = plot_stop(10)
ggsave("figs/map_stop_grey_10.png", p, width = 10, height = 6.18, dpi = 300)

p = plot_stop(50)
ggsave("figs/map_stop_grey_50.png", p, width = 10, height = 6.18, dpi = 300)


p = plot_stop(100)
ggsave("figs/map_stop_grey_100.png", p, width = 10, height = 6.18, dpi = 300)






#### ACTIVE pings ###
plot_active = function(n_ping_threshold = 100){
  maxping = d[status == "active", max(n_ping)]
  dactive = d %>%
    .[n_ping >= n_ping_threshold &
        status == "active" &
        V4 < 50 & V4 > 23 &
        V6 < -65,] %>%
    .[,ping_quart := Hmisc::cut2(n_ping, g = 5)] %>%
    .[,ping_quart := sapply(stringr::str_extract_all(ping_quart, "\\d+"),
                            function(x) paste(x, collapse="-"))] %>%
    .[,ping_quart := case_when(
      grepl(maxping, ping_quart) ~ paste0(">=", gsub(paste0("-", maxping),
                                                     "", ping_quart)),
      TRUE ~ ping_quart)]

  correct_levels = dactive[,.N, ping_quart] %>%
    .[,ping_quart] %>%
    .[order(as.integer(gsub("(-\\d+)|(>=)", "", .)))]
  dactive[,ping_quart := factor(ping_quart, levels = correct_levels)]

  p = ggplot() +
    geom_sf(data = us_border, fill = NA, color = "grey5", size = 0.2) +
    geom_sf(data = us_road,
            fill = "white", color = "grey0", size = 0.1) + # , color = "grey"
    geom_point(data = dactive,
               aes(x=V6, y=V4, fill = ping_quart, color = ping_quart),
               shape = 21, alpha = 0.4, size = 0.1) +
    scale_color_manual(values = c('#fee5d9','#fcae91','#fb6a4a','#de2d26','#a50f15'),
                       guide = FALSE) +
    scale_fill_manual(values = c('#fee5d9','#fcae91','#fb6a4a','#de2d26','#a50f15')) +
    coord_sf(xlim = c(-123, -68), ylim = c(24, 50)) +
    guides(fill = guide_legend(title = "Number of pings",
                               override.aes = list(alpha = 1, size = 2))) +
    theme_map(legend.justification = c(1, 1),
              legend.position = c(0.2, 0.25),
              legend.direction = "vertical",
              legend.spacing.x = unit(0.5, 'cm'),
              legend.spacing.y = unit(0.2, 'cm'),
              legend.key.size = unit(0.8, "lines")
    ) +
    labs(x = NULL, y = NULL,
         title = "Geographical distribution of active moving pings",
         subtitle = "A large commercial truck NDS data set in USA, 2015-2016",
         caption = paste("The grey line are major highways in the USA. Only locations with at least", n_ping_threshold, "pings were shown."))
  return(p)
}

p = plot_active(1)
ggsave("figs/map_active_grey_1.png", p, width = 10, height = 6.18, dpi = 300)

p = plot_active(10)
ggsave("figs/map_active_grey_10.png", p, width = 10, height = 6.18, dpi = 300)

p = plot_active(50)
ggsave("figs/map_active_grey_50.png", p, width = 10, height = 6.18, dpi = 300)

p = plot_active(100)
ggsave("figs/map_active_grey_100.png", p, width = 10, height = 6.18, dpi = 300)
