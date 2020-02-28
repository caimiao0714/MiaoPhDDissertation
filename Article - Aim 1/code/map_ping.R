# remotes::install_github("wilkelab/ggtext", force = TRUE)
pacman::p_load(dplyr, sf, data.table, extrafont, ggtext, ggplot2)

#font_import(pattern="Arial")y
source("functions/theme_map.R")
extrafont::fonts()
windowsFonts(LMRoman = windowsFont("LM Roman 10")) # For windows computer
default_font_family = "LMRoman"

crs_use = "+proj=laea +lat_0=35 +lon_0=-100"

d = data.table::fread("data/plot_ping_all.csv") # read ping
us_road = sf::st_read("map_data/roadtrl010g.shp_nt00920/roadtrl010g.shp") %>%
  filter(!(STATE %in% c("AK", "HI", "PR", "VI"))) %>%
  st_transform(crs = crs_use) # read major highway
us_border = sf::st_read("map_data/tl_2017_us_state/tl_2017_us_state.shp") %>%
  filter(!(STUSPS %in% c("AK", "HI", "PR", "VI", "MP", "GU", "AS")))  %>%
  st_transform(crs = crs_use)# read state borders


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
  dstop = dstop %>%
    .[,ping_quart := factor(ping_quart, levels = correct_levels)] %>%
    st_as_sf(coords = c("V6", "V4"), crs = 4326) %>%
    st_transform(crs = crs_use)

  p = ggplot() +
    geom_sf(data = us_border, fill = NA, color = "grey10", size = 0.3) +
    geom_sf(data = us_road,
            fill = "white", color = "grey0", size = 0.1) +
    geom_sf(data = dstop,
               aes(fill = ping_quart, color = ping_quart),
               shape = 21, alpha = 0.4, size = 0.2) +
    scale_color_manual(values = c('#9ecae1','#6baed6','#4292c6','#2171b5','#084594'),
                       guide = FALSE) +
    scale_fill_manual(values = c('#9ecae1','#6baed6','#4292c6','#2171b5','#084594')) +
    coord_sf(crs = st_crs(crs_use)) +
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
ggsave("figs/projected_map_stop_1.png", p, width = 10, height = 6.18, dpi = 600)

p = plot_stop(10)
ggsave("figs/projected_map_stop_10.png", p, width = 10, height = 6.18, dpi = 600)

p = plot_stop(50)
ggsave("figs/projected_map_stop_50.png", p, width = 10, height = 6.18, dpi = 600)

p = plot_stop(100)
ggsave("figs/projected_map_stop_100.png", p, width = 10, height = 6.18, dpi = 600)




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
  dactive =  dactive %>%
    .[,ping_quart := factor(ping_quart, levels = correct_levels)] %>%
    st_as_sf(coords = c("V6", "V4"), crs = 4326) %>%
    st_transform(crs = crs_use)

  p = ggplot() +
    geom_sf(data = us_border, fill = NA, color = "grey10", size = 0.3) +
    geom_sf(data = us_road,
            fill = "white", color = "grey0", size = 0.1) +
    geom_sf(data = dactive,
            aes(fill = ping_quart, color = ping_quart),
            shape = 21, alpha = 0.4, size = 0.2) +
    scale_color_manual(values = c('#fc9272','#fb6a4a','#ef3b2c','#cb181d','#99000d'),
                       guide = FALSE) +
    scale_fill_manual(values = c('#fc9272','#fb6a4a','#ef3b2c','#cb181d','#99000d')) +
    coord_sf(crs = st_crs(crs_use)) +
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
ggsave("figs/projected_map_active_1.png", p, width = 10, height = 6.18, dpi = 600)

p = plot_active(10)
ggsave("figs/projected_map_active_10.png", p, width = 10, height = 6.18, dpi = 600)

p = plot_active(50)
ggsave("figs/projected_map_active_50.png", p, width = 10, height = 6.18, dpi = 600)

p = plot_active(100)
ggsave("figs/projected_map_active_100.png", p, width = 10, height = 6.18, dpi = 600)

beepr::beep()
