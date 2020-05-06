# remotes::install_github("wilkelab/ggtext")
pacman::p_load(dplyr, data.table, ggplot2, sf, viridis, wesanderson, cowplot, ggthemes, colorspace)

source("figures/theme_map.R")
# extrafont::font_import()
# y
extrafont::fonts()
windowsFonts(Times = windowsFont("Times New Roman")) # For windows computer LM Roman 10
default_font_family = "Times"

crs_use = "+proj=laea +lat_0=35 +lon_0=-100"
#"+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100"
# "+proj=laea +lat_0=30 +lon_0=-95" Lambert Area
d = fst::read_fst("data/cleaned/01a_ping_original_500drivers.fst") %>%
  as.data.table# read ping
us_road = sf::st_read("map_data/roadtrl010g.shp_nt00920/roadtrl010g.shp") %>%
  filter(!(STATE %in% c("AK", "HI", "PR", "VI")))%>%
  st_transform(crs = crs_use) # read major highway
us_border = sf::st_read("map_data/tl_2017_us_state/tl_2017_us_state.shp") %>%
  filter(!(STUSPS %in% c("AK", "HI", "PR", "VI", "MP", "GU", "AS"))) %>%
  st_transform(crs = crs_use)


pd = function(dt){#plot data
  agg_p = dt[,.(lat = round(lat, 2), long = round(lon, 2))]

  maxping = agg_p %>%
    .[,.(n_ping = .N),.(lat, long)] %>%
    .[, max(n_ping)]

  fdt = agg_p %>%
    .[,.(n_ping = .N),.(lat, long)] %>%
    .[,ping_quart := Hmisc::cut2(n_ping, g = 5)] %>%
    .[,ping_quart := sapply(stringr::str_extract_all(ping_quart, "\\d+"),
                            function(x) paste(x, collapse="-"))] %>%
    .[,ping_quart := case_when(
      grepl(maxping, ping_quart) ~ paste0(">=", gsub(paste0("-", maxping),
                                                     "", ping_quart)),
      TRUE ~ ping_quart)]

  correct_levels = fdt[,.N, ping_quart] %>%
    .[,ping_quart] %>%
    .[order(as.integer(gsub("(-\\d+)|(>=)", "", .)))]

  fdt = fdt %>%
    .[,ping_quart := factor(ping_quart, levels = correct_levels)] %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    st_transform(crs = crs_use)

  return(fdt)
}




# ----------------- NO ROAD -----------------------
pnoroad = function(dt, viridis_color = "magma", moving = "moving"){
  z = ggplot() +
    geom_sf(data = us_border, fill = NA, color = "grey5", size = 0.2) +
    geom_sf(data = dt, aes(fill = ping_quart, color = ping_quart),
            shape = 21, alpha = 0.6, size = 0.3) +
    scale_fill_viridis(option = viridis_color, discrete = TRUE, direction = -1) +
    scale_color_viridis(option = viridis_color, discrete = TRUE, direction = -1,
                        guide = FALSE) +
    coord_sf(crs = st_crs(crs_use)) +
    guides(fill = guide_legend(title = "Number of pings",
                               override.aes = list(alpha = 1, size = 5))) +
    labs(x = NULL, y = NULL,
         title = paste0("Geographical distribution of ", moving,
                        " pings by the 497 truck drivers 2015-2016"),
         caption = "This is based on NDS ping data from 497 regional truck drivers in the United States.") +
    theme_map(
      # plot.title = element_markdown(size = 15, hjust = 0.5,
      #           margin = margin(t = 0.1, b = -0., unit = "cm"),
      #           color = default_font_color),
              legend.justification = c(1, 1),
              legend.position = c(0.2, 0.25),
              legend.direction = "vertical",
              legend.spacing.x = unit(0.5, 'cm'),
              legend.spacing.y = unit(0.2, 'cm'),
              legend.key.size = unit(0.8, "lines"))

  return(z)
}

# map active pings
pp = d %>%
  .[speed > 0] %>%
  pd() %>%
  pnoroad(viridis_color = "magma", moving = "moving")
ggsave("figures/Projected_map_active_ping_Aim2_NOROAD.png", pp,
       width = 10, height = 6.18, dpi = 400)

# map inactive pings
stopped_ping_Aim2 = d %>%
  .[speed == 0] %>%
  pd() %>%
  pnoroad(viridis_color = "viridis", moving = "stopped")
ggsave("figures/Projected_map_stopped_ping_Aim2_NOROAD.png", stopped_ping_Aim2,
       width = 10, height = 6.18, dpi = 400)





















































# ---------------- HAS ROAD ------------------
pmap = function(dt, viridis_color = "magma", moving = "moving"){
  z = ggplot() +
    geom_sf(data = us_border, fill = NA, color = "grey5", size = 0.2) +
    geom_sf(data = us_road,
            fill = "white", color = "grey0", size = 0.1) +
    geom_sf(data = dt,
            aes(fill = ping_quart, color = ping_quart),
            shape = 21, alpha = 0.6, size = 0.2) +
    scale_fill_viridis(option = viridis_color, discrete = TRUE, direction = -1) +
    scale_color_viridis(option = viridis_color, discrete = TRUE, direction = -1,
                        guide = FALSE) +
    coord_sf(crs = st_crs(crs_use)) +
    guides(fill = guide_legend(title = "Number of pings",
                               override.aes = list(alpha = 1, size = 2))) +
    labs(x = NULL, y = NULL,
         title = paste0("Geographical distribution of the ",
                        moving,
                        " pings"),
         caption = "This is based on NDS ping data from 497 regional truck drivers in the United States.
         The solid black lines are state borders.") +
    theme_map(
      # plot.title =
      #           ggtext::element_markdown(size = 15, hjust = 0.5,
      #                                    margin = margin(b = 0.3, unit = "cm"),
      #                                    color = default_font_color),
      legend.justification = c(1, 1),
      legend.position = c(0.2, 0.25),
      legend.direction = "vertical",
      legend.spacing.x = unit(0.5, 'cm'),
      legend.spacing.y = unit(0.2, 'cm'),
      legend.key.size = unit(0.8, "lines"))

  return(z)
}


# map active pings
active_ping_Aim2 = d %>%
  .[speed > 0] %>%
  pd() %>%
  pmap(viridis_color = "magma", moving = "moving")
ggsave("figures/Projected_map_active_ping_Aim2.png", active_ping_Aim2,
       width = 10, height = 6.18, dpi = 400)

# map inactive pings
stopped_ping_Aim2 = d %>%
  .[speed == 0] %>%
  pd() %>%
  pmap(viridis_color = "viridis", moving = "stopped")
ggsave("figures/Projected_map_stopped_ping_Aim2.png", stopped_ping_Aim2,
       width = 10, height = 6.18, dpi = 400)


