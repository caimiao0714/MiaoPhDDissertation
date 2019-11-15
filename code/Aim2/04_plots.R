pacman::p_load(data.table, dplyr, ggplot2, fst, ggthemes)

d = fst::read_fst("data/cleaned/31interval30_CE.fst") %>% as.data.table()

tokeep = d %>% 
  .[,.(maxdrive = max(cumdrive)), .(driver, shift_id)] %>% 
  .[maxdrive <= 11*60]

d %>% 
  merge(tokeep, by = c("driver", "shift_id")) %>% 
  .[,cumdrive_hour := ceiling(cumdrive/30)/2] %>% 
  .[,drive_minutes := cumdrive - (cumdrive_hour-0.5)*60] %>% 
  .[,.(nCE = sum(nCE), drive_minutes = sum(drive_minutes)/30), cumdrive_hour] %>% 
  .[,CErate := nCE/drive_minutes] %>%
  .[,`:=`(CI_left = CErate - 1.96*sqrt(CErate*(1 - CErate)/drive_minutes),
          CI_right = CErate + 1.96*sqrt(CErate*(1 - CErate)/drive_minutes))] %>% 
  ggplot(aes(cumdrive_hour, CErate)) + 
  stat_smooth(geom = "line", alpha = 0.4, se = FALSE, color = "blue", size = 1.1) + 
  geom_smooth(color = "blue", fill = "blue", alpha = 0.1, size = 0) + 
  geom_point() + geom_line() + 
  geom_ribbon(aes(ymin = CI_left, ymax = CI_right), alpha = 0.3) + 
  labs(x = "cumulative driving time (hours)", 
       y = "Number of SCEs per 0.5 hour",
       caption = "The black dots shows the empirical rates. 
       The blue curves shows the loess smooth estimates.
       The bands shows 95% confidence intervals using normal approximation.") +
  scale_x_continuous(breaks = 0:11) + ggthemes::geom_rangeframe() +
  ggthemes::theme_tufte(base_family='GillSans')  

  
ggsave("figures/p_11hours.png", width = 10, height = 6.18, dpi = 400)



d %>% 
  .[,cumdrive_hour := ceiling(cumdrive/30)/2] %>% 
  .[,drive_minutes := cumdrive - (cumdrive_hour-0.5)*60] %>% 
  .[,.(nCE = sum(nCE), drive_minutes = sum(drive_minutes)/30), cumdrive_hour] %>% 
  .[,CErate := nCE/drive_minutes] %>%
  .[,`:=`(CI_left = CErate - 1.96*sqrt(CErate*(1 - CErate)/drive_minutes),
          CI_right = CErate + 1.96*sqrt(CErate*(1 - CErate)/drive_minutes))] %>% 
  ggplot(aes(cumdrive_hour, CErate)) + 
  stat_smooth(geom = "line", alpha = 0.4, se = FALSE, color = "blue", size = 1.1) + 
  geom_smooth(color = "blue", fill = "blue", alpha = 0.1, size = 0) + 
  geom_point() + geom_line() + 
  geom_ribbon(aes(ymin = CI_left, ymax = CI_right), alpha = 0.3) + 
  labs(x = "cumulative driving time (hours)", 
       y = "Number of SCEs per 0.5 hour",
       caption = "The black dots shows the empirical rates. 
       The blue curves shows the loess smooth estimates.
       The bands shows 95% confidence intervals using normal approximation.") +
  ggthemes::geom_rangeframe() +
  ggthemes::theme_tufte(base_family='GillSans')  


ggsave("figures/p_unlimited_hours.png", width = 10, height = 6.18, dpi = 400)
