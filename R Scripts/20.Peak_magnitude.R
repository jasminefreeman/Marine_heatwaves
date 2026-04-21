library(tidyverse)
library(furrr)
library(progressr)
library(StrathE2E2)
library(ggpattern)

# load in data 
# using the big combined file of everything for now whilst the no of columns is small

region_name <- "Norwegian_Basin_MA"
data <- readRDS(paste0("./Objects/7.All_data_species_narrowed_", region_name, ".rds"))


# peak magnitude 

# what is the maximum difference between the base run and the experiment? Is it positive or negative?
# this will show the peak effect of the heatwave. 

peak_mag <- data %>% 
  filter(col_type == "diff",
         time >= hw_start.x +360) %>%  # only looks after the hw
  group_by(region, hw_month, hw_temp, guild_group) %>% 
  summarise(
    peak_diff = Values[which.max(abs(Values))],        # largest anomoly
    peak_yearday = yearday[which.max(abs(Values))],      # what day of the year did the peak occur 
    time_of_peak = time[which.max(abs(Values))],      # absolute timestep of peak (within the whole timeseries simulation)
    .groups = "drop")


# plotting 

# heat map absolutes

p1 <- peak_mag %>% 
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds"))) %>% 
  ggplot(aes(x = factor(hw_month), y = factor(hw_temp), fill = peak_diff)) +
  facet_wrap(vars(guild_group)) +
  geom_tile() +
  scale_fill_gradient() +
  scale_fill_gradient2(low = "#9100cd", mid = "white", high = "darkorange", midpoint = 0,
                       name = "Peak anomaly") +
  scale_x_discrete(labels = month.abb[1:12]) +
  labs(x = "Heatwave month", y = "Heatwave temp (°C)",
       title = "Peak magnitude anomaly") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p1

# save out with a naming convention that'll dynamicaly change depending on parameters selected

ggsave(paste0(region_name, "peak_magnitude_abs_Plot", "_all_guilds_",".png"), 
       plot = p1, dpi = 300, width = 12, height = 8)

# omni_zoo and phyto have much larger anomoly values than the other guilds, so the colour scale is being dominated 
# by those two and everything else appears white by comparison. 

# heat map scaled

# scale the other guilds so that they don't just show through as white. By scaling, it normalises each guilds value between 
# +1 and 1 so the colour scale is comparable within each panel. BUT!! the colours are relative, not absolute... a deep red in 
# two seperate panels don't mean the same magnitude, it just means the most extreme positive response for that guild.

p2 <- peak_mag %>% 
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds"))) %>% 
  group_by(guild_group) %>% 
  mutate(peak_diff_scaled = peak_diff / max(abs(peak_diff), na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = factor(hw_month), y = factor(hw_temp), fill = peak_diff_scaled)) +
  facet_wrap(vars(guild_group)) +
  geom_tile() +
  scale_fill_gradient() +
  scale_fill_gradient2(low = "#9100cd", mid = "white", high = "darkorange", midpoint = 0,
                       name = "Peak anomaly") +
  scale_x_discrete(labels = month.abb[1:12]) +
  labs(x = "Heatwave month", y = "Heatwave temp (°C)",
       title = "Peak magnitude anomaly") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p2

# orange = the hw scenario that resulted in the largest positive peak anomaly

# purple = the hw scenario that resulted in the largest negative peak anomoly



#Are there any other ways i could show this that doesn't use a heat map??

# save out with a naming convention that'll dynamicaly change depending on parameters selected

ggsave(paste0(region_name, "peak_magnitude_scaled_Plot", "_all_guilds_",".png"), 
       plot = p2, dpi = 300, width = 12, height = 8)
