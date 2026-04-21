library(tidyverse)
library(furrr)
library(progressr)
library(StrathE2E2)
library(ggpattern)

# load in data 
# using the big combined file of everything for now whilst the no of columns is small

region_name <- "Brazilian_Shelf_MA"
data <- readRDS(paste0("./Objects/7.All_data_species_narrowed_", region_name, ".rds"))


# how many days until the diff reaches 50% of it's peak anomoly?
# (if the scenario never reaches, it maxes out to a recovery time of 3240 (3600 sim days - 360 year 1))

half_life <- data %>%  
  filter(col_type == "diff", 
         time >= hw_start.x + 360) %>%  # time from hw inception
  group_by(region, hw_month, hw_temp, guild_group) %>% 
  summarise(
    hw_start_abs = first(hw_start.x) + 360,        # store the hw start time
    peak_value = Values[which.max(abs(Values))],   # find the peak anomoly value (+ or -)
    time_of_peak = time[which.max(abs(Values))],   # the day of peak anomoly
    half_peak = peak_value / 2,                    # calc what half of that peak is
    half_life_days = {                             # now find half life....
      if(peak_value > 0) {
        days_below_half <- time[Values <= half_peak & time > time_of_peak] # if peak anomaly is positive, find the first day it drops below the half peak
      }
      else {
        days_below_half <- time[Values >= half_peak & time > time_of_peak]  # if peak anomoly is negative, find the first day where values recover back up to half of what the drop was 
      }
      if(length(days_below_half) > 0) min(days_below_half) - hw_start_abs # take the first day that meets the condition
      else NA # if no day meets the condition (effect never halves) then return NA
    },
    .groups = "drop") %>% 
  mutate(half_life_days = ifelse(is.na(half_life_days), 3240, half_life_days)) # this means that if it never reaches half... max out at 3240

# plotting

p1 <- half_life %>%
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds"))) %>%
  ggplot(aes(x = factor(hw_month), y = factor(hw_temp), fill = half_life_days)) +
  facet_wrap(vars(guild_group)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Half life (days)") +
  scale_x_discrete(labels = month.abb[1:12]) +
  labs(x = "Heatwave month", y = "Heatwave temp (°C)",
       title = "Half life of heatwave effect") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1

# save out with a naming convention that'll dynamicaly change depending on parameters selected

ggsave(paste0(region_name, "_half_life_abs_Plot", "_all_guilds_",".png"), 
       plot = p1, dpi = 300, width = 12, height = 8)


# scaled 

p2 <- half_life %>%
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds"))) %>%
  group_by(guild_group) %>%
  mutate(scaled_val = half_life_days / max(abs(half_life_days), na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = factor(hw_month), y = factor(hw_temp), fill = scaled_val)) +
  facet_wrap(vars(guild_group)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Relative\nhalf life", option = "plasma") +
  scale_x_discrete(labels = month.abb[1:12]) +
  labs(x = "Heatwave month", y = "Heatwave temp (°C)",
       title = "Half life of heatwave effect") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2

# save out with a naming convention that'll dynamicaly change depending on parameters selected

ggsave(paste0(region_name, "_half_life_scaled_Plot", "_all_guilds_",".png"), 
       plot = p2, dpi = 300, width = 12, height = 8)
