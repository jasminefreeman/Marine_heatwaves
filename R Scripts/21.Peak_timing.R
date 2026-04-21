library(tidyverse)
library(furrr)
library(progressr)
library(StrathE2E2)
library(ggpattern)

# load in data 
# using the big combined file of everything for now whilst the no of columns is small

region_name <- "Norwegian_Basin_MA"
data <- readRDS(paste0("./Objects/7.All_data_species_narrowed_", region_name, ".rds"))
selected_year <- 2

# peak timing 

# this compares the day of peak abundance in the base run vs the experiment outcomes - each year or for the whole experiment, whatever the selected_month parameter says 

peak_timing <- data %>% 
  filter(col_type %in% c("experiment", "base"), year %in% (selected_year)) %>% 
  group_by(region, hw_month, hw_temp, year, guild_group, col_type) %>% 
  summarise(peak_time = time[which.max(Values)],
            .groups = "drop") %>% 
  pivot_wider(names_from = col_type,
              values_from = peak_time) %>% 
  mutate(timing_diff = base - experiment) # pos = advanced, neg = delayed



### need to add something in here to see how the peak timing changes in each year ##



# save out with a naming convention that'll dynamicaly change depending on parameters selected

ggsave(paste0(region_name, "_annual_peak_timing_shift_Plot", "_all_guilds_",".png"), 
       plot = p1, dpi = 300, width = 12, height = 8)




# maximum timing shift across all years 

peak_timing_summary <- data %>%  
  filter(col_type %in% c("experiment", "base"), 
         year %in% c(2:10)) %>% 
  group_by(region, hw_month, hw_temp, year, guild_group, col_type) %>% 
  summarise(peak_time = time[which.max(Values)],
            .groups = "drop") %>% 
  pivot_wider(names_from = col_type, 
              values_from = peak_time) %>% 
  mutate(timing_diff = base - experiment) %>% 
  group_by(region, hw_month, hw_temp, guild_group) %>% 
  summarise(timing_diff = timing_diff[which.max(abs(timing_diff))],
            .groups = "drop")


# how can i show this on a plot?

# plotting

p2 <- peak_timing_summary %>% 
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds"))) %>%
  ggplot(aes(x = factor(hw_month), y = factor(hw_temp), fill = timing_diff)) +
  facet_wrap(vars(guild_group)) +
  geom_tile() +
  scale_fill_gradient2(low = "#9100cd", mid = "white", high = "darkorange", midpoint = 0,
                       name = "Timing shift (days)") +
  scale_x_discrete(labels = month.abb[1:12]) +
  labs(x = "Heatwave month", y = "Heatwave temp (°C)",
       title = "Peak timing shift (days)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p2


# orange = the hw has advanced the timing (event happens earlier than it would have without the hw) - DELAY

# purple = the hw has delayed the timing (event happens later than it would without the hw) - ADVANCE

# save out with a naming convention that'll dynamicaly change depending on parameters selected

ggsave(paste0(region_name, "_maximum_peak_timing_shift_Plot", "_all_guilds_",".png"), 
       plot = p2, dpi = 300, width = 12, height = 8)
