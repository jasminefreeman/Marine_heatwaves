library(tidyverse)
library(furrr)
library(progressr)
library(StrathE2E2)
library(ggpattern)

# load in data 
# using the big combined file of everything for now whilst the no of columns is small

region_name <- "Norwegian_Basin_MA"
data <- readRDS(paste0("./Objects/7.All_data_species_narrowed_", region_name, ".rds"))


####-- define parameters --####

# this is where  change what you want to look at 
selected_temps <- c(-2, 3, 5)
selected_month = 3


# calculating peak magnitude of the HW 

peak_mag <- data %>%  
  filter(col_type == "diff") %>%  
  group_by(region, hw_month, hw_temp, year, guild_group) %>% 
  summarise(
    peak_diff = Values[which.max(abs(Values))], # the magnitude of the biggest anomoly (keeps the signage if it was positive or negative)
    .groups = "drop") %>% 
  mutate(guild_group = factor(guild_group, 
                              levels = c("Phyto", "Omni_zoo", "Carn_zoo", 
                                         "Pfish_larvae", "Pfish", "Birds")))
#### -- axis setup -- ####

y_limits <- peak_mag %>%
  filter(hw_temp %in% selected_temps) %>%
  group_by(guild_group, hw_temp) %>%
  summarise(ymin = min(peak_diff, na.rm = TRUE),
            ymax = max(peak_diff, na.rm = TRUE),
            .groups = "drop")

# this looks at the values in y_scales above and creates a function so that the y scale 
# on the plot looks to these values without hard coding them in. They'll update dynamically
# if you change region

library(ggh4x) # fancypants package to customise ggplot

y_scales <- purrr::map(
  seq_len(nrow(y_limits)),
  function(eachguild) {
    guild <- y_limits$guild_group[eachguild]
    scale_y_facet(
      guild_group == !!guild, 
      limits = c(y_limits$ymin[eachguild], y_limits$ymax[eachguild]))})


selection_plot1 <- peak_mag %>% 
  filter(hw_temp %in% selected_temps,
         hw_month == selected_month) %>%
  left_join(y_limits, by = "guild_group") %>% 
  mutate(guild_group = factor(guild_group, 
                              levels = c("Phyto", "Omni_zoo", "Carn_zoo", 
                                         "Pfish_larvae", "Pfish", "Birds")))

# this shows where the peak "effect" of the HW is felt, it does not track the effect of the 
# HW over time. You can see that in the anomoly plot in script 8. 
plot1 <- selection_plot1 %>% 
  ggplot(aes(x = year, y = peak_diff, group = hw_temp.x, colour = factor(hw_temp.x))) +
  facet_wrap(vars(guild_group), scales = "free_y") +
  geom_line() +
  geom_vline(xintercept = 2, linetype = "dashed", colour = "black", alpha = 0.5) +
  scale_colour_manual(values = c("-2" = "blue", "3" = "orange", "5" = "red"),
                         name = "Heatwave\nTemp (°C)") +
  labs(x = "Year", y = "Peak magnitude anomaly",
       title = paste0("Peak magnitude anomaly - HW month: ", month.abb[3])) +
  theme_bw()
plot1



#lag between HW and the peak "effect" on each guild 

lag_to_peak <- data %>%
  filter(col_type == "diff") %>%
  filter(guild_group == "Birds", hw_month == 3) %>%  # remove later
  group_by(region, hw_month, hw_temp, guild_group) %>%
  mutate(hw_start = first(hw_start.x)) %>% # force a single consistent start time per group
  filter(time >= hw_start + 360) %>% # only look after the hw starts
  slice_max(abs(Values), n = 1, with_ties = FALSE) %>%
  summarise(
    time_of_peak = time,
    peak_diff = Values,
    lag_days = time_of_peak - (first(hw_start) + 360),
    .groups = "drop")

#plotting
lag_to_peak %>%
  filter(hw_month == selected_month, hw_temp %in% selected_temps) %>%
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds"))) %>%
  ggplot(aes(x = guild_group, y = lag_days, colour = hw_temp)) +
  geom_point(size = 4) +
  #scale_colour_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0,
      #                   name = "HW temp (°C)") +
  #labs(x = "Guild", y = "Lag to peak (days)",
      # title = paste0("Lag to peak - ", month.abb[3], " heatwave")) +
  theme_bw()


























## old code from a previous script -- figure out if it's any good or not ##

## -- -- ##

# now to work out: 

#1 - peak deviation from baseline

peaks <- data_long %>% 
  group_by(region, guild_group, hw_temp) %>% 
  summarise(
    peak_anomoly = max(Abundance, na.rm = T),
    min_anomoly = min(Abundance, na.rm = T),
    max_abs_dev = max(abs(Abundance), na.rm = T),
    .groups = "drop")

#2 - half life/decay time of the heatwave response

# get peak value per group 
halflife <- data_long %>% 
  group_by(region, guild_group, hw_month, hw_temp) %>% 
  mutate(abs_dev = abs(Abundance)) %>% 
  filter(abs_dev == max(abs_dev, na.rm = T)) %>% 
  slice(1) %>% 
  ungroup()

halflife_new <- data_long %>%
  group_by(region, guild_group, hw_month, hw_temp) %>%
  arrange(time, .by_group = TRUE) %>%
  summarise(
    
    # peak
    peak_index = which.max(abs(Abundance)),
    peak_time = time[peak_index],
    peak = abs(Abundance)[peak_index],
    half_threshold = peak / 2,
    
    # find half-life time
    half_life_time = {
      t <- time
      a <- abs(Abundance)
      
      post_peak <- which(t > peak_time)
      below <- which(a[post_peak] <= half_threshold)
      
      if (length(below) == 0) NA_real_ else t[post_peak[below[1]]]
    },
    
    # duration
    half_life_duration = half_life_time - peak_time,
    
    .groups = "drop"
  )

check <- halflife_new %>% 
  ggplot(aes, x = )