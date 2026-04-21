library(tidyverse)
library(furrr)
library(progressr)
library(StrathE2E2)
library(ggpattern)

# load in data 
# using the big combined file of everything for now whilst the no of columns is small

region_name <- "Norwegian_Basin_MA"
data <- readRDS(paste0("./Objects/7.All_data_species_narrowed_", region_name, ".rds"))

# how many days after the hw starts until the diff stays near zero. 
# near zero here is within 5% of the peak anomoly.
# so this shows the duration of the hw effect on the system.

duration <- data %>% 
  filter(col_type == "diff",
         time >= hw_start.x + 360) %>% 
  group_by(region, hw_month, hw_temp, guild_group) %>% 
  summarise(
    peak = max(abs(Values), na.rm = T),
    threshold = 0.05 * peak,
    hw_start_abs = first(hw_start.x) + 360,     # absolute start in year 2
    duration_days = {
      above_threshold  <- abs(Values) > threshold
      if(any(above_threshold)) {
        max(time[above_threshold]) - hw_start_abs
      } else {
        0
      }
    },
    .groups = "drop")


# plotting

p1 <- duration %>%
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds"))) %>%
  ggplot(aes(x = factor(hw_month), y = factor(hw_temp), fill = duration_days)) +
  facet_wrap(vars(guild_group)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Duration (days)") +
  scale_x_discrete(labels = month.abb[1:12]) +
  labs(x = "Heatwave month", y = "Heatwave temp (°C)",
       title = "Duration of heatwave effect") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1

#scaled

p2 <- duration %>%
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds"))) %>%
  group_by(guild_group) %>%
  mutate(scaled_val = duration_days / max(abs(duration_days), na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = factor(hw_month), y = factor(hw_temp), fill = scaled_val)) +
  facet_wrap(vars(guild_group)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Relative\nDuration (days)") +
  scale_x_discrete(labels = month.abb[1:12]) +
  labs(x = "Heatwave month", y = "Heatwave temp (°C)",
       title = "Duration of heatwave effect") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2
