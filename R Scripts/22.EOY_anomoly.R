library(tidyverse)
library(furrr)
library(progressr)
library(StrathE2E2)
library(ggpattern)

# load in data 
# using the big combined file of everything for now whilst the no of columns is small

region_name <- "Norwegian_Basin_MA"
data <- readRDS(paste0("./Objects/7.All_data_species_narrowed_", region_name, ".rds"))


#The difference at exactly 360 days after the heatwave has started (end of year 2)

EOY_anomoly_1yr_after <- data %>% 
  filter(col_type == "diff") %>% 
  filter(time %in% c(361:3600)) %>% # remove year 1 as the HW only goes into year 2
  filter(time == hw_start.x + 720) %>% # adding 720 as the time col is the timeseries column. so 1-360 is year 1 (no hw), 360-720 is year 2 (hw year), 720 onwards are a year after the hw)
  group_by(region, hw_month, hw_temp, year, time, guild_group) %>% 
  summarise(EOY_diff = Values,
            .groups = "drop")
  
#The difference at exactly 720 days after the heatwave has started (end of year 3)

EOY_anomoly_2yr_after <- data %>% 
  filter(col_type == "diff") %>% 
  filter(time %in% c(361:3600)) %>% 
  filter(time == hw_start.x + 1080) %>% 
  group_by(region, hw_month, hw_temp, year, time, guild_group) %>% 
  summarise(EOY_diff = Values,
            .groups = "drop")

#The difference at exactly 1080 days after the heatwave has started (end of year 4)

EOY_anomoly_3yr_after <- data %>% 
  filter(col_type == "diff") %>% 
  filter(time %in% c(361:3600)) %>% 
  filter(time == hw_start.x + 1440) %>% 
  group_by(region, hw_month, hw_temp, year, time, guild_group) %>% 
  summarise(EOY_diff = Values,
            .groups = "drop")

#The difference at exactly 1440 days after the heatwave has started (end of year 5)

EOY_anomoly_4yr_after <- data %>% 
  filter(col_type == "diff") %>% 
  filter(time %in% c(361:3600)) %>% 
  filter(time == hw_start.x + 1800) %>% 
  group_by(region, hw_month, hw_temp, year, time, guild_group) %>% 
  summarise(EOY_diff = Values,
            .groups = "drop")


# so effectively, what does it look like after 1 year, and 2 years, and 3 years etc... 

# how can i plot this??

####-- plots to look at the system 12 months post heatwave --####

# absolute values

p1 <- EOY_anomoly_1yr_after %>% 
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds"))) %>%
  ggplot(aes(x = factor(hw_month), y = factor(hw_temp), fill = EOY_diff)) +
  facet_wrap(vars(guild_group)) +
  geom_tile() +
  scale_fill_gradient2(low = "tomato", mid = "white", high = "springgreen4", midpoint = 0,
                       name = "EOY anomaly") +
  scale_x_discrete(labels = month.abb[1:12]) +
  labs(x = "Heatwave month", y = "Heatwave temp (°C)",
       title = "End of year anomaly 1 full year post HW (EOY2)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p1

# scaled values 

p2 <- EOY_anomoly_1yr_after %>% 
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds"))) %>%
  group_by(guild_group) %>% 
  mutate(scaled_vals = EOY_diff / max(abs(EOY_diff), na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = factor(hw_month), y = factor(hw_temp), fill = scaled_vals)) +
  facet_wrap(vars(guild_group)) +
  geom_tile() +
  scale_fill_gradient2(low = "tomato", mid = "white", high = "springgreen4", midpoint = 0,
                       name = "Relative\nEOY anomaly") +
  scale_x_discrete(labels = month.abb[1:12]) +
  labs(x = "Heatwave month", y = "Heatwave temp (°C)",
       title = "Relative(scaled) anomaly 1 full year post HW (EOY2)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2

####-- plots to look at the system 24 months post heatwave --####

# absolute values

p3 <- EOY_anomoly_2yr_after %>% 
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds"))) %>%
  ggplot(aes(x = factor(hw_month), y = factor(hw_temp), fill = EOY_diff)) +
  facet_wrap(vars(guild_group)) +
  geom_tile() +
  scale_fill_gradient2(low = "tomato", mid = "white", high = "springgreen4", midpoint = 0,
                       name = "EOY anomaly") +
  scale_x_discrete(labels = month.abb[1:12]) +
  labs(x = "Heatwave month", y = "Heatwave temp (°C)",
       title = "End of year anomaly 2 full years post HW (EOY3)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3

# scaled values 

p4 <- EOY_anomoly_2yr_after %>% 
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds"))) %>%
  group_by(guild_group) %>% 
  mutate(scaled_vals = EOY_diff / max(abs(EOY_diff), na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = factor(hw_month), y = factor(hw_temp), fill = scaled_vals)) +
  facet_wrap(vars(guild_group)) +
  geom_tile() +
  scale_fill_gradient2(low = "tomato", mid = "white", high = "springgreen4", midpoint = 0,
                       name = "Relative\nEOY anomaly") +
  scale_x_discrete(labels = month.abb[1:12]) +
  labs(x = "Heatwave month", y = "Heatwave temp (°C)",
       title = "Relative(scaled) anomaly 2 full years post HW (EOY3)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4


####-- plots to look at the system 36 months post heatwave --####

# absolute values

p5 <- EOY_anomoly_3yr_after %>% 
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds"))) %>%
  ggplot(aes(x = factor(hw_month), y = factor(hw_temp), fill = EOY_diff)) +
  facet_wrap(vars(guild_group)) +
  geom_tile() +
  scale_fill_gradient2(low = "tomato", mid = "white", high = "springgreen4", midpoint = 0,
                       name = "EOY anomaly") +
  scale_x_discrete(labels = month.abb[1:12]) +
  labs(x = "Heatwave month", y = "Heatwave temp (°C)",
       title = "End of year anomaly 3 full years post HW (EOY4)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p5

# scaled values 

p6 <- EOY_anomoly_3yr_after %>% 
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds"))) %>%
  group_by(guild_group) %>% 
  mutate(scaled_vals = EOY_diff / max(abs(EOY_diff), na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = factor(hw_month), y = factor(hw_temp), fill = scaled_vals)) +
  facet_wrap(vars(guild_group)) +
  geom_tile() +
  scale_fill_gradient2(low = "tomato", mid = "white", high = "springgreen4", midpoint = 0,
                       name = "Relative\nEOY anomaly") +
  scale_x_discrete(labels = month.abb[1:12]) +
  labs(x = "Heatwave month", y = "Heatwave temp (°C)",
       title = "Relative(scaled) anomaly 3 full years post HW (EOY4)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p6

####-- plots to look at the system 48 months post heatwave --####

# absolute values

p7 <- EOY_anomoly_4yr_after %>% 
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds"))) %>%
  ggplot(aes(x = factor(hw_month), y = factor(hw_temp), fill = EOY_diff)) +
  facet_wrap(vars(guild_group)) +
  geom_tile() +
  scale_fill_gradient2(low = "tomato", mid = "white", high = "springgreen4", midpoint = 0,
                       name = "EOY anomaly") +
  scale_x_discrete(labels = month.abb[1:12]) +
  labs(x = "Heatwave month", y = "Heatwave temp (°C)",
       title = "End of year anomaly 4 full years post HW (EOY5)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p7

# scaled values 

p8 <- EOY_anomoly_4yr_after %>% 
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds"))) %>%
  group_by(guild_group) %>% 
  mutate(scaled_vals = EOY_diff / max(abs(EOY_diff), na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = factor(hw_month), y = factor(hw_temp), fill = scaled_vals)) +
  facet_wrap(vars(guild_group)) +
  geom_tile() +
  scale_fill_gradient2(low = "tomato", mid = "white", high = "springgreen4", midpoint = 0,
                       name = "Relative\nEOY anomaly") +
  scale_x_discrete(labels = month.abb[1:12]) +
  labs(x = "Heatwave month", y = "Heatwave temp (°C)",
       title = "Relative(scaled) anomaly 4 full years post HW (EOY5)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p8
