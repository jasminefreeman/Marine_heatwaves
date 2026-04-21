library(tidyverse)
library(furrr)
library(progressr)
library(StrathE2E2)
library(ggpattern)

# load in data 
# using the big combined file of everything for now whilst the no of columns is small

region_name <- "Norwegian_Basin_MA"
data <- readRDS(paste0("./Objects/7.All_data_species_narrowed_", region_name, ".rds"))



# the 'diff' value on the final day of the simulation 

EOS_anomoly <- data %>%  
  filter(col_type == "diff",
         time == 3591)  %>% # probs need to find out why it doesn't go to 3600. Think it's to do with the zeroes in year 0/1.
  group_by(region, hw_month, hw_temp, guild_group) %>% 
  summarise(EOS_diff = Values,
            .groups = "drop")

# plotting

p1 <- EOS_anomoly %>%
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds"))) %>%
  ggplot(aes(x = factor(hw_month), y = factor(hw_temp), fill = EOS_diff)) +
  facet_wrap(vars(guild_group)) +
  geom_tile() +
  scale_fill_gradient2(low = "tomato", mid = "white", high = "springgreen4", midpoint = 0,
                       name = "End sim anomaly") +
  scale_x_discrete(labels = month.abb[1:12]) +
  labs(x = "Heatwave month", y = "Heatwave temp (°C)",
       title = "End of simulation anomaly") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p1




#scaled value

p2 <- EOS_anomoly %>%
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds"))) %>%
  group_by(guild_group) %>%
  mutate(scaled_val = EOS_diff / max(abs(EOS_diff), na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = factor(hw_month), y = factor(hw_temp), fill = scaled_val)) +
  facet_wrap(vars(guild_group)) +
  geom_tile() +
  scale_fill_gradient2(low = "tomato", mid = "white", high = "springgreen4", midpoint = 0,
                       name = "Relative\nend sim anomaly") +
  scale_x_discrete(labels = month.abb[1:12]) +
  labs(x = "Heatwave month", y = "Heatwave temp (°C)",
       title = "End of simulation anomaly") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p2
