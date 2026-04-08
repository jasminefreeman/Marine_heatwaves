library(tidyverse)
library(furrr)
library(progressr)
library(StrathE2E2)

# load in data 
# using the big combined file of everything for now whilst the number of columns is small

data <- readRDS("./Objects/All_data__base_exp_&_diff.rds")

data <- data %>% 
  mutate(season = case_when(
    hw_month %in% c(12, 1, 2) ~ "Winter",
    hw_month %in% c(3, 4, 5) ~ "Spring",
    hw_month %in% c(6, 7, 8) ~ "Summer", 
    hw_month %in% c(9, 10, 11) ~ "Autumn")) %>% 
  mutate(season = factor(season, 
                         levels = c("Spring", "Summer", "Autumn", "Winter")))

# looking at the effect between months of different temperatures of heatwave 
# all data
month <- ggplot(data, 
                    aes(x = time, 
                        y = omni_o_diff,
                        group = hw_temp,
                        colour = hw_temp)) +
  facet_wrap(vars(hw_month)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  labs(y = "Omni_o difference (base - experiment)") + 
  scale_x_continuous(
    breaks = seq(0, 3600, by = 360),
    labels = paste0(0:10)) +
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0)
month

#looking at the effect between temperatures 
# all data 

temp <- ggplot(data, 
                aes(x = time, 
                    y = omni_o_diff,
                    group = hw_month,
                    colour = season)) +
  facet_wrap(vars(hw_temp)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  labs(y = "Omni_o difference (base - experiment)") + 
  scale_x_continuous(
    breaks = seq(0, 3600, by = 360),
    labels = paste0(0:10)) + 
  scale_y_continuous(
    breaks = seq(-1.5, 4, by = 0.5)) + 
  scale_colour_manual(values = c("Winter" = "Blue", 
                                 "Spring" = "Mediumseagreen", 
                                 "Summer" = "Red", 
                                 "Autumn" = "Orange"))
temp

# ok so those plots were just looking at one specific thing in the food chain 
# need to think about how to facet wrap by guild 

# i want to pull out: 
# phyt_so
# phyt_d
# phyt_si

# omni_o
# omni_i

# carn_o
# carn_i 

# fishp_o
# fishp_i

# fishplar_o
# fishplar_i 

# bird_o
# bird_i

# only selecting the columns i want for now 
data_small <- data %>% 
  select(
    region, hw_month, hw_temp, time, year, yearday, season,
    matches("^phyt|omni|carn|fishp|bird"))


# converting the columns i want to look at, to long format with all the guilds in one column
data_long <- data_small %>%  
  pivot_longer(
    cols = -c(region, hw_month, hw_temp, time, year, yearday, season),
    names_to = "Guild",
    values_to = "Abundance")

##--only pulling out the _diff columns but i might want to change this in the future--##

data_long <- data_long %>%
  filter(grepl("_diff$", Guild))

# grouping the inshore and offshore guilds together 
data_long <- data_long %>% 
  mutate(guild_group = case_when(
    grepl("^phyt", Guild) ~ "Phyto",
    grepl("^omni", Guild) ~ "Omni_zoo",
    grepl("^carn", Guild) ~ "Carn_zoo",
    grepl("^fishp_", Guild) ~ "Pfish",
    grepl("^fishplar", Guild) ~ "Pfish_larvae",
    grepl("^bird", Guild) ~ "Birds"
  ))

data_grouped <- data_long %>%
  group_by(region, hw_month, hw_temp, time, year, yearday, season, guild_group) %>%
  summarise(
    Abundance = sum(Abundance, na.rm = TRUE),
    .groups = "drop"
  )

test <- data_grouped %>%  
  filter(hw_temp == 5) %>% 
  ggplot(aes(
  x = time,
  y = Abundance,
  group = hw_month,
  colour = season
)) +
  geom_line() +
  facet_wrap(vars(guild_group), scales = "free_y") +
  labs(
    x = "Time",
    y = "Diff between base and hw experiment(+5)"
  ) +
  theme_minimal() + 
  scale_x_continuous(
    breaks = seq(0, 3600, by = 360),
    labels = paste0(0:10)) 
test
