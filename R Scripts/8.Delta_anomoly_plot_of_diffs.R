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
selected_guild <- "Birds"
selected_temps <- c(-2, 0, 3, 5)
selected_months <- c(1:6) #7:12 # for plot1:faceted by month
selected_month <- 3 #for plot2: faceted by guild

####-- axes setup--####

# this calculates the value limits per guild so that i can set the y axis to be consistent across plots
y_limits <- data %>%
  filter(col_type == "diff",
         hw_temp %in% selected_temps) %>%
  group_by(guild_group) %>%
  summarise(ymin = min(Values, na.rm = TRUE),
            ymax = max(Values, na.rm = TRUE),
            .groups = "drop")

install.packages("ggh4x")
library(ggh4x)

##-- plots --##

hw_windows <- data %>% 
  distinct(hw_month, hw_start.x, hw_end.x) %>% 
  filter(hw_month == selected_month)

selection_plot1 <- data %>% 
  filter(col_type == "diff",
         hw_temp %in% selected_temps,
         hw_month == selected_month) %>%
  left_join(y_limits, by = "guild_group") %>% 
  mutate(guild_group = factor(guild_group, 
                              levels = c("Phyto", "Omni_zoo", "Carn_zoo", 
                                         "Pfish_larvae", "Pfish", "Birds")))

plot1 <- selection_plot1 %>% 
  ggplot(aes(x = time, 
             y = Values,
             group = hw_temp,
             colour = hw_temp)) +
  facet_wrap(vars(guild_group), scales = "free_y") +
  scale_y_facet(guild_group == "Phyto", limits = c(-1.13, 0.741)) +
  scale_y_facet(guild_group == "Omni_zoo", limits = c(-1.45, 2.78)) +
  scale_y_facet(guild_group == "Carn_zoo", limits = c(-0.0424, 0.0922)) +
  scale_y_facet(guild_group == "Pfish_larvae", limits = c(-0.0106, 0.0246)) +
  scale_y_facet(guild_group == "Pfish", limits = c(-0.0622, 0.147)) +
  scale_y_facet(guild_group == "Birds", limits = c(-0.0000496, 0.000131)) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_rect(data = hw_windows,
            aes(xmin = hw_start.x + 360, xmax = hw_end.x + 360, 
                ymin = -Inf, ymax = Inf),
            fill = "khaki1", alpha = 0.02, colour = "khaki1", 
            linetype = "solid", linewidth = 0.5, inherit.aes = FALSE) +
  geom_line() +
  geom_vline(xintercept = seq(360, 3600, by = 360), linetype = "dashed", 
             colour = "black", linewidth = 0.5, alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 3600, by = 360), 
                     labels = paste0(0:10)) + 
  labs(x = "Year", y = "Anomaly from baseline", 
       title = paste0("Heatwave month: ", month.abb[selected_month])) +
  coord_cartesian(xlim = c(1441, 2880)) +      #c(361, 1440) - for start year 2 to end of year 4 c(1441, 2880) for year 5 to 8
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0, 
                         name = "Heatwave\nTemp (°C)", breaks = selected_temp) +
  theme_bw()

ggsave("diff_jan2.png", plot = all_guilds_diff, dpi = 300, width = 12, height = 8)





###### review the code below to check if you need it jasmine ######




















# looking at the effect between months of different temperatures of heatwave 
# all data
month <- data %>% 
  filter(hw_temp %in% c(-2, 0, 3, 5)) %>% 
  ggplot(aes(x = time, 
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

temp <- data %>% 
  filter(hw_temp %in% c(-2, -1, 0, 1, 3, 5)) %>% 
  ggplot(aes(x = time, 
            y = omni_o_diff + omni_i_diff,
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

# ok so those plots were just looking at one specific thing  
# need to think about how to facet wrap by guild 

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

##--only pulling out the _diff columns (i might want to change this in the future)--##

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
    grepl("^bird", Guild) ~ "Birds")) %>% 
  mutate(guild_group = factor(guild_group, 
                              levels = c("Phyto",
                                         "Omni_zoo", 
                                         "Carn_zoo", 
                                         "Pfish_larvae",
                                         "Pfish", 
                                         "Birds")))
# summing the abundances
data_grouped <- data_long %>%
  group_by(region, hw_month, hw_temp, time, year, yearday, season, guild_group) %>%
  summarise(
    Abundance = sum(Abundance, na.rm = TRUE),
    .groups = "drop"
  )

## -- plot -- ##

test <- data_grouped %>%  
  filter(hw_temp == 5) %>% 
  ggplot(aes(
  x = time,
  y = Abundance,
  group = hw_month,
  colour = season
)) +
  geom_line() +
  facet_wrap(vars(guild_group), scales = "free_y") + #change scales to fixed for same y axis on all plots
  labs(
    x = "Year of simulation",
    y = "Anomoly plot for +5 degree heatwave"
  ) +
  theme_minimal() + 
  scale_x_continuous(
    breaks = seq(0, 3600, by = 360),
    labels = paste0(0:10)) +
  scale_y_continuous(labels = scales::label_number())
test

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