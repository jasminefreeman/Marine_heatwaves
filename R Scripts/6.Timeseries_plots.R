library(tidyverse)
library(furrr)
library(progressr)
library(StrathE2E2)
library(ggpattern)

# load in data 
# using the big combined file of everything for now whilst the no of columns is small

data <- readRDS("./Objects/All_data__base_exp_&_diff.rds")

# only selecting the columns i want for now to make it easier to work with
data_small <- data %>% 
  select(
    region, hw_month, hw_temp, time, year, yearday, hw_start.x, hw_end.x,
    matches("^phyt|omni|carn|fishp|bird"))

# mutating data to stack all guilds in one column
# also grouping so that there is no inshore/offshore values. They just combine for a 'total'
data_grouped <- data_small %>% 
  pivot_longer(cols = -c(region, hw_month, hw_temp, time, year, yearday, hw_start.x, hw_end.x),
               names_to = "Guild",
               values_to = "Values") %>% 
  mutate(
    guild_group = case_when(
      grepl("^phyt", Guild) ~ "Phyto",
      grepl("^omni", Guild) ~ "Omni_zoo",
      grepl("^carn", Guild) ~ "Carn_zoo",
      grepl("^fishp_", Guild) ~ "Pfish",
      grepl("^fishplar", Guild) ~ "Pfish_larvae",
      grepl("^bird", Guild) ~ "Birds"),
    col_type = case_when(
      grepl("_diff$", Guild) ~ "diff",
      grepl("_base$", Guild) ~ "base",
      T ~ "experiment")) %>% 
  group_by(region, hw_month, hw_temp, time, year, yearday, guild_group, col_type) %>% 
  summarise(Values = sum(Values, na.rm = T), .groups = "drop")
  

# this calculates the heatwave window (the little yellow line on the plots)
# change the filter line to switch between the two 6 month sections
hw_windows <- data_small %>% 
  distinct(hw_month, hw_start.x, hw_end.x) %>% 
  filter(hw_month %in% c(7, 8, 9, 10, 11, 12)) %>% #splitting the year into 2, 6 month blocks to make it easier to see whats going on
  mutate(hw_month = factor(hw_month))

#c(1, 2, 3, 4, 5, 6)
#c(7, 8, 9, 10, 11, 12)

# this is where you define what you want the plot below to show
selection <- data_grouped %>% 
  filter(col_type == "experiment", 
         guild_group == "Birds",      # select the guild you want to look at 
         hw_temp %in% c(-2, 0, 3, 5),      # select the temperatures you want to look at
         hw_month %in% c(7, 8, 9, 10, 11, 12)     # select the months you want to look at
         ) %>% 
  mutate(hw_month = factor(hw_month))
  
# plot which shows the month as the focus, with different lines on the plot for temperature 
# this is seperate for each guild
month <- selection %>% 
  ggplot(aes(x = time, 
             y = Values,
             group = hw_temp,
             colour = hw_temp)) +
  facet_wrap(vars(hw_month), labeller = labeller(hw_month = setNames(month.abb[1:12], as.character(1:12)))) +
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
  labs( x = "Year", y = "Abundance", title = unique(selection$guild_group)) +
  coord_cartesian(xlim = c(361, 1440)) + # limits to just to year 2, 3, 4 & 5
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0, 
                         name = "Heatwave\nTemp (°C)", breaks = c(-2, 0, 3, 5))+
  theme_bw()

ggsave("birds7-12.png", plot = month, dpi = 300, width = 12, height = 8)
# this is v v interesting for birds!!

# plot which shows all guilds together for a specified HW month

selected_temp <- c(-2, 0, 3, 5)
selected_month <- 1  

hw_windows <- data_small %>% 
  distinct(hw_month, hw_start.x, hw_end.x) %>% 
  filter(hw_month == selected_month)

select <- data_grouped %>% 
  filter(col_type == "experiment",
         hw_temp %in% selected_temp,
         hw_month == selected_month) %>% 
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds")))

all_guilds <- select %>% 
  ggplot(aes(x = time, 
             y = Values,
             group = hw_temp,
             colour = hw_temp)) +
  facet_wrap(vars(guild_group), scales = "free_y") +  # facet by guild now
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
  labs(x = "Year", y = "Abundance", 
       title = paste0("Heatwave month: ", month.abb[selected_month])) +
  coord_cartesian(xlim = c(1441,2880)) + #c(361, 1440) - for start year 2 to end of year 4
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0, 
                         name = "Heatwave\nTemp (°C)", breaks = selected_temp) +
  theme_bw()
ggsave("all_jan3.png", plot = all_guilds, dpi = 300, width = 12, height = 8)
