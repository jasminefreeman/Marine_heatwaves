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

## -- plot 1: 1 guild, faceted by month -- ##

# this calculates the heatwave window (the little yellow line on the plots)
hw_windows_plot1 <- data %>% 
  distinct(hw_month, hw_start.x, hw_end.x) %>% 
  filter(hw_month %in% selected_months) %>% 
  mutate(hw_month = factor(hw_month))

# this is where you define what you want the plot below to show
selection_plot1 <- data %>% 
  filter(col_type == "experiment", 
         guild_group == selected_guild,      # select the guild you want to look at 
         hw_temp %in% selected_temps,      # select the temperatures you want to look at
         hw_month %in% selected_months) %>%     # select the months you want to look at
  mutate(hw_month = factor(hw_month))
  
# plot which shows the month as the focus, with different lines on the plot for temperature 
# this is seperate for each guild
plot1 <- selection_plot1 %>% 
  ggplot(aes(x = time, 
             y = Values,
             group = hw_temp,
             colour = hw_temp)) +
  facet_wrap(vars(hw_month), labeller = labeller(hw_month = setNames(month.abb[1:12], as.character(1:12)))) +
  geom_rect(data = hw_windows_plot1,
            aes(xmin = hw_start.x + 360, xmax = hw_end.x + 360, 
                ymin = -Inf, ymax = Inf),
            fill = "khaki1", alpha = 0.5, colour = "khaki1", 
            linetype = "solid", linewidth = 0.5, inherit.aes = FALSE) +
  geom_line() +
  geom_vline(xintercept = seq(360, 3600, by = 360), linetype = "dashed", 
             colour = "black", linewidth = 0.5, alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 3600, by = 360), 
                     labels = paste0(0:10)) + 
  labs( x = "Year", y = "Abundance", title = selected_guild) +
  coord_cartesian(xlim = c(361, 1440)) + # limits to just to year 2, 3, 4 & 5    #manually change this
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0, 
                         name = "Heatwave\nTemp (°C)", breaks = c(-2, 0, 3, 5))+
  theme_bw()

plot1

ggsave("Birds_1-6.png", plot = plot1, dpi = 300, width = 12, height = 8)

##-- plot2: faceted by guild --##

# this calculates the yellow HW bars on the plot
hw_windows_plot2 <- data %>% 
  distinct(hw_month, hw_start.x, hw_end.x) %>% 
  filter(hw_month == selected_month)

# this is where you define what you want the plot to show
selection_plot2 <- data %>% 
  filter(col_type == "experiment",
         hw_temp %in% selected_temps,
         hw_month == selected_month) %>% 
  mutate(guild_group = factor(guild_group, levels = c("Phyto", "Omni_zoo", "Carn_zoo",
                                                      "Pfish_larvae", "Pfish", "Birds")))

#plot with guild as the focus, showing 1 month only and lines on the plot for all temperatures defined above
plot2 <- selection_plot2 %>% 
  ggplot(aes(x = time, 
             y = Values,
             group = hw_temp,
             colour = hw_temp)) +
  facet_wrap(vars(guild_group), scales = "free_y") +  # facet by guild now
  geom_rect(data = hw_windows_plot2,
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
  coord_cartesian(xlim = c(1441,2880)) + #c(361, 1440) - for start year 2 to end of year 4    #manually change this
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0, 
                         name = "Heatwave\nTemp (°C)", breaks = selected_temps) +
  theme_bw()

plot2

ggsave("Mar_HW_by_guild.png", plot = plot2, dpi = 300, width = 12, height = 8)
