library(tidyverse)
library(furrr)
library(progressr)
library(StrathE2E2)
library(ggpattern)

# load in data 
# using the big combined file of everything for now whilst the no of columns is small

region_name <- "South_Africa_MA"
data <- readRDS(paste0("./Objects/7.All_data_species_narrowed_", region_name, ".rds"))


####-- define parameters --####

# this is where  change what you want to look at 
selected_temps <- c(-2, 0, 3, 5)
selected_month <- 12
x_lim_plot1 <- c(1441, 2880) # c(361, 1440)  or   c(1441, 2880)


####-- axes setup--####

# this calculates the max and min value limits per guild so that the y axis can be 
# fixed to be consistent across the differing month plots 

y_limits <- data %>%
  filter(col_type == "diff",
         hw_temp %in% selected_temps) %>%
  group_by(guild_group) %>%
  summarise(ymin = min(Values, na.rm = TRUE),
            ymax = max(Values, na.rm = TRUE),
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


####-- plots --####

# this calculates the heatwave window (the little yellow line on the plots)
hw_windows <- data %>% 
  distinct(hw_month, hw_start.x, hw_end.x) %>% 
  filter(hw_month == selected_month)

# this selects the data for the plot based on the parameters that are defined above
# and joins these to the y limits calculated, so that all the data for plotting is in one df
selection_plot1 <- data %>% 
  filter(col_type == "diff",
         hw_temp %in% selected_temps,
         hw_month == selected_month) %>%
  left_join(y_limits, by = "guild_group") %>% 
  mutate(guild_group = factor(guild_group, 
                              levels = c("Phyto", "Omni_zoo", "Carn_zoo", 
                                         "Pfish_larvae", "Pfish", "Birds")))

# plot which shows the difference from the baseline with HW month as the focus, 
# faceted by guild and showing the temperatures outlined in the parameters above. 

plot1 <- selection_plot1 %>% 
  ggplot(aes(x = time, 
             y = Values,
             group = hw_temp,
             colour = hw_temp)) +
  facet_wrap(vars(guild_group), scales = "free_y") +
  y_scales +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_rect(data = hw_windows,
            aes(xmin = hw_start.x + 360, xmax = hw_end.x + 360, 
                ymin = -Inf, ymax = Inf),
            fill = "khaki1", alpha = 0.5, colour = "khaki1", 
            linetype = "solid", linewidth = 0.5, inherit.aes = FALSE) +
  geom_line() +
  geom_vline(xintercept = seq(360, 3600, by = 360), linetype = "dashed", 
             colour = "black", linewidth = 0.5, alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 3600, by = 360), 
                     labels = paste0(0:10)) + 
  labs(x = "Year", y = "Anomaly from baseline", 
       title = paste0(region_name, " | " , "Heatwave month: ", month.abb[selected_month])) +
  coord_cartesian(xlim = x_lim_plot1) + 
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0, 
                         name = "Heatwave\nTemp (°C)", breaks = selected_temps) +
  theme_bw()

plot1

# save out with a naming convention that'll dynamicaly change depending on parameters selected

ggsave(paste0(region_name, "_Anomoly_Plot", "_all_guilds_", month.abb[selected_month], 
              "_yrs", floor(x_lim_plot1[1]/360), "-", ceiling(x_lim_plot1[2]/360),".png"), 
       plot = plot1, dpi = 300, width = 12, height = 8)

