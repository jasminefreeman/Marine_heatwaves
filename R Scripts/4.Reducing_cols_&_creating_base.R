library(tidyverse)
library(furrr)
library(progressr)
library(StrathE2E2)

####-- read in the data --####

region_name <- "Norwegian_Basin_MA"
data <- readRDS(paste0("./Objects/1.Full_data_", region_name, ".rds"))

## getting rid of columns i don't need/want right now 
# it all gets too big without reducing the number of columns 

data <- data %>%  
  select(-matches(
    "detritus|corpse|flux|ammonia|nitrate|atmos|land|disc|offal|gross|active|netprod|sed|spawn|recruit|migrat|flow|prod|nitri|NNCP"
  ))

#### -- creating a 'baseline' data frame --####

# filtering out just the hw_temp 0 run as this has not been subjected to any heat. so is the 'base' run

base <- data %>% 
  filter(hw_temp == "0")

# data frame to include all the columns from the big dataset 

df <- data %>% 
  subset(select = 1:6)

join_cols <- c("region", "hw_month", "time", "year", "yearday")

# joining hw_temp 0 run to dataframe with all the columns 

new <- df %>%  
  left_join(base, by = join_cols) #%>%  
  #filter(hw_temp.x == 4, year %in% c(3,4)) # test line 

# i think this worked

testplot <- new %>% 
  ggplot(aes(x = time, 
             y = phyt_so,
             colour = hw_temp.x,
             group = hw_temp.x)) +
          geom_line()
                        
testplot # this plots all 15 heatwave simulations on one graph

# it did work!
# all results for each temperature experiment are exactly the same. This is the base

# renaming the columns in this 'base' file, so that it can be joined to the main data file
# and the columns stay uniquely identifiable
new_renamed_cols <- new %>%  
  rename_with(~ paste0(.x, "_base"),
              -all_of(join_cols)) %>% 
  rename(hw_temp = hw_temp.x_base,
         hw_temp_base = hw_temp.y_base,
         hw_start = hw_start_base,
         hw_end = hw_end_base,
         heatwave = heatwave_base) 

# not really what i want - it's producing too many observations. That should stay the same 
all_data_with_base <- data %>% 
  left_join(
    new_renamed_cols, by = c("region", "hw_month", "time", "year", "yearday", "hw_temp"))
# ok this might have worked, do some check plots below

## -- check plots between 'base' and experiment outcomes -- ##

# filter the data to only include 1 heatwave scenario (for ease of looking)
plot_df <- all_data_with_base %>%
  filter(
    hw_temp == 5,
    hw_month == 3
  )

library(tidyr)

# pivoting data so that they can be plotted in the same way on the same plot
plot_df_long <- plot_df %>%
  select(time, bird_o, bird_o_base) %>%
  pivot_longer(
    cols = c(bird_o, bird_o_base),
    names_to = "type",
    values_to = "value"
  )

# make plot
ggplot(plot_df_long, aes(x = time, y = value, colour = type)) +
  geom_line() +
  labs(x = "Time", y = "bird_o", colour = "Series") + 
  coord_cartesian(xlim = c(361, 1200))

## ok so it looks like it's working. 

#### ---- saving all the data files out ---- ####

####-- save out the file that contains the base only --####

write.csv(new_renamed_cols, paste0("./Objects/2.Base_data_reduced_cols_", region_name, ".csv"))

saveRDS(new_renamed_cols, paste0("./Objects/2.Base_data_reduced_cols_", region_name, ".rds"))
check <- readRDS(paste0("./Objects/2.Base_data_reduced_cols_", region_name, ".rds"))

####-- save out the experiment results with reduced columns --####

write.csv(data, paste0("./Objects/3.Experiment_results_reduced_cols_", region_name, ".csv"))

saveRDS(data, paste0("./Objects/3.Experiment_results_reduced_cols_", region_name, ".rds"))
check <- readRDS(paste0("./Objects/3.Experiment_results_reduced_cols_", region_name, ".rds"))

####-- save out the combined file with experiment results and base --####

write.csv(all_data_with_base, paste0("./Objects/4.Experiment_results_reduced_cols_with_base_combined_", region_name, ".csv"))

saveRDS(all_data_with_base, paste0("./Objects/4.Experiment_results_reduced_cols_with_base_combined_", region_name, ".rds"))
check <- readRDS(paste0("./Objects/4.Experiment_results_reduced_cols_with_base_combined_", region_name, ".rds"))
