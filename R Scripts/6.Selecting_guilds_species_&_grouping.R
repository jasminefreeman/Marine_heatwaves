library(tidyverse)
library(furrr)
library(progressr)
library(StrathE2E2)
library(ggpattern)

# load in data 
# using the big combined file of everything for now whilst the no of columns is small

region_name <- "Norwegian_Basin_MA"
data <- readRDS(paste0("./Objects/6.All_data__base_exp_&_diff_", region_name, ".rds"))

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
  

##-- save out this filtered dataset -- ##

write.csv(data_grouped, paste0("./Objects/7.All_data_species_narrowed_", region_name, ".rds"))

saveRDS(data_grouped, paste0("./Objects/7.All_data_species_narrowed_", region_name, ".rds"))
check <- readRDS(paste0("./Objects/7.All_data_species_narrowed_", region_name, ".rds"))
