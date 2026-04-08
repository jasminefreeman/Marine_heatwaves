library(tidyverse)
library(furrr)
library(progressr)
library(StrathE2E2)

####-- read in the data --####

data <- readRDS("./Objects/Full_data.rds")

#### -- creating a 'baseline' data frame --####

base <- data %>% 
  filter(hw_temp == "0")

join_cols <- c("region", "hw_month", "time", "year", "yearday")

base_renamed <- base %>% 
  rename_with(~ paste0(.x, "_base"),
              -all_of(join_cols))

data_with_base <- data %>% 
  left_join(base_renamed, by = join_cols)

data_with_base %>% 
  filter(time == 1, hw_month == 1) %>% 
  select(hw_temp, detritus_so, detritus_so_base)

test <- data_with_base %>%  
  filter(hw_temp %in% c(0,5),
         year %in% c(2, 3, 4)) %>% 
  select(region, hw_month, hw_temp, time, year, yearday, detritus_so, detritus_so_base)
  
base_cols <- grep("_base$", names(data_with_base), value = T)

library(dplyr)

data_with_diff <- data_with_base %>% 
  mutate(
    across(
      ends_with("_base"),
      ~ {
        base_col <- .col
        orig_col <- sub("_base$", "", base_col)
        .data[[orig_col]] - .x
      },
      .names = "{sub('_base$', '', .col)}_diff"
    )
  )

