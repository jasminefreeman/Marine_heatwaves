library(tidyverse)
library(furrr)
library(progressr)
library(StrathE2E2)

plan(multisession, workers = 4)

####-- load in the model runs --####

# this loads in all of the Norwegian model runs
Senegal_files <- list.files("./Objects/Heatwaves/",
                                    pattern = "^Senegal",
                                    full.names = T)

with_progress({
  Senegal_data <- future_map(Senegal_files, read_rds)})

#this renames all the model runs inside Norwegian_Basin_files, to be called something easy
#to identify, so that it's not showing as [[1]] : [[180]]
names(Senegal_data) <- basename(Senegal_files)

####-- combine together --####

# this combines all of the daily time series data for all model runs into one
# big data frame
mass_combined <- imap_dfr(
  Senegal_data, 
  ~ as.data.frame(.x$output), 
  .id = "model_run")

####-- amend columns names --####

# this pulls out the separate parts of the model_run name, into different columns 
# and drops the ones we don't want
mass_combined <- mass_combined %>% 
  mutate(model_run = sub("\\.rds$", "", model_run)) %>%  # this gets rid of .rds file extension at the end
  mutate(model_run = gsub("--", "-neg", model_run)) %>%  # this temporarily sorts the double negative issue
  separate(model_run,
           into = c("region", "model", "years", "gcm", "scenario", "month", "temp"),
           sep = "-") %>%  
  select(-"gcm", -"scenario", -"model", -"years") # removes the columns we don't want

mass_combined$temp <- as.numeric(gsub("^neg", "-", mass_combined$temp))  # this fixes the double negative issue

####-- fix problem --####

## something funky going on in the original experiment scripts - day 0 coming through and there are duplicate
## days when the years cross over.

## this code temp fixes this by taking only the first combination of time/month/temp in the time series 
## but really this should be fixed earlier on in the original experiment code. 
## we're left with some day 0 and year 0 values but ok for now
mass_combined2 <- mass_combined %>% 
  group_by(time, month, temp) %>% 
  slice(1)  #%>% 
  #mutate(n = n()) # check line to see if there are any duplicate time/month/temp combos

####-- add a few more columns for better analysis --####

# this adds in some new columns which may be useful for plots 
# year (which year of the simulation we're in)
# yearday (1-360 in each year)
# hw_start - start of HW (in terms of yearday) - will differ in the simulations where we've put it in in different months 
# hw_end - end of HW (in terms of yearday)
mass_combined3 <- mass_combined2 %>% 
  mutate(
    year = ((time-1) %/% 360) + 1,
    yearday = ((time - 1) %% 360 + 1),
    month = as.numeric(as.character(month)), 
    hw_start = (month - 1) * 30 + 1, 
    hw_end = month * 30, 
    heatwave = year == 2 & yearday >= hw_start & yearday <= hw_end) %>% 
  relocate(year, yearday, hw_start, hw_end, heatwave, .after = time) %>% 
  rename(hw_month = month, hw_temp = temp) # renamed for clarity

final <- mass_combined3

####-- save the master data file --####

region_name <- "Senegal_MA"

#write.csv(final, paste0("./Objects/1.Full_data_", region_name, ".csv"))

saveRDS(final, paste0("./Objects/1.Full_data_", region_name, ".rds"))
check <- readRDS(paste0("./Objects/1.Full_data_", region_name, ".rds"))

