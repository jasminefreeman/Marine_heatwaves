library(tidyverse)
library(furrr)
library(progressr)
library(StrathE2E2)


data <- readRDS("./Objects/Experiment_results_reduced_cols_with_base_combined.rds")

# copy data into a new dataframe
diff <- data

#finds all columns that end in _base ($  means end of text)
base_cols <- grep("_base$", names(data), value = TRUE)       #(grep = find things that match this pattern)

#loops through all column names in the dataset
for (eachbasecolumnname in base_cols) {
  exp_col <- sub("_base$", "", eachbasecolumnname)   #takes text in eachbasecolumnname, remove _base from the end, replace with nothing
  diff[[paste0(exp_col, "_diff")]] <- diff[[exp_col]] - diff[[eachbasecolumnname]]      #creates new column name + _diff, do calculation row by row
}

# check that it looks right below

diff %>% 
  filter(hw_temp == 5, hw_month == 4) %>% 
  select(omni_o, omni_o_base, omni_o_diff) %>% 
  print(n = 500)
# you can see that the values start to change from the hw month

checkplot <- ggplot(diff, aes(x = time, y = omni_o_diff)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  labs(y = "Difference (experiment - base)")
checkplot

# woohoo

## create dataframe of just the difference columns only ##

diff_only <- diff %>% 
  select(region, hw_month, hw_temp, time, year, yearday, 
         hw_start.x, hw_end.x, heatwave.x, 
         ends_with("_diff")) %>% 
  select(!hw_temp_diff)


#### -- save out files -- ####

# difference only file 

write.csv(diff_only, "./Objects/Diff_btw_base_&_exp.csv", row.names = FALSE)

saveRDS(diff_only, "./Objects/Diff_btw_base_&_exp.rds")
check <- readRDS("./Objects/Diff_btw_base_&_exp.rds")

# big datafile with base, experiment outputs and the difference between the two 

write.csv(diff, "./Objects/All_data__base_exp_&_diff.csv", row.names = FALSE)

saveRDS(diff, "./Objects//All_data__base_exp_&_diff.rds")
check <- readRDS("./Objects//All_data__base_exp_&_diff.rds")
