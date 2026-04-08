library(tidyverse)
library(furrr)
library(progressr)
library(StrathE2E2)

# load in data 
# using the big combined file of everything for now whilst the no of columns is small

data <- readRDS("./Objects/All_data__base_exp_&_diff.rds")
