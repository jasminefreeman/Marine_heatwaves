library(tidyverse)
library(furrr)
library(progressr)

plan(multisession, workers = 4)

#files <- list.files("./Objects/Heatwaves/", full.names = T)

Norwegian_Basin_files <- list.files("./Objects/Heatwaves/",
pattern = "^Norwegian",
full.names = T)

with_progress({
  Norwegian_Basin_data <- future_map(Norwegian_Basin_files, read_rds, .progress = T)})

#this renames all the model runs inside ascension_files, to be called something easy
#to identify, so that it's not showing as [[1]] : [[180]]
names(Norwegian_Basin_data) <- basename(Norwegian_Basin_files)

mass_combined <- imap_dfr(ascension_data, function(model, name) {
  
  model[["final.year.outputs"]][["mass_results_wholedomain"]] %>% 
    select(Description, Units, Model_annual_mean) %>% 
    mutate(model_run = tools::file_path_sans_ext(name))
  
}) %>% 
  pivot_wider(names_from = model_run,
              values_from = Model_annual_mean)

mass_stacked <- imap_dfr(ascension_data, function(model, name) {
  
  model[["final.year.outputs"]][["mass_results_wholedomain"]] %>% 
    select(Description, Units, Model_annual_mean) %>% 
    mutate(model_run = tools::file_path_sans_ext(name))
  
}) %>% 
 mutate(
   Model_name = str_extract(model_run, ".*(?=_MA)"),
   #the above looks for anything right before _MA and returns it 
   #month is the number immediately after "ssp370-"
   Month = str_match(model_run, "ssp370-([0-9]{1,2})")[,2] %>% as.numeric(),
   #the above looks for any digit 0-9, and matches 1 or 2 of them for the month, and converts it to a number
   
   Temperature = str_match(model_run, "ssp370-[0-9]{1,2}-(.+)$")[,2] %>% as.numeric()) %>%  
  #the above looks for the dash immediately after the month, and puts everything that comes after that dash as temperature, and converts it to a number
  select(Model_name, Month, Temperature, Description, Model_annual_mean) %>% 
  pivot_wider(names_from = Description, values_from = Model_annual_mean)

ggplot(mass_stacked, aes(
  x = Month,
  y = Planktivorous_fish, 
  colour = factor(Temperature),
  group = factor(Temperature)))+
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks = 1:12)

################################################################################

#messing around trying to produce any output right now

run <- Norwegian_Basin_data[["Norwegian_Basin_MA-2010-2019-CNRM-ssp370-6-5.rds"]]

e2e_plot_ts(run, run)
