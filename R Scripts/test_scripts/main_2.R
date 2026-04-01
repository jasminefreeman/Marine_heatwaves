rm(list = ls()) # reset

## -- data outputs from experiment -- ##

data <- list.files("./Objects/", full.names = TRUE) %>% 
  map(read_rds)

ascension <- data[[1]][["final.year.outputs"]][["mass_results_wholedomain"]][1:31,] %>% 
  mutate(model = "ascension")

azores <- data[[2]][["final.year.outputs"]][["mass_results_wholedomain"]][1:31,] %>% 
  mutate(model = "azores")

brazil <- data[[3]][["final.year.outputs"]][["mass_results_wholedomain"]][1:31,] %>% 
  mutate(model = "brazil")

celtic <- data[[4]][["final.year.outputs"]][["mass_results_wholedomain"]][1:31,] %>% 
  mutate(model = "celtic")

norway <- data[[5]][["final.year.outputs"]][["mass_results_wholedomain"]][1:31,] %>% 
  mutate(model = "norway")

saint_hel <- data[[6]][["final.year.outputs"]][["mass_results_wholedomain"]][1:31,] %>% 
  mutate(model = "saint_hel")

senegal <- data[[7]][["final.year.outputs"]][["mass_results_wholedomain"]][1:31,] %>% 
  mutate(model = "senegal")

south_africa <- data[[8]][["final.year.outputs"]][["mass_results_wholedomain"]][1:31,] %>% 
  mutate(model = "south_africa")

all_data <- bind_rows(ascension, azores, brazil, celtic, norway, saint_hel, senegal, south_africa)

saveRDS(all_data, "./Objects/temp_dataset.rds")

##______________________________________________

## -- data outputs from base run -- ##

pattern <- "\\-base.rds"
base_data <- list.files("./Objects/", full.names = T, pattern = pattern) %>% 
  map(read_rds)

ascension_base <- base_data[[1]][["final.year.outputs"]][["mass_results_wholedomain"]][1:31,] %>% 
mutate(model = "ascension")

azores_base <- base_data[[2]][["final.year.outputs"]][["mass_results_wholedomain"]][1:31,] %>% 
mutate(model = "azores")

brazil_base <- base_data[[3]][["final.year.outputs"]][["mass_results_wholedomain"]][1:31,] %>% 
mutate(model = "brazil")

celtic_base <- base_data[[4]][["final.year.outputs"]][["mass_results_wholedomain"]][1:31,] %>% 
mutate(model = "celtic")

norway_base <- base_data[[5]][["final.year.outputs"]][["mass_results_wholedomain"]][1:31,] %>% 
mutate(model = "norway")

saint_hel_base <- base_data[[6]][["final.year.outputs"]][["mass_results_wholedomain"]][1:31,] %>% 
mutate(model = "saint_hel")

senegal_base <- base_data[[7]][["final.year.outputs"]][["mass_results_wholedomain"]][1:31,] %>% 
mutate(model = "senegal")

south_africa_base <- base_data[[8]][["final.year.outputs"]][["mass_results_wholedomain"]][1:31,] %>% 
mutate(model = "south_africa")

base_model_data <- bind_rows(ascension_base, azores_base, brazil_base, celtic_base, 
                       norway_base, saint_hel_base, senegal_base, south_africa_base)


saveRDS(base_model_data, "./Objects/temp_dataset_base_runs.rds")
