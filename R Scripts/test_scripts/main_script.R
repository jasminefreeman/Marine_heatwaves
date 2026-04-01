#### --Setup -- ####

rm(list = ls()) # reset

library(StrathE2E2)
library(tidyverse)
library(future)
library(furrr)

plan(multisession, workers = 12)

#### --Define the experiment-- ####

runs <- expand.grid(Decade = paste0(seq(2010, 2060, by = 10), "-", seq(2019, 2069, by = 10)),
                    ESM = c("GFDL", "CNRM"),
                    SSP = c("ssp126", "ssp370"),
                    Month = 1:12,
                    Temp = seq(0, 10, by = 0.5),
                    Region = c("Ascension_MA",
                               "Azores_MA",
                               "Brazilian_Shelf_MA",
                               "Celtic_Sea_MA",
                               "Norwegian_Basin_MA",
                               "Saint_Helena_MA",
                               "Senegal_MA",
                               "South_Africa_MA"))

#### --Run experiment--####

# run_simulation <- function(Decade, ESM, SSP, Month, Temp, Region) { #unhash this when ready to roll
  
  #comment out the below once happy. This is testing the function to save from having to run all of the models. It's overwriting the big run with specific parameters. 
  Decade <- "2030-2039"; ESM <- "CNRM" ; SSP <- "ssp370"; Month <- 3 ; Temp <- 5 ; Region <- "South_Africa_MA";

##reading in the model

  model <- e2e_read(Region, str_glue("{Decade}-{ESM}-{SSP}"), models.path = "./Data/") #maybe need to remove the last slash or remove the space between the name of folder 

#changing/updating the model
model[["data"]][["physics.drivers"]][["so_temp"]][Month] <-
model[["data"]][["physics.drivers"]][["so_temp"]][Month] + Temp

model[["data"]][["physics.drivers"]][["si_temp"]][Month] <-
  model[["data"]][["physics.drivers"]][["si_temp"]][Month] + Temp

model[["data"]][["physics.drivers"]][["d_temp"]][Month] <-
  model[["data"]][["physics.drivers"]][["d_temp"]][Month] + Temp
  
##do the simulation
  results <- e2e_run(model = model, nyears = 50) # will increase the years but just wanted to get it running 
  
##save out results 
  saveRDS(results,str_glue("./Objects/{Region}-{Decade}-{ESM}-{SSP}-{Month}-{Temp}-base.rds"))

#}

# ## run #unhash this when ready to run the main model 
# all_results <- future_pmap(
#   #runs,
#   runs[1:5,], # this is a way of testing the iteration - remove this line when happy
#   run_simulation,
#   .options = furrr_options(seed = TRUE), .progress = TRUE)

# saveRDS(res,"C:/Users/xvb24200/OneDrive - University of Strathclyde/Marine_heatwaves/Objects/ascention2010_2019.RDS")

model_base <- e2e_read(Region, str_glue("{Decade}-{ESM}-{SSP}"), models.path = "./Data/")
results_base <- e2e_run(model = model_base,nyears = 2)
base <- results_base[["final.year.outputs"]][["mass_results_wholedomain"]][1:31, ]
res <- readRDS("./Objects/{Region}-{Decade}-{ESM}-{SSP}-{Month}-{Temp}.rds") # change this directory

