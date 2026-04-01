#### --Setup -- ####

rm(list = ls()) # reset

library(StrathE2E2)
library(tidyverse)
library(future)
library(furrr)

source("./R Scripts/e2e_run_heatwave.R")

e2e_ls()

#### --Define the experiment-- ####

runs <- expand.grid(Decade = "2010-2019",
                    ESM = "CNRM",
                    SSP = "ssp370",
                    Month = 1:12,
                     Temp = seq(1, 5, by = 1),
                     Region = c("Ascension_MA", 
                                "Azores_MA",
                                "Brazilian_Shelf_MA",
                                "Celtic_Sea_MA",
                                "Norwegian_Basin_MA",
                                "Saint_Helena_MA",
                                "Senegal_MA",
                                "South_Africa_MA"))

#### --Run experiment--####                

run_simulation <- function(Decade, ESM, SSP, Month, Temp, Region) {

  #comment this out when happy
  Decade <- "2010-2019"; ESM <- "CNRM" ; SSP <- "ssp370"; Month <- 1:12 ; Temp <- 0:5 ; Region <- "Ascension_MA";
    
   ##reading in the model
  
  model <- e2e_read(Region, str_glue("{Decade}-{ESM}-{SSP}"), models.path = "./Data/")
  
  ## extracting initial state 
  
  initial_state <- model$data$initial.state
  
  ##changing/updating the model
  
  #surface offshore temp
   model[["data"]][["physics.drivers"]][["so_temp"]][Month] <-
    model[["data"]][["physics.drivers"]][["so_temp"]][Month] + Temp
   
  #surface inshore temp
   model[["data"]][["physics.drivers"]][["si_temp"]][Month] <-
    model[["data"]][["physics.drivers"]][["si_temp"]][Month] + Temp
   
   # do the simulation
   results <- e2e_run(model = model, nyears = 5, csv.output=F) 
   
   
   
   
}