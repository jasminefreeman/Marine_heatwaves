#### --Setup -- ####

rm(list = ls()) # reset

library(StrathE2E2)
library(tidyverse)
library(future)
library(furrr)
library(tictoc)

plan(multisession)

source("./R Scripts/e2e_run_heatwave.R")

#### --Define the experiment-- ####

runs <- expand.grid(Decade = "2010-2019",
                    ESM = "CNRM",
                    SSP = "ssp370",
                    Month = 1:12,
                     Temp = seq(-2, 5, by = 0.5),
                     Region = c("Ascension_MA", 
                                "Azores_MA",
                                "Brazilian_Shelf_MA",
                                "Celtic_Sea_MA",
                                "Norwegian_Basin_MA",
                                "Saint_Helena_MA",
                                "Senegal_MA",
                                "South_Africa_MA")) %>% 
  mutate(Decade = as.character(Decade), 
         ESM = as.character(ESM), 
         SSP = as.character(SSP), 
         Region = as.character(Region))

#### --Run experiment--####                

run_simulation <- function(Decade, ESM, SSP, Month, Temp, Region) {

  #comment this out when happy
  #Decade <- "2010-2019"; ESM <- "CNRM" ; SSP <- "ssp370"; Month <- 1:12 ; Temp <- 0:5 ; Region <- "Ascension_MA";
    
   ##reading in the model
  
  model <- e2e_read(Region, str_glue("{Decade}-{ESM}-{SSP}"), models.path = "./Data/")
  
   # do the simulation
   results <- e2e_run_heatwave(model = model, nyears = 10, hw_year = 2, temp = Temp, month = Month) 
   
   ##save out results 
   saveRDS(results,str_glue("./Objects/Heatwaves/{Region}-{Decade}-{ESM}-{SSP}-{Month}-{Temp}.rds"))
   
}

#uncomment this when ready to go

tic()
## run
all_results <- future_pmap(
  runs,
  #runs[1:5,], # this is a way of testing the iteration - remove this line when happy
  run_simulation,
  .options = furrr_options(seed = TRUE), .progress = TRUE)
toc()
