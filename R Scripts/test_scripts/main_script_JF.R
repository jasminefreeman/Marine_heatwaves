#### --Setup -- ####

rm(list = ls()) # reset

library(StrathE2E2)
library(tidyverse)
library(future)
library(furrr)

plan(multisession, workers = 12)

#### --Define the experiment-- ####

runs <- expand.grid(Decade = "2010-2019",
                    ESM = c("GFDL", "CNRM"),
                    SSP = c("ssp126","ssp370"),
                    Month = 1:12,
                    Temp = seq(-2, 5, by = 0.5),
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
  
  #comment out the below once happy. This is testing the function to save from having to run all of the models. It's overwriting the big run with specific parameters. 
   Decade <- "2010-2019"; ESM <- "CNRM" ; SSP <- "ssp370"; Month <- 1:12 ; Temp <- 0:5 ; Region <- "Ascension_MA";

##reading in the model

  model <- e2e_read(Region, str_glue("{Decade}-{ESM}-{SSP}"), models.path = "./Data/") #maybe need to remove the last slash or remove the space between the name of folder 

##changing/updating the model
  model[["data"]][["physics.drivers"]][["so_temp"]][Month] <-
  model[["data"]][["physics.drivers"]][["so_temp"]][Month] + Temp
  
  model[["data"]][["physics.drivers"]][["si_temp"]][Month] <-
    model[["data"]][["physics.drivers"]][["si_temp"]][Month] + Temp
  
  
##do the simulation
  results <- e2e_run(model = model, nyears = 5) 
  
##save out results 
  saveRDS(results,str_glue("./Objects/{Region}-{Decade}-{ESM}-{SSP}-{Month:Month}-{Temp:Temp}.rds"))

}

e2e_ls()


#uncomment this when ready to go

## run
# all_results <- future_pmap(
#   #runs,
#   runs[1:5,], # this is a way of testing the iteration - remove this line when happy
#   run_simulation,
#   .options = furrr_options(seed = TRUE), .progress = TRUE)

# saveRDS(res,"C:/Users/xvb24200/OneDrive - University of Strathclyde/Marine_heatwaves/Objects/ascention2010_2019.RDS")

model_base <- e2e_read("Ascension_MA", "2010-2019-CNRM-ssp126")
results_base <- e2e_run(model = model_base,nyears = 1)
base <- results_base[["final.year.outputs"]][["mass_results_wholedomain"]][1:31, ]
res <- readRDS("C:/Users/xvb24200/OneDrive - University of Strathclyde/Marine_heatwaves/Objects/ascention2010_2019.RDS") # change this directory
#res repeated here so i dont have to run the model each time, i can just read the data in from the model run above.

ggplot() +
  geom_line(
    data = res,
    aes(x = Month, y = Model_annual_mean)
  ) +
  geom_point(
    data = res,
    aes(x = Month, y = Model_annual_mean)
  ) +
  geom_hline(
    data = base,
    aes(yintercept = Model_annual_mean),
    linetype = "dashed",
    colour = "black"
  ) +
  scale_x_continuous(labels = seq(1,12),breaks = seq(1,12)) +
  facet_wrap(~ Description, scales = "free_y") +
  theme_minimal() +
  NULL

ggsave("C:/Users/xvb24200/OneDrive - University of Strathclyde/Marine_heatwaves/Results/ascention2010_2019.png",
       height = 4000,
       width = 6000,
       units = "px",
       dpi = 400)

ggplot() +
  geom_col(
    data = res %>% filter(Description == "Planktivorous_fish"),
    aes(x = Month, y = Model_annual_mean)
  ) +
  geom_hline(
    data = base %>% filter(Description == "Planktivorous_fish"),
    aes(yintercept = Model_annual_mean),
    linetype = "dashed",
    colour = "red"
  ) +
  labs(x = "Heatwave Occurence Month",y = "Planktivorous fish biomass (mmN/m^2)") +
  scale_x_continuous(labels = seq(1,12),breaks = seq(1,12)) +
  # facet_wrap(~ Description, scales = "free_y") +
  theme_minimal() +
  NULL

ggsave("C:/Users/xvb24200/OneDrive - University of Strathclyde/Marine_heatwaves/Results/ascention2010_2019-BAR.png",
       height = 4000,
       width = 6000,
       units = "px",
       dpi = 400)
