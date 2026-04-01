rm(list = ls()) # reset

library(StrathE2E2)
library(tidyverse)
library(future)
library(furrr)

e2e_ls()

plan(multisession, workers = 12)
 
# # function for a single month perturbation


scenarios_ascention <- c("2010-2019-CNRM-ssp126","2010-2019-CNRM-ssp370",
                         "2010-2019-GFDL-ssp126","2010-2019-GFDL-ssp370",
                         "2020-2029-CNRM-ssp126","2020-2029-CNRM-ssp370",
                         "2020-2029-GFDL-ssp126","2020-2029-GFDL-ssp370",
                         "2030-2039-CNRM-ssp126","2030-2039-CNRM-ssp370",
                         "2030-2039-GFDL-ssp126","2030-2039-GFDL-ssp370",
                         "2040-2049-CNRM-ssp126","2040-2049-CNRM-ssp370",
                         "2040-2049-GFDL-ssp126","2040-2049-GFDL-ssp370",
                         "2050-2059-CNRM-ssp126","2050-2059-CNRM-ssp370",
                         "2050-2059-GFDL-ssp126","2050-2059-GFDL-ssp370",
                         "2060-2069-CNRM-ssp126","2060-2069-CNRM-ssp370",
                         "2060-2069-GFDL-ssp126","2060-2069-GFDL-ssp370")

for (i in 1:length(scenarios_ascention)) {
  model <- e2e_read("Ascension_MA",scenarios_ascention[i])
  
  model[["data"]][["physics.drivers"]][["so_temp"]][i] <-
    model[["data"]][["physics.drivers"]][["so_temp"]][i] + 5
  
  model[["data"]][["physics.drivers"]][["si_temp"]][i] <-
    model[["data"]][["physics.drivers"]][["si_temp"]][i] + 5
  
  model[["data"]][["physics.drivers"]][["d_temp"]][i] <-
    model[["data"]][["physics.drivers"]][["d_temp"]][i] + 5
  
  results <- e2e_run(model = model, nyears = 10) # will increase the years but just put as 10 so it didn't take days to run
  
  results[["final.year.outputs"]][["mass_results_wholedomain"]][1:31, ] %>%
    mutate(Month = i)
}
  
## run

# need to change the code below as its now in a for loop and run_month does not exist

# res <- future_map_dfr(
#    1:12,
#    run_month,
#    .options = furrr_options(seed = TRUE)
#  )
 
 saveRDS(res,"C:/Users/xvb24200/OneDrive - University of Strathclyde/Marine_heatwaves/Objects/ascention_scenarios.RDS") 
  
  
##experiment
##save results - bind to master dataframe to store results

model_base <- e2e_read("North_Sea","2003-2013")
results_base <- e2e_run(model = model_base,nyears = 1)
base <- results_base[["final.year.outputs"]][["mass_results_wholedomain"]][1:31, ]
res <- readRDS("C:/Users/xvb24200/OneDrive - University of Strathclyde/Marine_heatwaves/Objects/northsea_2003-2013.RDS") # change this directory

ggplot() +
  geom_line(
    data = marineHeatwaves,
    aes(x = Month, y = Model_annual_mean)
  ) +
  geom_point(
    data = marineHeatwaves,
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

ggsave("C:/Users/xvb24200/OneDrive - University of Strathclyde/Marine_heatwaves/Results/northsea_2003-2013_5degrees.png",                    # change this directory
       height = 4000,
       width = 6000,
       units = "px",
       dpi = 400)

ggplot() +
  geom_col(
    data = marine %>% filter(Description == "Planktivorous_fish"),
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
ggsave("C:/Users/xvb24200/OneDrive - University of Strathclyde/Marine_heatwaves/Results/northsea_2003-2013_5degrees-BAR.png",                # change this directory
       height = 4000,
       width = 6000,
       units = "px",
       dpi = 400)
