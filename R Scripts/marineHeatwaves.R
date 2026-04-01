rm(list = ls()) # reset

library(StrathE2E2)
library(tidyverse)
# library(future)
# library(furrr)

e2e_ls()

# plan(multisession, workers = 12)
# 
# # function for a single month perturbation
scenarios <- c("2011-2019...",
               "2020-2029...")

for (i in 1:length(scenarios)) {
  model <- e2e_read(...,scenarios[i])
  ##experiment
  ##save results - bind to master dataframe to store results
}

# run_month <- function(i) {
#   
#   model <- e2e_read("North_Sea", "2003-2013")


    model_SH <- e2e_read("Saint_Helena_MA","2010-2019-CNRM-SSP126")
    
    
#   model[["data"]][["physics.drivers"]][["so_temp"]][i] <-
#     model[["data"]][["physics.drivers"]][["so_temp"]][i] + 5
#   
#   model[["data"]][["physics.drivers"]][["si_temp"]][i] <-
#     model[["data"]][["physics.drivers"]][["si_temp"]][i] + 5
#   
#   model[["data"]][["physics.drivers"]][["d_temp"]][i] <-
#     model[["data"]][["physics.drivers"]][["d_temp"]][i] + 5
#   
#   results <- e2e_run(model = model, nyears = 500)
#   
#   results[["final.year.outputs"]][["mass_results_wholedomain"]][1:31, ] %>%
#     mutate(Month = i)
# }
# 
# ## run
# res <- future_map_dfr(
#   1:12,
#   run_month,
#   .options = furrr_options(seed = TRUE)
# )
# 
# saveRDS(res,"C:/Desktop/PhD/Teaching/Jasmine/marineHeatwaves.RDS")            # change this directory

model_base <- e2e_read("North_Sea","2003-2013")
results_base <- e2e_run(model = model_base,nyears = 1)
base <- results_base[["final.year.outputs"]][["mass_results_wholedomain"]][1:31, ]
res <- readRDS("C:/Desktop/PhD/Teaching/Jasmine/marineHeatwaves.RDS") # change this directory

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

ggsave("C:/Desktop/PhD/Teaching/Jasmine/marineHeatwaves.png",                    # change this directory
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
ggsave("C:/Desktop/PhD/Teaching/Jasmine/marineHeatwaves-BAR.png",                # change this directory
       height = 4000,
       width = 6000,
       units = "px",
       dpi = 400)
