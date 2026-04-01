000o0oo0oo0oooo0oo0rm(list = ls()) # reset

library(StrathE2E2)
library(tidyverse)
 library(future)
 library(furrr)

plan(multisession, workers = 12)

# function for a single month perturbation
run_month <- function(i) {

  model <- e2e_read("North_Sea", "2003-2013")

  model[["data"]][["physics.drivers"]][["so_temp"]][i] <-
    model[["data"]][["physics.drivers"]][["so_temp"]][i] + 5

  model[["data"]][["physics.drivers"]][["si_temp"]][i] <-
    model[["data"]][["physics.drivers"]][["si_temp"]][i] + 5

  model[["data"]][["physics.drivers"]][["d_temp"]][i] <-
    model[["data"]][["physics.drivers"]][["d_temp"]][i] + 5

  results <- e2e_run(model = model, nyears = 10) # will increase the years but just wanted to get it running 

  results[["final.year.outputs"]][["mass_results_wholedomain"]][1:31, ] %>%
    mutate(Month = i)
}

## run
res <- future_map_dfr(
  1:12,
  run_month,
  .options = furrr_options(seed = TRUE)
)

saveRDS(res,"C:/Users/xvb24200/OneDrive - University of Strathclyde/Marine_heatwaves/Objects/northsea2003_2013.RDS")

model_base <- e2e_read("North_Sea","2003-2013")
results_base <- e2e_run(model = model_base,nyears = 1)
base <- results_base[["final.year.outputs"]][["mass_results_wholedomain"]][1:31, ]
res <- readRDS("C:/Users/xvb24200/OneDrive - University of Strathclyde/Marine_heatwaves/Objects/northsea2003_2013.RDS") # change this directory
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

ggsave("C:/Users/xvb24200/OneDrive - University of Strathclyde/Marine_heatwaves/Results/northsea2003_2013.png",
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

ggsave("C:/Users/xvb24200/OneDrive - University of Strathclyde/Marine_heatwaves/Results/northsea2003_2013-BAR.png",
       height = 4000,
       width = 6000,
       units = "px",
       dpi = 400)
