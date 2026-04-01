
data <- readRDS("./Objects/temp_dataset.rds")

base_model_output <- readRDS("./Objects/temp_dataset.rds")

ggplot() +
  geom_line(
    data = data,
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