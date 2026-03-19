library(tidyverse)

# raw data
base = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-31/"
ocean_temp_deploy <- read_csv(paste0(base, "ocean_temperature_deployments.csv"))
ocean_temp <- read_csv(paste0(base, "ocean_temperature.csv"))

# join and rename
d = ocean_temp |>
    left_join(
        ocean_temp_deploy,
        join_by(between(date, start_date, end_date))
    ) |>
    mutate(yday = yday(date)) |>
    select(
        date,
        yday,
        latitude,
        longitude,
        depth_lowtide = sensor_depth_at_low_tide_m,
        mean_temp = mean_temperature_degree_c,
        sd_temp = sd_temperature_degree_c,
        n_obs,
        deploy_id = deployment_id
    ) 

# estimate a background trend
m = mgcv::gam(mean_temp ~ s(yday), data = d)
d_avg = tibble(
    yday = 1:366,
    mean_temp = predict(m, newdata = tibble(yday = 1:366))
)