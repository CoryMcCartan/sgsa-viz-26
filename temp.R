library(tidyverse)
library(geomtextpath)
library(wacolors)

base = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-31/"
ocean_temp_deploy <- read_csv(paste0(base, "ocean_temperature_deployments.csv"))
ocean_temp <- read_csv(paste0(base, "ocean_temperature.csv"))

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

m = mgcv::gam(mean_temp ~ s(yday), data = d)
d_avg = tibble(
    yday = 1:366,
    mean_temp = predict(m, newdata = tibble(yday = 1:366))
)

ggplot(d, aes(yday, mean_temp, color = -depth_lowtide, group = depth_lowtide)) +
    facet_wrap(~ year(date)) +
    geom_line() +
    geom_textline(
        aes(yday, mean_temp),
        data = d_avg,
        inherit.aes = FALSE,
        label = "Average trend",
        lty = "21",
        lwd = 0.6,
        hjust = 0.55,
        vjust = -0.1,
        size = 3.0,
        text_smoothing = 20
    ) +
    geom_textvline(
        aes(xintercept = yday(date), label = deploy_id),
        size = 2.5,
        hjust = 0.975,
        vjust = 1.2,
        lwd = 0.25,
        data = summarize(d, date = min(date), .by = deploy_id)
    ) +
    scale_color_wa_c(
        "sea",
        name = "Depth\nat low tide",
        which = 5:15,
        labels = \(x) scales::number(-x, suffix = "m")
    ) +
    scale_y_continuous(
        "Mean daily temperature (°C)",
        limits = c(0, 25),
        expand = c(0, 0)
    ) +
    scale_x_continuous("Day of year", breaks = seq(0, 360, 90)) +
    theme_bw() +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.direction = "vertical",
        legend.position = "inside",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.25 * 1/4, "npc"),
        legend.title.position = "left",
        legend.title = element_text(hjust=1, vjust=1), 
        legend.justification.inside = "center",
        legend.position.inside = c(5 / 6, 1 / 6),
    )

m = mgcv::gam(
    mean_temp ~ factor(deploy_id) +
        factor(depth_lowtide) +
        s(yday, by = factor(depth_lowtide)),
    data = d
)
d$loc_effect = predict(m, type="terms")[, 1]

d_loc = count(d, latitude, longitude, loc_effect, wt = n_obs) |> 
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
mapgl::mapboxgl_view(
    d_loc,
    column = "loc_effect",
    n = 5,
    palette = colorRampPalette(wacolors$vantage)
)
