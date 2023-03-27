vis_meas_ts <- function(df_meas_vis) {
  df_meas_vis |>
    ggplot(aes(x = distance, y = value, color = id)) +
    geom_line() +
    facet_wrap(~ name, ncol = 1, scales = "free_y") +
    theme_minimal() +
    labs(x = "Distance", y = "Value") +
    theme(legend.position = "none")
}
