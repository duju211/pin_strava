meas_wide <- function(df_meas) {
  df_meas %>%
    pivot_wider(names_from = type, values_from = data)
}
