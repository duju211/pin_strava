meas_wide <- function(df_meas) {
  pivot_wider(df_meas, names_from = type, values_from = data)
}
