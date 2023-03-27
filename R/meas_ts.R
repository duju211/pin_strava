meas_ts <- function(df_meas, keep_moving) {
  df_meas_pro <- df_meas |>
    select(-c(original_size, lat, lng)) |>
    pivot_longer(cols = where(is.numeric) & -c(distance, time))

  if (keep_moving) {
    filter(df_meas_pro, moving)
  } else {
    df_meas_pro
  }
}
