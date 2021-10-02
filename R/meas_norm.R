meas_norm <- function(df_meas_pro) {
  df_meas_pro |>
    group_by(id) |>
    mutate(across(
      c(lat, lng),
      ~ (.x - min(.x, na.rm = TRUE)) /
        (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)))) |>
    ungroup() |>
    mutate(
      heartrate =
        (heartrate - (min(heartrate, na.rm = TRUE))) /
        (max(heartrate, na.rm = TRUE) - min(heartrate, na.rm = TRUE)))
}
