pre_process_act <- function(df_act_raw, active_user_id, meas_board) {
  df_act_raw |>
    mutate(across(contains("id"), as.character)) |>
    select(name, id, start_date, distance, total_elevation_gain, type) |>
    arrange(desc(start_date))
}
