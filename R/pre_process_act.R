pre_process_act <- function(df_act_raw, active_user_id, meas_board) {
  df_act_raw |>
    rename(athlete_id = `athlete.id`) |>
    mutate(
      across(contains("id"), as.character),
      id_name = str_glue("{id}_{athlete_id}"))
}
