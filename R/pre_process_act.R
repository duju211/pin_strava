pre_process_act <- function(df_act_raw, active_user_id, meas_board) {
  df_act_raw %>%
    mutate(across(contains("id"), as.character)) %>%
    rename(athlete_id = `athlete.id`)
}
