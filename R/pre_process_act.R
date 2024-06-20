pre_process_act <- function(df_act_raw, active_user_id) {
  df_act_raw |>
    mutate(across(contains("id"), as.character), week = yearweek(start_date)) |>
    arrange(desc(start_date))
}
