pre_process_act <- function(df_act_raw, board_name) {
  board_register_github(repo = board_name, name = "strava_act")

  df_act_old <- pin_get("df_act", board = "strava_act")

  df_act <- df_act_raw %>%
    mutate(across(contains("id"), as.character)) %>%
    bind_rows(df_act_old) %>%
    distinct(id, `athlete.id`, .keep_all = TRUE)

  pin("df_act", board = "strava_act")

  board_disconnect("strava_act")

  return(df_act)
}
