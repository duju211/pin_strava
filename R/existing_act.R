existing_act <- function(board_name) {
  board_register_github(repo = board_name, name = "strava_act")

  df_act_old <- pin_get("df_act", board = "strava_act")

  board_disconnect("strava_act")

  return(df_act_old)
}
