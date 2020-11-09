pin_new_activities <- function(df_act) {
  board_register_github(repo = "duju211/strava_act", name = "strava_act")

  pin(df_act, "df_act", board = "strava_act")

  board_disconnect("strava_act")
}
