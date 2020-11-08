existing_activities <- function(board_name) {
  board_register_github(repo = board_name, name = "strava_act")

  df_all_pins_raw <- pin_find(board = "strava_act")

  df_all_pins <- df_all_pins_raw %>%
    distinct(name) %>%
    mutate(name = str_remove(name, "^act_")) %>%
    filter(str_detect(name, "^\\d*$")) %>%
    pull()

  board_disconnect("strava_act")

  return(df_all_pins)
}
