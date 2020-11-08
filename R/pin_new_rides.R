pin_new_rides <- function(df_act, existing_act, my_sig, act_col_types) {
  df_act_new <- df_act %>%
    filter(!(id %in% existing_act))

  df_meas <- df_act_new %>%
    transmute(
      id, stream = map(id, ~ read_activity_stream(id = .x, sig = my_sig))) %>%
    tidy_streams() %>%
    unnest(stream) %>%
    select(id, where(is_list)) %>%
    unnest(where(purrr::is_list))

  df_meas_nested <- df_meas %>%
    nest(meas = -id)

  board_register_github(repo = "duju211/strava_act", name = "strava_act")

  walk2(
    df_meas_nested$meas, df_meas_nested$id,
    ~ pin(.x, str_glue("act_{.y}"), board = "strava_act"))

  board_disconnect("strava_act")
}
