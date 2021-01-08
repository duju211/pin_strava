read_meas_nested <- function(df_act_ids) {
  board_register_github(repo = "duju211/strava_act", branch = "master")

  df_act_meas_nested <- df_act_ids %>%
    anti_join(readd(df_act_meas_nested), by = c("id", "athlete.id")) %>%
    mutate(
      meas = map2(
        id, athlete.id,
        ~ pin_get(str_glue("act_{.x}_{.y}"), board = "github"))) %>%
    bind_rows(readd(df_act_meas_nested))

  board_disconnect("github")

  return(df_act_meas_nested)
}
