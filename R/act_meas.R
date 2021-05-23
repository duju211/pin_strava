act_meas <- function(board_name, pin_result) {
  board_register_github(repo = board_name, name = board_name, branch = "master")

  df_act_meas_nested <- pin_find(board = board_name) %>%
    filter(str_detect(name, "^act")) %>%
    mutate(meas = map(name, ~ pin_get(name = .x, board = board_name)))

  df_act_meas <- df_act_meas_nested %>%
    select(name, meas) %>%
    filter(map_lgl(meas, ~ is_tibble(.x))) %>%
    unnest(meas)

  board_disconnect(board_name)

  df_act_meas
}
