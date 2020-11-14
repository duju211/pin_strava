pin_new_rides <- function(df_act, df_existing_act, my_sig, board_name) {
  df_act_new <- df_act %>%
    anti_join(df_existing_act, by = c("id", "athlete.id"))

  df_meas <- df_act_new %>%
    transmute(
      id, `athlete.id`,
      stream = map(id, ~ read_activity_stream(id = .x, sig = my_sig))) %>%
    tidy_streams() %>%
    unnest(stream) %>%
    select(id, `athlete.id`, where(is_list)) %>%
    unnest(where(purrr::is_list))

  df_meas_nested <- df_meas %>%
    nest(meas = -c(id, `athlete.id`))

  board_register_github(repo = board_name, name = board_name)

  pwalk(
    list(
      m = df_meas_nested$meas, id = df_meas_nested$id,
      a_id = df_meas_nested$`athlete.id`),
    function(m, id, a_id)
      pin(m, str_glue("act_{id}_{a_id}"), board = board_name))

  board_disconnect(board_name)
}
