source("R/libraries.R")

loadd(my_sig)
loadd(df_act)

df_act

df_act_stream_raw <- df_act %>%
  mutate(stream = map(id, safely(~ read_activity_stream(.x, my_sig))))

df_act_stream <- df_act_stream_raw %>%
  filter(map_lgl(stream, ~ is.null(.x[["error"]]))) %>%
  transmute(id, stream, act_date = as_date(start_date)) %>%
  mutate(stream = map(stream, "result")) %>%
  tidy_streams()

df_act_meas <- df_act_stream %>%
  select(id, act_date, stream) %>%
  unnest(stream) %>%
  select(-c(original_size, resolution, series_type)) %>%
  unnest(where(purrr::is_list))

df_act_meas_nested <- df_act_meas %>%
  nest(meas = -c(id, act_date))

df_act_meas_nested

board_register_github(repo = "duju211/strava_act")

pin(df_act, "df_act", board = "github")

walk2(
  df_act_meas_nested$meas, df_act_meas_nested$id,
  ~ pin(.x, str_glue("act_{.y}"), board = "github"))

board_disconnect("github")
