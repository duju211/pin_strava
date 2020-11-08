source("R/libraries.R")

loadd(my_sig)
loadd(df_act)

df_act

df_act_stream_raw <- df_act %>%
  mutate(stream = map(id, safely(~ read_activity_stream(.x, my_sig))))

df_act_stream <- df_act_stream_raw %>%
  filter(map_lgl(stream, ~ is.null(.x[["error"]]))) %>%
  transmute(id, stream, `athlete.id`, act_date = as_date(start_date)) %>%
  mutate(stream = map(stream, "result")) %>%
  tidy_streams()

df_act_meas <- df_act_stream %>%
  select(id, act_date, `athlete.id`, stream) %>%
  unnest(stream) %>%
  select(-c(original_size, resolution, series_type)) %>%
  unnest(where(purrr::is_list))

df_act_meas_nested <- df_act_meas %>%
  nest(meas = -c(id, act_date, `athlete.id`))

df_act_meas_nested

board_register_github(repo = "duju211/strava_act")

pin(df_act, "df_act", board = "github")

pwalk(
  list(
    m = df_act_meas_nested$meas, id = df_act_meas_nested$id,
    a_id = df_act_meas_nested$`athlete.id`),
  function(m, id, a_id) pin(m, str_glue("act_{id}_{a_id}"), board = "github"))

board_disconnect("github")
