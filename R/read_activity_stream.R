read_activity_stream <- function(id, start_date, sig, meas_board) {
  act_url <- parse_url(stringr::str_glue(
    "https://www.strava.com/api/v3/activities/{id}/streams"))
  access_token <- sig$credentials$access_token[[1]]

  r <- modify_url(
    act_url,
    query = list(
      access_token = access_token,
      keys = str_glue(
        "distance,time,latlng,altitude,velocity_smooth,heartrate,cadence,",
        "watts,temp,moving,grade_smooth"))) %>%
    GET()

  stop_for_status(r)

  df_stream_raw <- fromJSON(content(r, as = "text"), flatten = TRUE) %>%
    as_tibble() %>%
    mutate(id = id) %>%
    pivot_wider(names_from = type, values_from = data)

  if ("latlng" %in% colnames(df_stream_raw)) {
    df_stream <- df_stream_raw %>%
      mutate(
        lat = map(
          .x = latlng, .f = ~ .x[, 1]),
        lng = map(
          .x = latlng, .f = ~ .x[, 2])) %>%
      select(-latlng)
  } else {
    df_stream <- df_stream_raw
  }

  df_stream_pro <- df_stream %>%
    unnest(where(is_list)) %>%
    mutate(time = start_date + dseconds(time))

  pin_write(meas_board, df_stream_pro, id, type = "arrow")
  pin_download(meas_board, id)
}
