read_activity_stream <- function(id, access_token) {
  req <- request("https://www.strava.com/api/v3/activities") |>
    req_auth_bearer_token(token = access_token) |>
    req_url_query(keys = str_glue(
      "distance,time,latlng,altitude,velocity_smooth,heartrate,cadence,",
      "watts,temp,moving,grade_smooth")) |>
    req_url_path_append(id) |>
    req_url_path_append("streams")

  resp <- req_perform(req)

  resp_check_status(resp)

  df_stream_raw <- resp |>
    resp_body_json(simplifyVector = TRUE) |>
    as_tibble() |>
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

  df_stream %>%
    unnest(where(is_list)) %>%
    mutate(id = id)
}
