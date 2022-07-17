read_all_activities <- function(access_token, active_user_id) {
  act_vec <- vector(mode = "list")
  df_act <- tibble(init = "init")
  i <- 1L

  while (nrow(df_act) != 0) {
    req <- request("https://www.strava.com/api/v3/athlete/activities") |>
      req_auth_bearer_token(token = access_token) |>
      req_url_query(page = i)

    resp <- req_perform(req)

    resp_check_status(resp)

    df_act <- resp |>
      resp_body_json(simplifyVector = TRUE) |>
      as_tibble()

    if (nrow(df_act) != 0)
      act_vec[[i]] <- df_act
    i <- i + 1L
  }

  act_vec |>
    bind_rows() |>
    mutate(
      start_date = ymd_hms(start_date),
      active_user_id = active_user_id)
}
