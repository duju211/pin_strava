read_all_activities <- function(access_token, active_user_id) {
  activities_url <- parse_url(
    "https://www.strava.com/api/v3/athlete/activities")

  act_vec <- vector(mode = "list")
  df_act <- tibble::tibble(init = "init")
  i <- 1L

  while (nrow(df_act) != 0) {
    r <- activities_url %>%
      modify_url(
        query = list(
          access_token = access_token,
          page = i)) %>%
      GET()

    stop_for_status(r)

    df_act <- content(r, as = "text") %>%
      fromJSON(flatten = TRUE) %>%
      as_tibble()
    if (nrow(df_act) != 0)
      act_vec[[i]] <- df_act
    i <- i + 1L
  }

  act_vec %>%
    bind_rows() %>%
    mutate(
      start_date = ymd_hms(start_date),
      active_user_id = active_user_id)
}
