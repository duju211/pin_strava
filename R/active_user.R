active_user <- function(access_token, user_list_cols, active_user_id, meas_board) {
  athlete_url <- parse_url("https://www.strava.com/api/v3/athlete")

  r <- athlete_url %>%
    modify_url(
      query = list(access_token = access_token)) %>%
    GET()

  user_list <- content(r, as = "text") %>%
    fromJSON()

  df_user <- user_list[
    map_lgl(user_list, ~ !is.null(.x))
    & map_lgl(names(user_list), ~ !(.x %in% user_list_cols))] %>%
    as_tibble()

  list_cols <- user_list[names(user_list) %in% user_list_cols] %>%
    map(as_tibble)

  for (i in seq_along(list_cols)) {
    df_user[[names(list_cols)[[i]]]] <- list(list_cols[[i]])
  }
  df_user
}
