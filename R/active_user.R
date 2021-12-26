active_user <- function(sig) {
  list_cols <- c("shoes", "clubs", "bikes")

  athlete_url <- parse_url("https://www.strava.com/api/v3/athlete")

  r <- athlete_url %>%
    modify_url(
      query = list(
        access_token = sig$credentials$access_token[[1]])) %>%
    GET()

  user_list <- content(r, as = "text") %>%
    fromJSON(flatten = TRUE)

  df_user <- user_list[
    map_lgl(user_list, ~ !is.null(.x))
    & map_lgl(names(user_list), ~ !(.x %in% list_cols))] %>%
    as_tibble()

  list_list_cols <- user_list[names(user_list) %in% list_cols] %>%
    map(as_tibble)

  for (i in seq_along(list_list_cols)) {
    df_user[[names(list_list_cols)[[i]]]] <- list(list_list_cols[[i]])
  }

  return(df_user)
}