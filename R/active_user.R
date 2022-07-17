active_user <- function(json_active_user, user_list_cols) {
  json_active_user[
    map_lgl(json_active_user, negate(is.null))
    & map_lgl(names(json_active_user), ~ !.x %in% user_list_cols)] |>
    as_tibble()
}
