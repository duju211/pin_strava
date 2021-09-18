meas_pro <- function(df_meas_wide) {
  df_meas_wide %>%
    mutate(
      lat = map_if(
        .x = latlng, .p = ~ !is.null(.x), .f = ~ .x[, 1]),
      lng = map_if(
        .x = latlng, .p = ~ !is.null(.x), .f = ~ .x[, 2])) %>%
    select(-latlng) %>%
    unnest(where(is_list))
}
