poi_raw <- function(df_meas_all, start_line, finish_line) {
  df_meas_all |>
    nest(spatial_data = -id) |>
    mutate(
      id,
      multi_point = map(
        spatial_data,
        ~ st_as_sf(.x, coords = c("lng", "lat", "altitude"))),
      line = st_as_sfc(map(multi_point, st_cast, "LINESTRING")),
      inter_start = st_as_sfc(map(line, st_intersection, start_line)),
      inter_finish = st_as_sfc(map(line, st_intersection, finish_line))) |>
    filter(
      map_lgl(inter_start, negate(st_is_empty))
      & map_lgl(inter_finish, negate(st_is_empty)))
}
