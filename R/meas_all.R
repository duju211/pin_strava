meas_all <- function(paths_meas) {
  act_col_types <- schema(
    moving = boolean(), velocity_smooth = double(),
    grade_smooth = double(), distance = double(),
    altitude = double(), heartrate = int32(), time = int32(),
    lat = double(), lng = double(), cadence = int32(),
    watts = int32(), id = string())

  open_dataset(paths_meas, format = "parquet", schema = act_col_types) |>
    to_duckdb() |>
    select(id, lat, lng, altitude, time) |>
    filter(!is.na(lat) & !is.na(lng)) |>
    collect() |>
    mutate(point = st_as_sfc(map2(lng, lat, ~ st_point(c(.x, .y)))))
}
