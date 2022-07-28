poi <- function(df_act, paths_meas,
                start_left_bottom, start_right_top,
                end_left_bottom, end_right_top) {
  act_col_types <- schema(
    moving = boolean(), velocity_smooth = double(),
    grade_smooth = double(), distance = double(),
    altitude = double(), heartrate = int32(), time = int32(),
    lat = double(), lng = double(), cadence = int32(),
    watts = int32(), id = string())

  strava_db <- open_dataset(
    paths_meas, format = "parquet", schema = act_col_types) |>
    to_duckdb()

  strava_db |>
    filter(
      lng >= lng_min, lng <= lng_max, lat >= lat_min, lat <= lat_max) |>
    select(-heartrate) |>
    collect() |>
    left_join(select(df_act, id, type, start_date), by = "id")
}
