poi <- function(df_act, paths_meas, target_file, act_type,
                lng_min, lng_max, lat_min, lat_max) {
  act_col_types <- schema(
    moving = boolean(), velocity_smooth = double(),
    grade_smooth = double(), distance = double(),
    altitude = double(), heartrate = int32(), time = int32(),
    lat = double(), lng = double(), cadence = int32(),
    watts = int32(), id = string())

  strava_db <- open_dataset(
    paths_meas, format = "arrow", schema = act_col_types) |>
    to_duckdb()

  df_strava_poi <- strava_db |>
    filter(
      id %in% local(df_act$id[df_act$type == act_type]),
      lng >= lng_min, lng <= lng_max, lat >= lat_min, lat <= lat_max) |>
    select(-heartrate) |>
    collect()
}
