lochen <- function(df_act, paths_meas, lng_min, lng_max, lat_min, lat_max) {
  act_col_types <- schema(
    moving = boolean(), velocity_smooth = double(),
    grade_smooth = double(), distance = double(),
    altitude = double(), heartrate = int32(), time = int32(),
    lat = double(), lng = double(), cadence = int32(),
    watts = int32(), id = string())

  strava_db <- open_dataset(
    paths_meas, format = "arrow", schema = act_col_types) |>
    to_duckdb()

  strava_lochen <- strava_db |>
    filter(
      id %in% local(df_act$id[df_act$type == "Ride"]),
      lng >= lng_min, lng <= lng_max, lat >= lat_min, lat <= lat_max) |>
    select(-heartrate) |>
    group_by(id) |>
    mutate(time = time - min(time)) |>
    ungroup()

  strava_lochen_wrong_direction <- strava_lochen |>
    group_by(id) |>
    filter(time == min(time)) |>
    mutate(
      wrong_direction = lat < lat_max - ((lat_max - lat_min) / 2)) |>
    filter(wrong_direction)

  strava_lochen |>
    anti_join(strava_lochen_wrong_direction, by = "id") |>
    collect()
}
