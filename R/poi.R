poi <- function(df_act, paths_meas, act_type,
                lng_min, lng_max, lat_min, lat_max, start_act, end_act) {
  act_col_types <- schema(
    moving = boolean(), velocity_smooth = double(),
    grade_smooth = double(), distance = double(),
    altitude = double(), heartrate = int32(), time = int32(),
    lat = double(), lng = double(), cadence = int32(),
    watts = int32(), id = string())

  rel_act <- df_act |>
    filter(type %in% act_type) |>
    pull(id)

  strava_db <- open_dataset(
    paths_meas, format = "parquet", schema = act_col_types) |>
    to_duckdb()

  strava_db_act_cand <- strava_db |>
    filter(
      lng >= lng_min, lng <= lng_max, lat >= lat_min, lat <= lat_max,
      id %in% rel_act)

  strava_db_act_sum_min_time <- strava_db_act_cand |>
    group_by(id) |>
    filter(time == min(time)) |>
    select(id, start_lat = lat, start_lng = lng)

  strava_db_act_sum_max_time <- strava_db_act_cand |>
    group_by(id) |>
    filter(time == max(time)) |>
    select(id, end_lat = lat, end_lng = lng)

  strava_db_act_sum <- strava_db_act_cand |>
    group_by(id) |>
    mutate(min_time = min(time), max_time = max(time)) |>
    summarise(
      min_lat = min(lat), max_lat = max(lat),
      min_lng = min(lng), max_lng = max(lng)) |>
    left_join(strava_db_act_sum_min_time, by = "id") |>
    left_join(strava_db_act_sum_max_time, by = "id") |>
    mutate(
      start = case_when(
        start_lat == min_lat ~ "bottom",
        start_lat == max_lat ~ "top",
        start_lng == min_lng ~ "left",
        start_lng == max_lng ~ "right"),
      end = case_when(
        end_lat == min_lat ~ "bottom",
        end_lat == max_lat ~ "top",
        end_lng == min_lng ~ "left",
        end_lng == max_lng ~ "right")) |>
    filter(start == start_act, end == end_act)

  strava_db_act_cand |>
    semi_join(strava_db_act_sum, by = "id") |>
    select(-heartrate) |>
    collect() |>
    left_join(select(df_act, id, type, start_date), by = "id")
}
