act_agg <- function(df_act, agg_unit, earliest_datetime) {
  df_act |>
    filter(start_date >= earliest_datetime) |>
    as_tsibble(key = id, index = .data[[agg_unit]]) |>
    index_by(agg = .data[[agg_unit]]) |>
    group_by(type, .add = TRUE) |>
    summarise(anz = n(), .groups = "drop_last") |>
    fill_gaps(anz = 0L) |>
    ungroup()
}
