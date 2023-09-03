meas <- function(pin_meas) {
  open_dataset(pin_meas, format = "parquet") |>
    to_duckdb() |>
    arrange(time) |>
    select(id, lat, lng) |>
    distinct() |>
    collect()
}
