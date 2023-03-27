rel_act_ids <- function(df_act_raw) {
  df_act_raw |>
    filter(!manual) |>
    pull(id)
}
