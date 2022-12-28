act_top_n <- function(df_act, n_top, rel_type) {
  df_act |>
    filter(type == rel_type) |>
    top_n(n = n_top, wt = distance) |>
    select(id, name, distance) |>
    arrange(desc(distance)) |>
    mutate(distance = distance / 1000)
}
