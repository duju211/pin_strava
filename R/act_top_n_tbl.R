act_top_n_tbl <- function(df_act, n_top, rel_type) {
  df_act_top_n <- df_act |>
    filter(type == rel_type) |>
    top_n(n = n_top, wt = distance) |>
    select(id, name, distance) |>
    arrange(desc(distance)) |>
    mutate(distance = distance / 1000)

  df_act_top_n |>
    reactable(
      selection = "multiple", defaultPageSize = n_top,
      columns = list(
        distance = colDef(
          format = colFormat(digits = 1),
          aggregate = "sum", align = "left",
          style = function(value) {
            bar_style(width = value / max(df_act_top_n$distance))
          })
      )
    )
}
