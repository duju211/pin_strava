act_top_n_tbl <- function(df_act_top_n) {
  df_act_top_n |>
    reactable(
      selection = "multiple", defaultPageSize = nrow(df_act_top_n),
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
