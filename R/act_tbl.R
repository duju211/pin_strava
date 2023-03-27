act_tbl <- function(df_act) {
  df_act |>
    reactable(
      filterable = TRUE, selection = "multiple",
      columns = list(
        distance = colDef(
          cell = data_bars(df_act)
        )
      ))
}
