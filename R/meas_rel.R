meas_rel <- function(df_act, df_meas_pro) {
  df_act_rel <- slice(df_act, (nrow(df_act) - 604):nrow(df_act))
  df_meas_pro %>%
    semi_join(df_act_rel, by = "id")
}
