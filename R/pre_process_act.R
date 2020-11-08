pre_process_act <- function(df_act_raw) {
  df_act_raw %>%
    mutate(across(contains("id"), as.character))
}
