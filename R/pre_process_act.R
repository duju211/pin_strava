pre_process_act <- function(df_act_raw, df_act_old) {
  df_act <- df_act_raw %>%
    mutate(across(contains("id"), as.character)) %>%
    bind_rows(df_act_old) %>%
    distinct(id, `athlete.id`, .keep_all = TRUE)
}
