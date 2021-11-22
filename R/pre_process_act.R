pre_process_act <- function(df_act_raw, athlete_id) {
  df_act <- df_act_raw %>%
    mutate(across(contains("id"), as.character)) %>%
    rename(athlete_id = `athlete.id`)
}
