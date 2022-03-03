rel_act_ids <- function(df_act) {
  df_act %>%
    filter(!manual) %>%
    pull(id)
}
