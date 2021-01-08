vis_act_meas <- function(df_act, df_act_meas) {
  df_vis <- df_act_meas %>%
    left_join(
      select(df_act, id, `athlete.id`, type), by = c("id", "athlete.id"))

  df_vis %>%
    ggplot(aes(x = lng, y = lat, color = type)) +
    geom_path() +
    facet_wrap(~ id, scales = "free") +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "bottom",
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      strip.text = element_blank()) +
    labs(color = "Type of Activity")
}
