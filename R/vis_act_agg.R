vis_act_agg <- function(df_act_agg) {
  df_act_agg |>
    ggplot(aes(x = agg, y = anz, fill = type)) +
    geom_col() +
    scale_x_date(
      labels  = date_format(format = "W%W-%y"), date_breaks = "week") +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) +
    labs(x = "Date", y = "Number of Activities", fill = "Activity Type")
}
