save_gg_meas <- function(gg_meas, active_user_path) {
  gg_path <- file_create(active_user_path, "gg_meas.png")
  ggsave(gg_path, gg_meas)

  return(gg_path)
}
