save_gg_meas <- function(gg_meas) {
  ggsave("data/gg_meas.png", gg_meas)

  return("data/gg_meas.png")
}
