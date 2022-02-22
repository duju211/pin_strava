meas_files <- function(meas) {
  children <- tar_meta(meas) %>%
    pull(children) %>%
    first()

  children_regex <- glue_collapse(children, "|")

  dir_ls("_targets/objects/") %>%
    str_subset(children_regex)
}
