all_meas <- function(board_name) {
  dir_ls(
    board_name, type = "file", regexp = "df_\\d.*\\.arrow$", recurse = TRUE) %>%
    map_df(read_arrow)
}
