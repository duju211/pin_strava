meas_paths <- function(board_name) {
  dir_ls(
    board_name, type = "file", regexp = "df_\\d.*\\.arrow$", recurse = TRUE)
}
