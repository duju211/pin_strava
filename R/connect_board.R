connect_board <- function(active_user_id) {
  if (Sys.getenv("board_remote") == "") {
    board <- board_folder(dir_create(active_user_id), versioned = FALSE)
  } else {
    od <- get_personal_onedrive()
    board <- board_ms365(od, active_user_id, versioned = FALSE)
  }

  return(board)
}
