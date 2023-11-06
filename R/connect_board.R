connect_board <- function(active_user_id) {
  po <- get_personal_onedrive()

  board_ms365(po, "strava_board", versioned = FALSE)
}
