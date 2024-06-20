connect_board <- function(active_user_id) {
  # Create google cloud folder 'strava' before running this
  board_gdrive("strava", versioned = FALSE)
}
