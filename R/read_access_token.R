read_access_token <- function() {
  po <- Microsoft365R::get_personal_onedrive(auth_type="device_code")
  po_board <- board_ms365(po, "strava_board")

  pin_read(po_board, "strava_sig")[["credentials"]][["access_token"]]
}
