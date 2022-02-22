pin_meas <- function(act_id, active_user_id, access_token, meas_board) {
  pin_name <- paste0("df_", act_id, "_", active_user_id)
  meas <- read_activity_stream(act_id, active_user_id, access_token)
  pin_write(meas_board, meas, pin_name, type = "arrow")
}
