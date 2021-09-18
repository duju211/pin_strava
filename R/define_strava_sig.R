define_strava_sig <- function(endpoint, app) {
  oauth2.0_token(
    endpoint, app,
    scope = "activity:read_all,activity:read,profile:read_all",
    type = NULL, use_oob = FALSE, as_header = FALSE,
    use_basic_auth = FALSE, cache = FALSE)
}
