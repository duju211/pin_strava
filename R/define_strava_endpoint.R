define_strava_endpoint <- function() {
  oauth_endpoint(
    request = NULL,
    authorize = "https://www.strava.com/oauth/authorize",
    access = "https://www.strava.com/oauth/token")
}
