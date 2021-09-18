define_strava_app <- function() {
  oauth_app(
    appname = "r_api",
    key = Sys.getenv("STRAVA_KEY"),
    secret = Sys.getenv("STRAVA_SECRET"))
}
