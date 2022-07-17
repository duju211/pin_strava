strava_oauth_token <- function() {
  if (Sys.getenv("STRAVA_KEY") == "" | Sys.getenv("STRAVA_SECRET") == "")
    stop(str_glue(
      "Please set system variables 'STRAVA_KEY' and 'STRAVA_SECRET' before ",
      "continuing. How you can create these variables is described here: ",
      "https://developers.strava.com/docs/getting-started/. ",
      "You can set the system variables with the `usethis::edit_r_environ` ",
      "function."))

  strava_client <- oauth_client(
    id = Sys.getenv("STRAVA_KEY"),
    token_url = "https://www.strava.com/oauth/token",
    secret = Sys.getenv("STRAVA_SECRET"), name = "r_api")

  oauth_flow_auth_code(
    client = strava_client,
    auth_url = "https://www.strava.com/oauth/authorize",
    scope = "activity:read_all,activity:read,profile:read_all")
}
