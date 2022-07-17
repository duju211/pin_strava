active_user_json <- function(access_token) {
  req <- request("https://www.strava.com/api/v3/athlete") |>
    req_auth_bearer_token(token = access_token)

  resp <- req_perform(req)

  resp |>
    resp_body_json()
}
