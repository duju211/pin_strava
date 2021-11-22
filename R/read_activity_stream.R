read_activity_stream <- function(id, sig) {
  act_url <- parse_url(stringr::str_glue(
    "https://www.strava.com/api/v3/activities/{id}/streams"))
  access_token <- sig$credentials$access_token[[1]]

  r <- modify_url(
    act_url,
    query = list(
      access_token = access_token,
      keys = str_glue(
        "distance,time,latlng,altitude,velocity_smooth,heartrate,cadence,",
        "watts,temp,moving,grade_smooth"))) %>%
    GET()

  stop_for_status(r)

  fromJSON(content(r, as = "text"), flatten = TRUE) %>%
    as_tibble() %>%
    mutate(id = id)
}
