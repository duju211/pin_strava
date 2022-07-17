source("libraries.R")

walk(dir_ls("R"), source)

token <- strava_oauth_token()
token[["access_token"]]
