source("libraries.R")

walk(dir_ls("R"), source)

my_app <- define_strava_app()
my_endpoint <- define_strava_endpoint()
my_sig <- define_strava_sig(my_endpoint, my_app)

access_token <- my_sig[["credentials"]][["access_token"]]

Sys.setenv(STRAVA_TOKEN = access_token)
