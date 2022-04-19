source("libraries.R")

walk(dir_ls("R"), source)

my_app <- define_strava_app()
my_endpoint <- define_strava_endpoint()
my_sig <- define_strava_sig(my_endpoint, my_app)

po <- Microsoft365R::get_personal_onedrive()
po_board <- board_ms365(po, path = "strava_board", versioned = FALSE)

pin_write(po_board, my_sig, "strava_sig")
