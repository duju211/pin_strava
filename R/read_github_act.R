read_github_act <- function(act_id, act_col_types) {
  base_url <- "https://api.github.com/repos/duju211/strava_act/contents"
  csv_url <- str_glue("{base_url}/{act_id}/data.csv?ref=master")

  r_csv <- GET(
    csv_url,
    authenticate(Sys.getenv("GITHUB_PAT"), ""),
    add_headers(Accept = "application/vnd.github.v3+json"))

  c_csv <- content(r_csv)

  dest_file <- file_temp()
  download.file(c_csv[["download_url"]], destfile = dest_file)

  read_csv(dest_file, col_types = act_col_types)
}
