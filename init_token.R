source("libraries.R")

walk(dir_ls("R"), source)

token <- oauth_token()
token[["access_token"]]
