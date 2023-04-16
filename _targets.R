source("libraries.R")

walk(dir_ls("R"), source)

list(
  tar_target(user_list_cols, c("shoes", "clubs", "bikes")),
  tar_target(n_top, 10L),
  tar_target(rel_type, "Run"),
  tar_target(earliest_datetime, now() - dweeks(16)),

  tar_age(
    access_token, rstudioapi::askForSecret("Strava Access Token"),
    age = as.difftime(6, units = "hours")),
  tar_target(
    json_active_user, active_user_json(access_token), cue = tar_cue("always")),
  tar_target(df_active_user, active_user(json_active_user, user_list_cols)),
  tar_target(active_user_id, df_active_user[["id"]]),
  tar_target(
    user_board, connect_board(active_user_id), cue = tar_cue("always")),
  tar_target(
    df_act_raw, read_all_activities(access_token, active_user_id),
    cue = tar_cue("always")),
  tar_target(
    df_act, pre_process_act(df_act_raw, active_user_id, meas_board)),
  tar_target(pin_act, pin_write(user_board, df_act, "df_act")),
  tar_target(act_ids, rel_act_ids(df_act_raw)),
  tar_target(
    df_act_ex,
    filter(df_act, start_date >= earliest_datetime)),

  tar_target(
    pin_meas, command = {
      df_meas <- read_activity_stream(act_ids, access_token);
      pin_meas <- pin_write(user_board, df_meas, act_ids, type = "arrow");
      folder <- pin_meta(user_board, pin_meas)[["local"]][["dir"]];
      file <- pin_meta(user_board, pin_meas)[["file"]];
      path_join(c(folder, file))
    }, pattern = map(act_ids), format = "file", cue = tar_cue("never")),
  tar_target(df_meas, meas(pin_meas)),
  tar_target(gg_meas, vis_meas(df_meas)),

  tar_render(strava_report, "scrape_strava.Rmd"),
  tar_render(
    strava_post, "scrape_strava.Rmd",
    output_format = distill::distill_article(),
    output_file = "scrape_strava_post.html"),
  tar_render(
    strava_readme, "scrape_strava.Rmd", output_format = "md_document",
    output_file = "README.md", params = list(read_me = TRUE))
)
