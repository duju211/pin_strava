source("libraries.R")

walk(dir_ls("R"), source)

list(
  tar_target(act_col_types, list(
    moving = col_logical(), velocity_smooth = col_number(),
    grade_smooth = col_number(), distance = col_number(),
    altitude = col_number(), heartrate = col_integer(), time = col_integer(),
    lat = col_number(), lng = col_number(), cadence = col_integer(),
    watts = col_integer())),
  tar_target(user_list_cols, c("shoes", "clubs", "bikes")),

  tar_target(my_app, define_strava_app()),
  tar_target(my_endpoint, define_strava_endpoint()),
  tar_target(
    my_sig, define_strava_sig(my_endpoint, my_app),
    cue = tar_cue(mode = "always")),
  tar_target(active_user_id, my_sig[["credentials"]][["athlete"]][["id"]]),
  tar_target(
    df_active_user, active_user(my_sig, user_list_cols),
    pattern = map(active_user_id)),
  tar_target(
    df_act_raw, read_all_activities(my_sig, active_user_id),
    pattern = map(active_user_id)),
  tar_target(
    df_act, pre_process_act(df_act_raw), pattern = map(active_user_id)),
  tar_target(act_ids, pull(df_act, id)),
  tar_target(start_dates, pull(df_act, start_date)),
  tar_target(
    meas_board, board_folder("meas", versioned = FALSE),
    cue = tar_cue("always")),

  # Dynamic branching
  tar_target(
    df_meas, read_activity_stream(act_ids, start_dates, my_sig, meas_board),
    pattern = map(act_ids, start_dates),
    cue = tar_cue(mode = "never"), format = "file")

  # tar_target(gg_meas, vis_meas(df_meas), pattern = map(active_user_id)),
  # tar_target(gg_meas_save, save_gg_meas(gg_meas), format = "file"),
  #
  # tar_render(strava_report, "scrape_strava.Rmd"),
  # tar_render(
  #   strava_post, "scrape_strava.Rmd",
  #   output_format = distill::distill_article(),
  #   output_file = "scrape_strava_post.html"),
  # tar_render(
  #   strava_readme, "scrape_strava.Rmd", output_format = "md_document",
  #   output_file = "README.md")
)
