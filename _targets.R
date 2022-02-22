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
  tar_target(board_name, paste0("strava_data_", active_user_id)),
  tar_target(meas_board, board_folder(board_name, versioned = FALSE)),

  tar_target(my_app, define_strava_app()),
  tar_target(my_endpoint, define_strava_endpoint()),
  tar_target(
    my_sig, define_strava_sig(my_endpoint, my_app),
    cue = tar_cue(mode = "always")),
  tar_target(active_user_id, my_sig[["credentials"]][["athlete"]][["id"]]),
  tar_target(access_token, my_sig[["credentials"]][["access_token"]]),
  tar_target(
    df_active_user,
    active_user(access_token, user_list_cols, active_user_id, meas_board)),
  tar_target(
    pin_user, pin_write(meas_board, df_active_user, "df_user", type = "arrow")),
  tar_target(df_act_raw, read_all_activities(access_token, active_user_id)),
  tar_target(
    df_act, pre_process_act(df_act_raw, active_user_id, meas_board)),
  tar_target(pin_act, pin_write(meas_board, df_act, "df_act", type = "arrow")),
  tar_target(act_ids, pull(df_act, id)),

  tar_target(
    meas_pinned_local,
    pin_meas(act_ids, active_user_id, access_token, meas_board),
    pattern = map(act_ids), cue = tar_cue("never")),
  tar_target(df_all_meas, all_meas(board_name)),
  tar_target(gg_meas, vis_meas(df_all_meas)),
  tar_target(png_meas, save_gg_meas(gg_meas)),

  tar_render(strava_report, "scrape_strava.Rmd"),
  tar_render(
    strava_post, "scrape_strava.Rmd",
    output_format = distill::distill_article(),
    output_file = "scrape_strava_post.html"),
  tar_render(
    strava_readme, "scrape_strava.Rmd", output_format = "md_document",
    output_file = "README.md")
)
