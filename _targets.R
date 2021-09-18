source("libraries.R")

walk(dir_ls("R"), source)

list(
  tar_target(ex_act, "1327205128"),
  tar_target(athlete_id, "26845822"),
  tar_target(board_name, "duju211/strava_act"),
  tar_target(act_col_types, list(
    moving = col_logical(), velocity_smooth = col_number(),
    grade_smooth = col_number(), distance = col_number(),
    altitude = col_number(), heartrate = col_integer(), time = col_integer(),
    lat = col_number(), lng = col_number(), cadence = col_integer(),
    watts = col_integer())),

  tar_target(my_app, define_strava_app()),
  tar_target(my_endpoint, define_strava_endpoint()),
  tar_force(my_sig, define_strava_sig(my_endpoint, my_app), force = TRUE),
  tar_target(df_act_raw, read_all_activities(my_sig)),
  tar_target(df_act, pre_process_act(df_act_raw, athlete_id)),
  tar_target(act_ids, pull(distinct(df_act, id))),

  # Dynamic branching
  tar_target(
    df_meas, read_activity_stream(act_ids, my_sig), pattern = map(act_ids),
    cue = tar_cue(mode = "never")),

  tar_target(df_meas_all, bind_rows(df_meas)),
  tar_target(df_meas_wide, meas_wide(df_meas_all)),
  tar_target(df_meas_pro, meas_pro(df_meas_wide)),
  tar_target(gg_meas, vis_meas(df_meas_pro)),

  tar_render(strava_report, "scrape_strava.Rmd"),
  tar_render(
    strava_post, "scrape_strava.Rmd",
    output_format = distill::distill_article(),
    output_file = "scrape_strava_post.html")
)
