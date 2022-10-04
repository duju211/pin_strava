source("libraries.R")

walk(dir_ls("R"), source)

df_poi <- tribble(
  ~target_name, ~act_type, ~start_left_bottom, ~start_right_top, ~end_left_bottom, ~end_right_top,
  "lochen", "Ride", c(48.237931, 8.852108), c(48.238081, 8.852586), c(48.218262, 8.852467), c(48.218394, 8.852634))

mapped_poi <- tar_map(
  df_poi, names = "target_name",
  tar_target(
    poi_file, command = {
      poi_file <- file_create(poi_path, target_name);
      df_poi_raw <- poi(
        df_act, meas, start_left_bottom, start_right_top,
        end_left_bottom, end_right_top);
      write_rds(df_poi_raw, poi_file);
      return(poi_file)
  }, format = "file")
)

list(
  tar_target(user_list_cols, c("shoes", "clubs", "bikes")),
  tar_age(
    access_token, rstudioapi::askForSecret("Strava Access Token"),
    age = as.difftime(6, units = "hours")),

  tar_target(
    json_active_user, active_user_json(access_token), cue = tar_cue("always")),
  tar_target(df_active_user, active_user(json_active_user, user_list_cols)),
  tar_target(active_user_id, df_active_user[["id"]]),
  tar_target(active_user_path, dir_create(active_user_id)),
  tar_target(meas_path, dir_create(active_user_path, "meas")),
  tar_target(act_path, dir_create(active_user_path, "act")),
  tar_target(user_path, dir_create(active_user_path, "user")),
  tar_target(poi_path, dir_create(active_user_path, "poi")),
  tar_target(
    df_act_raw, read_all_activities(access_token, active_user_id),
    cue = tar_cue("always")),
  tar_target(
    df_act, pre_process_act(df_act_raw, active_user_id, meas_board)),
  tar_target(act_ids, rel_act_ids(df_act)),

  tar_target(
    meas, command = {
      stream_path <- file_create(meas_path, act_ids);
      df_stream <- read_activity_stream(act_ids, access_token);
      write_parquet(df_stream, stream_path);
      return(stream_path)
    },
    pattern = map(act_ids), cue = tar_cue("never"), format = "file"),
  #mapped_poi,
  tar_target(df_meas_all, meas_all(meas)),
  tar_target(gg_meas, vis_meas(df_meas_all)),
  tar_target(png_meas, save_gg_meas(gg_meas, active_user_path)),

  tar_render(strava_report, "scrape_strava.Rmd"),
  tar_render(
    strava_post, "scrape_strava.Rmd",
    output_format = distill::distill_article(),
    output_file = "scrape_strava_post.html"),
  tar_render(
    strava_readme, "scrape_strava.Rmd", output_format = "md_document",
    output_file = "README.md")
)
