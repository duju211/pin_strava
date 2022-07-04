source("libraries.R")

walk(dir_ls("R"), source)

dir_create("data")
dir_create("poi")

df_poi <- tribble(
  ~target_name, ~target_file, ~act_type, ~lng_min, ~lng_max, ~lat_min, ~lat_max,
  "lochen", "poi/lochen.rds", "Ride", 8.843454, 8.859889, 48.21787, 48.23242)

mapped_poi <- tar_map(
  df_poi, names = "target_name",
  tar_target(df_poi_raw, poi(df_act, meas, lng_min, lng_max, lat_min, lat_max)),
  tar_target(
    poi_file, command = {
      write_rds(df_poi_raw, target_file);
      return(target_file)
  }, format = "file")
)

list(
  tar_target(user_list_cols, c("shoes", "clubs", "bikes")),
  tar_target(access_token, read_access_token(), cue = tar_cue("always")),
  tar_target(meas_path, dir_create("meas")),
  tar_target(act_path, dir_create("act")),
  tar_target(user_path, dir_create("user")),
  tar_target(poi_path, dir_create("poi")),

  tar_target(active_user_id, df_active_user[["id"]]),
  tar_target(
    df_active_user, active_user(access_token, user_list_cols, meas_board),
    cue = tar_cue("always")),
  tar_target(
    df_act_raw, read_all_activities(access_token, active_user_id),
    cue = tar_cue("always")),
  tar_target(
    df_act, pre_process_act(df_act_raw, active_user_id, meas_board)),
  tar_target(act_ids, rel_act_ids(df_act)),

  tar_target(
    meas, command = {
      file_path <- paste0(meas_path, "/meas_", act_ids, "_", active_user_id);
      df_meas <- read_activity_stream(act_ids, active_user_id, access_token);
      write_feather(df_meas, file_path);
      file_path
    }, pattern = map(act_ids), cue = tar_cue("never")),
  tar_target(meas_files, command = {meas_path; dir_ls(meas_path)}),
  tar_target(
    act, command = {
      act_path <- paste0("data/act_", active_user_id);
      write_feather(df_act, act_path);
      act_path
    }, format = "file"),
  mapped_poi,
  tar_target(df_meas_all, meas_all(meas)),
  tar_target(gg_meas, vis_meas(df_meas_all)),
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
