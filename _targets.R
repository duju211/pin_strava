source("libraries.R")

walk(dir_ls("R"), source)

df_poi <- tribble(
  ~target_name, ~act_type, ~start_1, ~start_2, ~finish_1, ~finish_2,
  "lochen", "Ride", c(8.852108, 48.238081), c(8.852467, 48.237931), c(8.852355, 48.218347), c(8.852677, 48.218465),
  "mess_steige", "Ride", c(9.011506, 48.208878), c(9.011577, 48.208750), c(8.999030, 48.203863), c(8.999406, 48.203921))

mapped_poi <- tar_map(
  values = df_poi, names = "target_name",
  tar_target(
    start_line,
    st_cast(st_union(st_point(start_1), st_point(start_2)), "LINESTRING")),
  tar_target(
    finish_line,
    st_cast(st_union(st_point(finish_1), st_point(finish_2)), "LINESTRING")),
  tar_target(
    df_poi_raw, poi_raw(df_meas_all, start_line, finish_line))
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
