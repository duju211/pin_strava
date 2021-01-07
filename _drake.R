source("R/libraries.R")

dir_ls("R/") %>%
  str_subset("libraries.R$", negate = TRUE) %>%
  walk(source)

strava_pin_plan <- drake_plan(
  ex_act = "1327205128",
  board_name = "duju211/strava_act",
  act_col_types = list(
    moving = col_logical(), velocity_smooth = col_number(),
    grade_smooth = col_number(), distance = col_number(),
    altitude = col_number(), heartrate = col_integer(), time = col_integer(),
    lat = col_number(), lng = col_number(), cadence = col_integer(),
    watts = col_integer()),

  my_app = define_strava_app(),
  my_endpoint = define_strava_endpoint(),
  my_sig = target(
    define_strava_sig(my_endpoint, my_app),
    trigger = trigger(condition = TRUE)),
  df_act_raw = read_all_activities(my_sig),
  df_act = pre_process_act(df_act_raw),

  df_existing_act = target(
    existing_activities(board_name),
    trigger = trigger(condition = TRUE)),

  pin_act = pin_new_activities(df_act),
  pin_result = pin_new_rides(df_act, df_existing_act, my_sig, board_name)
)

drake_config(strava_pin_plan)
