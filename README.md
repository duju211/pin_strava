I am an avid runner and cyclist. For the past couple of years, I have
recorded almost all my activities on some kind of GPS device.

I record my runs with a Garmin device and my bike rides with a Wahoo
device, and I synchronize both accounts on Strava. I figured that it
would be nice to directly access my data from my Strava account.

In the following text, I will describe the progress to get Strava data
into R, process the data, and then create a visualization of activity
routes.

You will need the following packages:

    library(tarchetypes)
    library(conflicted)
    library(tidyverse)
    library(lubridate)
    library(jsonlite)
    library(targets)
    library(httpuv)
    library(duckdb)
    library(httr2)
    library(arrow)
    library(pins)
    library(glue)
    library(fs)
    library(sf)

    conflict_prefer("filter", "dplyr")
    conflict_prefer("lag", "dplyr")

# Data

The whole data pipeline is implemented with the help of the `targets`
package. You can learn more about the package and its functionalities
[here](https://docs.ropensci.org/targets/).

In order to reproduce the analysis, perform the following steps:

-   Clone the repository: <https://github.com/duju211/pin_strava>
-   Install the packages listed in the `libraries.R` file
-   Run the target pipeline by executing `targets::tar_make()` command
-   Follow the instructions printed in the console

## Target Plan

We will go through the most important targets in detail.

## OAuth Dance from R

The Strava API requires an ‘OAuth dance’, described below.

### Create an OAuth Strava app

To get access to your Strava data from R, you must first create a Strava
API. The steps are documented on the [Strava Developer
site](https://developers.strava.com/docs/getting-started/). While
creating the app, you’ll have to give it a name. In my case, I named it
`r_api`.

After you have created your personal API, you can find your Client ID
and Client Secret variables in the [Strava API
settings](https://www.strava.com/settings/api). Save the Client ID as
STRAVA\_KEY and the Client Secret as STRAVA\_SECRET in your R
environment.

<aside>
You can edit your R environment by running `usethis::edit_r_environ()`,
saving the keys, and then restarting R.
</aside>

    STRAVA_KEY=<Client ID>
    STRAVA_SECRET=<Client Secret>

The information in `my_sig` can now be used to access Strava data. Set
the `cue_mode` of the target to ‘always’ so that the following API calls
are always executed with an up-to-date authorization token.

## Current authenticated user

Download information about the currently authenticated user. When
preprocessing the data, the columns shoes, clubs and bikes need special
attention, because they can contain multiple entries and can be
interpreted as list columns.

    active_user <- function(json_active_user, user_list_cols) {
      json_active_user[
        map_lgl(json_active_user, negate(is.null))
        & map_lgl(names(json_active_user), ~ !.x %in% user_list_cols)] |>
        as_tibble()
    }

In the end there is a data frame with one row for the currently
authenticated user:

    ## # A tibble: 1 × 26
    ##         id resource_state firstname lastname city    state country sex   premium
    ##      <int>          <int> <chr>     <chr>    <chr>   <chr> <chr>   <chr> <lgl>  
    ## 1 26845822              3 "Julian " During   Baling… Bade… Germany M     FALSE  
    ## # … with 17 more variables: summit <lgl>, created_at <chr>, updated_at <chr>,
    ## #   badge_type_id <int>, weight <dbl>, profile_medium <chr>, profile <chr>,
    ## #   blocked <lgl>, can_follow <lgl>, follower_count <int>, friend_count <int>,
    ## #   mutual_friend_count <int>, athlete_type <int>, date_preference <chr>,
    ## #   measurement_preference <chr>, is_winback_via_upload <lgl>,
    ## #   is_winback_via_view <lgl>

## Activities

Load a data frame that gives an overview of all the activities from the
data. Because the total number of activities is unknown, use a while
loop. It will break the execution of the loop if there are no more
activities to read.

    read_all_activities <- function(access_token, active_user_id) {
      act_vec <- vector(mode = "list")
      df_act <- tibble(init = "init")
      i <- 1L

      while (nrow(df_act) != 0) {
        req <- request("https://www.strava.com/api/v3/athlete/activities") |>
          req_auth_bearer_token(token = access_token) |>
          req_url_query(page = i)

        resp <- req_perform(req)

        resp_check_status(resp)

        df_act <- resp |>
          resp_body_json(simplifyVector = TRUE) |>
          as_tibble()

        if (nrow(df_act) != 0)
          act_vec[[i]] <- df_act
        i <- i + 1L
      }

      act_vec |>
        bind_rows() |>
        mutate(
          start_date = ymd_hms(start_date),
          active_user_id = active_user_id)
    }

The resulting data frame consists of one row per activity:

    ## # A tibble: 738 × 57
    ##    resource_state athlete$id name              distance moving_time elapsed_time
    ##             <int>      <int> <chr>                <dbl>       <int>        <int>
    ##  1              2   26845822 "Zeit Verbrechen…    4628.        2030         2165
    ##  2              2   26845822 "Bauerfeind rett…    4595.        1926         1938
    ##  3              2   26845822 "Cui Bono"           4560.        1972         1996
    ##  4              2   26845822 "Semi Final Run"     4620.        1897         1934
    ##  5              2   26845822 "Too Many Tabs"      5794.        2778         2991
    ##  6              2   26845822 "Hard Fork ChatG…    5129.        2396         2465
    ##  7              2   26845822 "Invisible Run "     4195         1747         1775
    ##  8              2   26845822 "Costa Run"          4322.        1823         1823
    ##  9              2   26845822 "Pre Casper "        4550.        1897         1907
    ## 10              2   26845822 "Advent 🏃 "         5422.        2379         2385
    ## # … with 728 more rows, and 52 more variables: athlete$resource_state <int>,
    ## #   total_elevation_gain <dbl>, type <chr>, sport_type <chr>,
    ## #   workout_type <int>, id <dbl>, start_date <dttm>, start_date_local <chr>,
    ## #   timezone <chr>, utc_offset <dbl>, location_city <lgl>,
    ## #   location_state <lgl>, location_country <chr>, achievement_count <int>,
    ## #   kudos_count <int>, comment_count <int>, athlete_count <int>,
    ## #   photo_count <int>, map <df[,3]>, trainer <lgl>, commute <lgl>, …

Make sure that all ID columns have a character format and improve the
column names.

    pre_process_act <- function(df_act_raw, active_user_id, meas_board) {
      df_act_raw |>
        mutate(across(contains("id"), as.character))
    }

Extract ids of all activities. Exclude activities which were recorded
manually, because they don’t include additional data:

    rel_act_ids <- function(df_act) {
      df_act %>%
        filter(!manual) %>%
        pull(id)
    }

## Measurements

A ‘stream’ is a nested list (JSON format) with all available
measurements of the corresponding activity.

To get the available variables and turn the result into a data frame,
define a helper function `read_activity_stream`. This function takes an
ID of an activity and an authentication token, which you created
earlier.

Preprocess and unnest the data in this function. The column `latlng`
needs special attention, because it contains latitude and longitude
information. Separate the two measurements before unnesting all list
columns.

    read_activity_stream <- function(id, access_token) {
      req <- request("https://www.strava.com/api/v3/activities") |>
        req_auth_bearer_token(token = access_token) |>
        req_url_query(keys = str_glue(
          "distance,time,latlng,altitude,velocity_smooth,heartrate,cadence,",
          "watts,temp,moving,grade_smooth")) |>
        req_url_path_append(id) |>
        req_url_path_append("streams")

      resp <- req_perform(req)

      resp_check_status(resp)

      df_stream_raw <- resp |>
        resp_body_json(simplifyVector = TRUE) |>
        as_tibble() |>
        mutate(id = id) %>%
        pivot_wider(names_from = type, values_from = data)

      if ("latlng" %in% colnames(df_stream_raw)) {
        df_stream <- df_stream_raw %>%
          mutate(
            lat = map(
              .x = latlng, .f = ~ .x[, 1]),
            lng = map(
              .x = latlng, .f = ~ .x[, 2])) %>%
          select(-latlng)
      } else {
        df_stream <- df_stream_raw
      }

      df_stream %>%
        unnest(where(is_list)) %>%
        mutate(id = id)
    }

Do this for every id and save the resulting data frames as `feather`
file. By doing so we can later effectively query the data.

<aside>
The name of the board is determined by the currently logged in user and
will have a different name, if you run the pipeline.
</aside>

# Visualisation

Visualize the final data by displaying the geospatial information in the
data. Join all the activities into one data frame. To do this, get the
paths to all the measurement files:

    meas_paths <- function(board_name) {
      dir_ls(
        board_name, type = "file", regexp = "df_\\d.*\\.arrow$", recurse = TRUE)
    }

Insert them all into a duckdb and select relevant columns:

    meas_all <- function(paths_meas) {
      act_col_types <- schema(
        moving = boolean(), velocity_smooth = double(),
        grade_smooth = double(), distance = double(),
        altitude = double(), heartrate = int32(), time = int32(),
        lat = double(), lng = double(), cadence = int32(),
        watts = int32(), id = string())

      open_dataset(paths_meas, format = "parquet", schema = act_col_types) |>
        to_duckdb() |>
        select(id, lat, lng, altitude, time) |>
        filter(!is.na(lat) & !is.na(lng)) |>
        collect()
    }

    ## # A tibble: 2,545,904 × 5
    ##    id           lat   lng altitude  time
    ##    <chr>      <dbl> <dbl>    <dbl> <int>
    ##  1 8271787410  48.2  9.02     763.     0
    ##  2 8271787410  48.2  9.02     763.     1
    ##  3 8271787410  48.2  9.02     763.     7
    ##  4 8271787410  48.2  9.02     763.    13
    ##  5 8271787410  48.2  9.02     763.    14
    ##  6 8271787410  48.2  9.02     763.    15
    ##  7 8271787410  48.2  9.02     763.    16
    ##  8 8271787410  48.2  9.02     763.    17
    ##  9 8271787410  48.2  9.02     763.    18
    ## 10 8271787410  48.2  9.02     763.    19
    ## # … with 2,545,894 more rows

In the final plot every facet is one activity. Keep the rest of the plot
as minimal as possible.

    vis_meas <- function(df_meas_pro) {
      df_meas_pro %>%
        ggplot(aes(x = lng, y = lat)) +
        geom_path() +
        facet_wrap(~ id, scales = "free") +
        theme(
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "bottom",
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          strip.text = element_blank())
    }

![](README_files/figure-markdown_strict/gg_strava-1.png)

And there it is: All your Strava data in a few tidy data frames and a
nice-looking plot. Future updates to the data shouldn’t take too long,
because only measurements from new activities will be downloaded. With
all your Strava data up to date, there are a lot of appealing
possibilities for further data analyses of your fitness data.
