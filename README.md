I am an avid runner and cyclist. For the past couple of years, I have
recorded almost all my activities on some kind of GPS device.

I record my runs with a Garmin device and my bike rides with a Wahoo
device, and I synchronize both accounts on Strava. I figured that it
would be nice to directly access my data from my Strava account.

In the following text, I will describe the progress to get Strava data
into R, process the data, and then create a visualization of activity
routes.

You will need the following packages:

    library(Microsoft365R)
    library(tarchetypes)
    library(conflicted)
    library(tidyverse)
    library(lubridate)
    library(jsonlite)
    library(feather)
    library(targets)
    library(httpuv)
    library(arrow)
    library(httr)
    library(pins)
    library(httr)
    library(glue)
    library(fs)

    conflict_prefer("filter", "dplyr")
    conflict_prefer("read_feather", "feather")

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

The function `define_strava_app` shown below creates the OAuth app:

<table>
<thead>
<tr class="header">
<th style="text-align: left;">name</th>
<th style="text-align: left;">command</th>
<th style="text-align: left;">pattern</th>
<th style="text-align: left;">cue_mode</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">my_app</td>
<td style="text-align: left;">define_strava_app()</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
</tbody>
</table>

    define_strava_app <- function() {
      if (Sys.getenv("STRAVA_KEY") == "" | Sys.getenv("STRAVA_SECRET") == "")
        stop(str_glue(
          "Please set system variables 'STRAVA_KEY' and 'STRAVA_SECRET' before ",
          "continuing. How you can create these variables is described here: ",
          "https://developers.strava.com/docs/getting-started/. ",
          "You can set the system variables with the `usethis::edit_r_environ` ",
          "function."))

      oauth_app(
        appname = "r_api",
        key = Sys.getenv("STRAVA_KEY"),
        secret = Sys.getenv("STRAVA_SECRET"))
    }

### Define an endpoint

Define an endpoint called `my_endpoint` using the function
`define_strava_endpoint`.

The `authorize` parameter describes the authorization url and the
`access` argument exchanges the authenticated token.

<table>
<thead>
<tr class="header">
<th style="text-align: left;">name</th>
<th style="text-align: left;">command</th>
<th style="text-align: left;">pattern</th>
<th style="text-align: left;">cue_mode</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">my_endpoint</td>
<td style="text-align: left;">define_strava_endpoint()</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
</tbody>
</table>

    define_strava_endpoint <- function() {
      oauth_endpoint(
        request = NULL,
        authorize = "https://www.strava.com/oauth/authorize",
        access = "https://www.strava.com/oauth/token")
    }

### The final authentication step

Before you can execute the following steps, you have to authenticate the
API in the web browser.

<table>
<thead>
<tr class="header">
<th style="text-align: left;">name</th>
<th style="text-align: left;">command</th>
<th style="text-align: left;">pattern</th>
<th style="text-align: left;">cue_mode</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">my_sig</td>
<td style="text-align: left;">define_strava_sig(my_endpoint, my_app)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">always</td>
</tr>
</tbody>
</table>

    define_strava_sig <- function(endpoint, app) {
      oauth2.0_token(
        endpoint, app,
        scope = "activity:read_all,activity:read,profile:read_all",
        type = NULL, use_oob = FALSE, as_header = FALSE,
        use_basic_auth = FALSE, cache = FALSE)
    }

The information in `my_sig` can now be used to access Strava data. Set
the `cue_mode` of the target to ‘always’ so that the following API calls
are always executed with an up-to-date authorization token.

## Current authenticated user

Get the id of the currently authenticated user:

<table>
<thead>
<tr class="header">
<th style="text-align: left;">name</th>
<th style="text-align: left;">command</th>
<th style="text-align: left;">pattern</th>
<th style="text-align: left;">cue_mode</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">active_user_id</td>
<td style="text-align: left;">my_sig[[“credentials”]][[“athlete”]][[“id”]]</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
</tbody>
</table>

Download information about the currently authenticated user. Map over
the `active_user_id` to make sure, that no information is overwritten.
When preprocessing the data, the columns shoes, clubs and bikes need
special attention, because they can contain multiple entries and can be
interpreted as list columns.

<table>
<colgroup>
<col style="width: 13%" />
<col style="width: 70%" />
<col style="width: 7%" />
<col style="width: 8%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">name</th>
<th style="text-align: left;">command</th>
<th style="text-align: left;">pattern</th>
<th style="text-align: left;">cue_mode</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">df_active_user</td>
<td style="text-align: left;">active_user(access_token, user_list_cols, active_user_id, meas_board)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
</tbody>
</table>

    active_user <- function(access_token, user_list_cols, active_user_id, meas_board) {
      athlete_url <- parse_url("https://www.strava.com/api/v3/athlete")

      r <- athlete_url %>%
        modify_url(
          query = list(access_token = access_token)) %>%
        GET()

      user_list <- content(r, as = "text") %>%
        fromJSON()

      df_user <- user_list[
        map_lgl(user_list, ~ !is.null(.x))
        & map_lgl(names(user_list), ~ !(.x %in% user_list_cols))] %>%
        as_tibble()

      list_cols <- user_list[names(user_list) %in% user_list_cols] %>%
        map(as_tibble)

      for (i in seq_along(list_cols)) {
        df_user[[names(list_cols)[[i]]]] <- list(list_cols[[i]])
      }
      df_user
    }

In the end there is a data frame with one row for the currently
authenticated user:

    ## # A tibble: 1 x 27
    ##         id resource_state firstname lastname city    state country sex   premium
    ##      <int>          <int> <chr>     <chr>    <chr>   <chr> <chr>   <chr> <lgl>  
    ## 1 26845822              3 "Julian " During   Baling~ Bade~ Germany M     TRUE   
    ## # ... with 18 more variables: summit <lgl>, created_at <chr>, updated_at <chr>,
    ## #   badge_type_id <int>, weight <dbl>, profile_medium <chr>, profile <chr>,
    ## #   blocked <lgl>, can_follow <lgl>, follower_count <int>, friend_count <int>,
    ## #   mutual_friend_count <int>, athlete_type <int>, date_preference <chr>,
    ## #   measurement_preference <chr>, clubs <list>, bikes <list>, shoes <list>

## Activities

Load a data frame that gives an overview of all the activities from the
data. Because the total number of activities is unknown, use a while
loop. It will break the execution of the loop if there are no more
activities to read.

<table>
<thead>
<tr class="header">
<th style="text-align: left;">name</th>
<th style="text-align: left;">command</th>
<th style="text-align: left;">pattern</th>
<th style="text-align: left;">cue_mode</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">df_act_raw</td>
<td style="text-align: left;">read_all_activities(access_token, active_user_id)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
</tbody>
</table>

    read_all_activities <- function(access_token, active_user_id) {
      activities_url <- parse_url(
        "https://www.strava.com/api/v3/athlete/activities")

      act_vec <- vector(mode = "list")
      df_act <- tibble::tibble(init = "init")
      i <- 1L

      while (nrow(df_act) != 0) {
        r <- activities_url %>%
          modify_url(
            query = list(
              access_token = access_token,
              page = i)) %>%
          GET()

        stop_for_status(r)

        df_act <- content(r, as = "text") %>%
          fromJSON(flatten = TRUE) %>%
          as_tibble()
        if (nrow(df_act) != 0)
          act_vec[[i]] <- df_act
        i <- i + 1L
      }

      act_vec %>%
        bind_rows() %>%
        mutate(
          start_date = ymd_hms(start_date),
          active_user_id = active_user_id)
    }

The resulting data frame consists of one row per activity:

    ## # A tibble: 641 x 62
    ##    resource_state name  distance moving_time elapsed_time total_elevation~ type 
    ##             <int> <chr>    <dbl>       <int>        <int>            <dbl> <chr>
    ##  1              2 "Sto~    6771.        2845         2915            106.  Run  
    ##  2              2 "Uni~   44468         6730         7321            536.  Ride 
    ##  3              2 "Feb~    6701.        2839         2850             76.4 Run  
    ##  4              2 "Fee~   32493.        4890         4997            467   Ride 
    ##  5              2 "Nig~    6736.        2678         2678             76.2 Run  
    ##  6              2 "Feb~    7815.        3136         3148             31.4 Run  
    ##  7              2 "End~    6658.        2734         2778            103.  Run  
    ##  8              2 "8K"     7548.        3083         3103            108.  Run  
    ##  9              2 "10K"   10399.        4177         4254              9.5 Run  
    ## 10              2 "Sno~   38529.        5220         6150            420.  Ride 
    ## # ... with 631 more rows, and 55 more variables: workout_type <int>, id <dbl>,
    ## #   start_date <dttm>, start_date_local <chr>, timezone <chr>,
    ## #   utc_offset <dbl>, location_city <lgl>, location_state <lgl>,
    ## #   location_country <chr>, achievement_count <int>, kudos_count <int>,
    ## #   comment_count <int>, athlete_count <int>, photo_count <int>, trainer <lgl>,
    ## #   commute <lgl>, manual <lgl>, private <lgl>, visibility <chr>,
    ## #   flagged <lgl>, gear_id <chr>, start_latlng <list>, end_latlng <list>, ...

Make sure that all ID columns have a character format and improve the
column names.

<table>
<thead>
<tr class="header">
<th style="text-align: left;">name</th>
<th style="text-align: left;">command</th>
<th style="text-align: left;">pattern</th>
<th style="text-align: left;">cue_mode</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">df_act</td>
<td style="text-align: left;">pre_process_act(df_act_raw, active_user_id, meas_board)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
</tbody>
</table>

    pre_process_act <- function(df_act_raw, active_user_id, meas_board) {
      df_act_raw %>%
        mutate(across(contains("id"), as.character)) %>%
        rename(athlete_id = `athlete.id`)
    }

Use `dplyr::pull()` to extract all activity IDs

<table>
<thead>
<tr class="header">
<th style="text-align: left;">name</th>
<th style="text-align: left;">command</th>
<th style="text-align: left;">pattern</th>
<th style="text-align: left;">cue_mode</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">act_ids</td>
<td style="text-align: left;">pull(df_act, id)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
</tbody>
</table>

## Measurements

A ‘stream’ is a nested list (JSON format) with all available
measurements of the corresponding activity.

To get the available variables and turn the result into a data frame,
define a helper function `read_activity_stream`. This function takes an
ID of an activity and an authentication token, which you created
earlier.

The target is defined with dynamic branching which maps over all
activity IDs. Define the `cue_mode` as ‘never’ to make sure that every
target runs exactly once.

<table>
<thead>
<tr class="header">
<th style="text-align: left;">name</th>
<th style="text-align: left;">command</th>
<th style="text-align: left;">pattern</th>
<th style="text-align: left;">cue_mode</th>
</tr>
</thead>
<tbody>
</tbody>
</table>

    read_activity_stream <- function(id, active_user_id, access_token) {
      act_url <- parse_url(stringr::str_glue(
        "https://www.strava.com/api/v3/activities/{id}/streams"))

      r <- modify_url(
        act_url,
        query = list(
          access_token = access_token,
          keys = str_glue(
            "distance,time,latlng,altitude,velocity_smooth,heartrate,cadence,",
            "watts,temp,moving,grade_smooth"))) %>%
        GET()

      stop_for_status(r)

      df_stream_raw <- fromJSON(content(r, as = "text"), flatten = TRUE) %>%
        as_tibble() %>%
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

Preprocess and unnest the data. The column `latlng` needs special
attention, because it contains latitude and longitude information.
Separate the two measurements before unnesting all list columns.

After this step, every row is one point in time and every column is a
measurement at this point in time (if there was any activity at that
moment).

# Visualisation

Visualize the final data by displaying the geospatial information in the
data. Every facet is one activity. Keep the rest of the plot as minimal
as possible.

<table>
<thead>
<tr class="header">
<th style="text-align: left;">name</th>
<th style="text-align: left;">command</th>
<th style="text-align: left;">pattern</th>
<th style="text-align: left;">cue_mode</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">gg_meas</td>
<td style="text-align: left;">vis_meas(df_all_meas)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
</tbody>
</table>

    vis_meas <- function(df_meas_pro) {
      df_meas_pro %>%
        filter(!is.na(lat)) %>%
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

<img src="gg_meas.png" width="2100" />

And there it is: All your Strava data in a few tidy data frames and a
nice-looking plot. Future updates to the data shouldn’t take too long,
because only measurements from new activities will be downloaded. With
all your Strava data up to date, there are a lot of appealing
possibilities for further data analyses of your fitness data.
