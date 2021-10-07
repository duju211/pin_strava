I am a vivid runner and cyclist. Since a couple of years, I’m recording
almost all my activities with some kind of GPS device.

I record my runs with a Garmin device and my bike rides with a Wahoo
device. Both accounts get synchronized with my Strava account. I figured
that it would be nice to directly access my data from my Strava account.

In the following text, I will describe the progress to get the data into
R.

In this analysis, the following packages are used:

    library(tarchetypes)
    library(conflicted)
    library(tidyverse)
    library(lubridate)
    library(jsonlite)
    library(targets)
    library(httr)
    library(pins)
    library(httr)
    library(fs)

    conflict_prefer("filter", "dplyr")

# Data

The whole data pipeline is implemented with the help of the `targets`
package. [Here](https://docs.ropensci.org/targets/) you can learn more
about the package and its functionalities.

## Target Plan

The manifest of the target plan looks like this:

<table>
<colgroup>
<col style="width: 4%" />
<col style="width: 89%" />
<col style="width: 3%" />
<col style="width: 2%" />
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
<td style="text-align: left;">my_app</td>
<td style="text-align: left;">define_strava_app()</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
<tr class="even">
<td style="text-align: left;">my_endpoint</td>
<td style="text-align: left;">define_strava_endpoint()</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
<tr class="odd">
<td style="text-align: left;">act_col_types</td>
<td style="text-align: left;">list(moving = col_logical(), velocity_smooth = col_number(), grade_smooth = col_number(), distance = col_number(), altitude = col_number(), heartrate = col_integer(), time = col_integer(), lat = col_number(), lng = col_number(), cadence = col_integer(), watts = col_integer())</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
<tr class="even">
<td style="text-align: left;">my_sig</td>
<td style="text-align: left;">define_strava_sig(my_endpoint, my_app)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">always</td>
</tr>
<tr class="odd">
<td style="text-align: left;">df_act_raw</td>
<td style="text-align: left;">read_all_activities(my_sig)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
<tr class="even">
<td style="text-align: left;">df_act</td>
<td style="text-align: left;">pre_process_act(df_act_raw, athlete_id)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
<tr class="odd">
<td style="text-align: left;">act_ids</td>
<td style="text-align: left;">pull(distinct(df_act, id))</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
<tr class="even">
<td style="text-align: left;">df_meas</td>
<td style="text-align: left;">read_activity_stream(act_ids, my_sig)</td>
<td style="text-align: left;">map(act_ids)</td>
<td style="text-align: left;">never</td>
</tr>
<tr class="odd">
<td style="text-align: left;">df_meas_all</td>
<td style="text-align: left;">bind_rows(df_meas)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
<tr class="even">
<td style="text-align: left;">df_meas_wide</td>
<td style="text-align: left;">meas_wide(df_meas_all)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
<tr class="odd">
<td style="text-align: left;">df_meas_pro</td>
<td style="text-align: left;">meas_pro(df_meas_wide)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
<tr class="even">
<td style="text-align: left;">gg_meas</td>
<td style="text-align: left;">vis_meas(df_meas_pro)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
<tr class="odd">
<td style="text-align: left;">df_meas_norm</td>
<td style="text-align: left;">meas_norm(df_meas_pro)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
<tr class="even">
<td style="text-align: left;">gg_meas_save</td>
<td style="text-align: left;">save_gg_meas(gg_meas)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
</tbody>
</table>

The most important targets of the plan are described in detail in the
following subsections.

## OAuth Dance from R

To get access to your Strava data from R, you have to create a Strava
api. How to do this is documented
[here](https://developers.strava.com/docs/getting-started/).

The Strava api requires a so called OAuth dance. How this can be done
from within R is described in the following section.

Create an OAuth Strava app:

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
      oauth_app(
        appname = "r_api",
        key = Sys.getenv("STRAVA_KEY"),
        secret = Sys.getenv("STRAVA_SECRET"))
    }

You can find your `STRAVA_KEY` and `STRAVA_SECRET` variables under the
Strava api settings after you have created your own personal api. The
name of api is determined during creation. In my case I named it
`r_api`.

Define an endpoint:

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

The `authorize` parameter describes the authorization url. And the
`access` argument is used to exchange the authenticated token.

The final authentication step. Before the user can execute the following
steps, he has to authenticate the api in the web browser.

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
the `cue_mode` of the target to ‘always’, so that the user has to
authenticate and the following api calls are all executed with an up to
date authorization token.

## Activities

We are now authenticated and can directly access Strava data. At first
load an overview table of all available activities. Because the total
number of activities is unknown, use a while loop. Break the execution
of the loop, if there are no more activities to read.

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
<td style="text-align: left;">read_all_activities(my_sig)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
</tbody>
</table>

    read_all_activities <- function(sig) {
      activities_url <- parse_url(
        "https://www.strava.com/api/v3/athlete/activities")

      act_vec <- vector(mode = "list")
      df_act <- tibble::tibble(init = "init")
      i <- 1L

      while (nrow(df_act) != 0) {
        r <- activities_url %>%
          modify_url(
            query = list(
              access_token = sig$credentials$access_token[[1]],
              page = i)) %>%
          GET()

        df_act <- content(r, as = "text") %>%
          fromJSON(flatten = TRUE) %>%
          as_tibble()
        if (nrow(df_act) != 0)
          act_vec[[i]] <- df_act
        i <- i + 1L
      }

      df_activities <- act_vec %>%
        bind_rows() %>%
        mutate(start_date = ymd_hms(start_date))
    }

The resulting data frame consists of one row per activity:

    ## # A tibble: 597 x 60
    ##    resource_state name  distance moving_time elapsed_time total_elevation~ type 
    ##             <int> <chr>    <dbl>       <int>        <int>            <dbl> <chr>
    ##  1              2 "Sho~  32580.         5646         6027             329. Ride 
    ##  2              2 "Mit~  33862.         5293         6958             372  Ride 
    ##  3              2 "Mon~  28069.         4195         6650             391  Ride 
    ##  4              2 "Aft~  41243.         5962         5962             606. Ride 
    ##  5              2 "Aut~  51372.         7313         8020             556  Ride 
    ##  6              2 "Wee~  55641          9037        10610             923. Ride 
    ##  7              2 "Bre~  43892.         5594         5721             606  Ride 
    ##  8              2 "Rad~  18244.         3454        35536             300. Ride 
    ##  9              2 "Sta~  28529.         4214        11105             370  Ride 
    ## 10              2 "Abe~     26.5           3            3               0  Ride 
    ## # ... with 587 more rows, and 53 more variables: workout_type <int>, id <dbl>,
    ## #   external_id <chr>, upload_id <dbl>, start_date <dttm>,
    ## #   start_date_local <chr>, timezone <chr>, utc_offset <dbl>,
    ## #   start_latlng <list>, end_latlng <list>, location_city <lgl>,
    ## #   location_state <lgl>, location_country <chr>, start_latitude <dbl>,
    ## #   start_longitude <dbl>, achievement_count <int>, kudos_count <int>,
    ## #   comment_count <int>, athlete_count <int>, photo_count <int>, ...

Preprocess activities. Make sure that all id columns are represented as
characters and improve the column names:

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
<td style="text-align: left;">pre_process_act(df_act_raw, athlete_id)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
</tbody>
</table>

    pre_process_act <- function(df_act_raw, athlete_id) {
      df_act <- df_act_raw %>%
        mutate(
          across(contains("id"), as.character),
          `athlete.id` = athlete_id)
    }

Extract all ids of the activities:

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
<td style="text-align: left;">pull(distinct(df_act, id))</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
</tbody>
</table>

## Measurements

Read the ‘stream’ data from Strava. A ‘stream’ is a nested list (json
format) with all available measurements of the corresponding activity.

To get all available variables and turn the result into a data frame,
define a helper function. This function takes an id of an activity and
an authentication token, which we have created earlier.

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
<td style="text-align: left;">df_meas</td>
<td style="text-align: left;">read_activity_stream(act_ids, my_sig)</td>
<td style="text-align: left;">map(act_ids)</td>
<td style="text-align: left;">never</td>
</tr>
</tbody>
</table>

    read_activity_stream <- function(id, sig) {
      act_url <- parse_url(stringr::str_glue(
        "https://www.strava.com/api/v3/activities/{id}/streams"))
      access_token <- sig$credentials$access_token[[1]]

      r <- modify_url(
        act_url,
        query = list(
          access_token = access_token,
          keys = str_glue(
            "distance,time,latlng,altitude,velocity_smooth,heartrate,cadence,watts,
            temp,moving,grade_smooth"))) %>%
        GET()

      stop_for_status(r)

      fromJSON(content(r, as = "text"), flatten = TRUE) %>%
        as_tibble() %>%
        mutate(id = id)
    }

The target is defined with dynamic branching which maps over all
activity ids. Define the cue mode as `never` to make sure, that every
target runs exactly once.

Bind the single targets into one data frame:

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
<td style="text-align: left;">df_meas_all</td>
<td style="text-align: left;">bind_rows(df_meas)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
</tbody>
</table>

The data now is represented by one row per measurement series:

    ## # A tibble: 4,761 x 6
    ##    type            data              series_type original_size resolution id    
    ##    <chr>           <list>            <chr>               <int> <chr>      <chr> 
    ##  1 moving          <lgl [5,587]>     distance             5587 high       60735~
    ##  2 latlng          <dbl [5,587 x 2]> distance             5587 high       60735~
    ##  3 velocity_smooth <dbl [5,587]>     distance             5587 high       60735~
    ##  4 grade_smooth    <dbl [5,587]>     distance             5587 high       60735~
    ##  5 distance        <dbl [5,587]>     distance             5587 high       60735~
    ##  6 altitude        <dbl [5,587]>     distance             5587 high       60735~
    ##  7 time            <int [5,587]>     distance             5587 high       60735~
    ##  8 moving          <lgl [5,281]>     distance             5281 high       60572~
    ##  9 latlng          <dbl [5,281 x 2]> distance             5281 high       60572~
    ## 10 velocity_smooth <dbl [5,281]>     distance             5281 high       60572~
    ## # ... with 4,751 more rows

Turn the data into a wide format:

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
<td style="text-align: left;">df_meas_wide</td>
<td style="text-align: left;">meas_wide(df_meas_all)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
</tbody>
</table>

    meas_wide <- function(df_meas) {
      pivot_wider(df_meas, names_from = type, values_from = data)
    }

In this format every activity is one row again:

    ## # A tibble: 597 x 14
    ##    series_type original_size resolution id         moving        latlng velocity_smooth
    ##    <chr>               <int> <chr>      <chr>      <list>        <list> <list>         
    ##  1 distance             5587 high       6073551706 <lgl [5,587]> <dbl ~ <dbl [5,587]>  
    ##  2 distance             5281 high       6057232328 <lgl [5,281]> <dbl ~ <dbl [5,281]>  
    ##  3 distance             4186 high       6038252742 <lgl [4,186]> <dbl ~ <dbl [4,186]>  
    ##  4 distance             5811 high       6028456784 <lgl [5,811]> <dbl ~ <dbl [5,811]>  
    ##  5 distance             7262 high       6002615359 <lgl [7,262]> <dbl ~ <dbl [7,262]>  
    ##  6 distance             9045 high       5975328478 <lgl [9,045]> <dbl ~ <dbl [9,045]>  
    ##  7 distance             5607 high       5947271836 <lgl [5,607]> <dbl ~ <dbl [5,607]>  
    ##  8 distance             3460 high       5944515311 <lgl [3,460]> <dbl ~ <dbl [3,460]>  
    ##  9 distance             4234 high       5936333308 <lgl [4,234]> <dbl ~ <dbl [4,234]>  
    ## 10 distance                4 high       5936332751 <lgl [4]>     <dbl ~ <dbl [4]>      
    ## # ... with 587 more rows, and 7 more variables: grade_smooth <list>,
    ## #   distance <list>, altitude <list>, time <list>, heartrate <list>,
    ## #   cadence <list>, watts <list>

Preprocess and unnest the data. The column `latlng` needs special
attention, because it contains latitude and longitude information.
Separate the two measurements before unnesting all list columns.

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
<td style="text-align: left;">df_meas_pro</td>
<td style="text-align: left;">meas_pro(df_meas_wide)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
</tbody>
</table>

    meas_pro <- function(df_meas_wide) {
      df_meas_wide %>%
        mutate(
          lat = map_if(
            .x = latlng, .p = ~ !is.null(.x), .f = ~ .x[, 1]),
          lng = map_if(
            .x = latlng, .p = ~ !is.null(.x), .f = ~ .x[, 2])) %>%
        select(-c(latlng, original_size, resolution, series_type)) %>%
        unnest(where(is_list))
    }

After this step every row is one point in time and every column is (if
present) a measurement at this point in time.

    ## # A tibble: 2,139,787 x 12
    ##    id         moving velocity_smooth grade_smooth distance altitude  time heartrate
    ##    <chr>      <lgl>            <dbl>        <dbl>    <dbl>    <dbl> <int>     <int>
    ##  1 6073551706 FALSE              0            0        0       554.     0        NA
    ##  2 6073551706 TRUE               0            0.7      3.3     554.     1        NA
    ##  3 6073551706 TRUE               3.3          1        6.5     554.     2        NA
    ##  4 6073551706 TRUE               4.5          1       13.6     555.     3        NA
    ##  5 6073551706 TRUE               5            0.9     20.1     555.     4        NA
    ##  6 6073551706 TRUE               4.7          1.1     23.6     555.     5        NA
    ##  7 6073551706 TRUE               4.9          0.6     27.8     555.     6        NA
    ##  8 6073551706 TRUE               5            0.5     31.3     555.     7        NA
    ##  9 6073551706 TRUE               4.7          1       36.9     555.     8        NA
    ## 10 6073551706 TRUE               4.4          0.5     42       555.     9        NA
    ## # ... with 2,139,777 more rows, and 4 more variables: cadence <int>,
    ## #   watts <int>, lat <dbl>, lng <dbl>

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
<td style="text-align: left;">vis_meas(df_meas_pro)</td>
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
