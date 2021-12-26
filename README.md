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
    library(httpuv)
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
<col style="width: 87%" />
<col style="width: 5%" />
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
<td style="text-align: left;">df_active_user</td>
<td style="text-align: left;">active_user(my_sig)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
<tr class="even">
<td style="text-align: left;">active_user_id</td>
<td style="text-align: left;">first(pull(df_active_user, id))</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
<tr class="odd">
<td style="text-align: left;">df_act_raw</td>
<td style="text-align: left;">read_all_activities(my_sig, active_user_id)</td>
<td style="text-align: left;">map(active_user_id)</td>
<td style="text-align: left;">thorough</td>
</tr>
<tr class="even">
<td style="text-align: left;">df_act</td>
<td style="text-align: left;">pre_process_act(df_act_raw)</td>
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
<td style="text-align: left;">df_meas_rel</td>
<td style="text-align: left;">meas_rel(df_act, df_meas_pro)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
<tr class="even">
<td style="text-align: left;">df_meas_norm</td>
<td style="text-align: left;">meas_norm(df_meas_pro)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
<tr class="odd">
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

<table style="width:100%;">
<colgroup>
<col style="width: 13%" />
<col style="width: 52%" />
<col style="width: 23%" />
<col style="width: 10%" />
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
<td style="text-align: left;">df_act_raw</td>
<td style="text-align: left;">read_all_activities(my_sig, active_user_id)</td>
<td style="text-align: left;">map(active_user_id)</td>
<td style="text-align: left;">thorough</td>
</tr>
</tbody>
</table>

    read_all_activities <- function(sig, active_user_id) {
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

      act_vec %>%
        bind_rows() %>%
        mutate(
          start_date = ymd_hms(start_date), active_user_id = active_user_id)
    }

The resulting data frame consists of one row per activity:

    ## # A tibble: 619 x 61
    ##    resource_state name  distance moving_time elapsed_time total_elevation~ type 
    ##             <int> <chr>    <dbl>       <int>        <int>            <dbl> <chr>
    ##  1              2 "Chr~    5620.        2330         2398             45.1 Run  
    ##  2              2 "Chr~    7601.        3251         3279            108.  Run  
    ##  3              2 "Las~    6727         2879         2879            106.  Run  
    ##  4              2 "Mit~   65485.       11124        11835            944.  Ride 
    ##  5              2 "Fog~    7442.        3105         3351            108.  Run  
    ##  6              2 "Fog~    7482.        3122         3131             79.3 Run  
    ##  7              2 "Spa~   15512.       14448        16006            468   Walk 
    ##  8              2 "Fes~   32422.        5508         5668            490   Ride 
    ##  9              2 "Lau~    7348.        2853         2853              7.4 Run  
    ## 10              2 "Adv~    6830.        2994         3101             43.2 Run  
    ## # ... with 609 more rows, and 54 more variables: workout_type <int>, id <dbl>,
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
<td style="text-align: left;">pre_process_act(df_act_raw)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">thorough</td>
</tr>
</tbody>
</table>

    pre_process_act <- function(df_act_raw) {
      df_act_raw %>%
        mutate(across(contains("id"), as.character)) %>%
        rename(athlete_id = `athlete.id`)
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
            "distance,time,latlng,altitude,velocity_smooth,heartrate,cadence,",
            "watts,temp,moving,grade_smooth"))) %>%
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

    ## # A tibble: 4,941 x 6
    ##    type            data            series_type original_size resolution id      
    ##    <chr>           <list>          <chr>               <int> <chr>      <chr>   
    ##  1 moving          <lgl [298]>     distance              298 high       6428676~
    ##  2 latlng          <dbl [298 x 2]> distance              298 high       6428676~
    ##  3 velocity_smooth <dbl [298]>     distance              298 high       6428676~
    ##  4 grade_smooth    <dbl [298]>     distance              298 high       6428676~
    ##  5 cadence         <int [298]>     distance              298 high       6428676~
    ##  6 distance        <dbl [298]>     distance              298 high       6428676~
    ##  7 altitude        <dbl [298]>     distance              298 high       6428676~
    ##  8 heartrate       <int [298]>     distance              298 high       6428676~
    ##  9 time            <int [298]>     distance              298 high       6428676~
    ## 10 moving          <lgl [385]>     distance              385 high       6421879~
    ## # ... with 4,931 more rows

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

    ## # A tibble: 619 x 15
    ##    series_type original_size resolution id         moving latlng velocity_smooth
    ##    <chr>               <int> <chr>      <chr>      <list> <list> <list>         
    ##  1 distance              298 high       6428676365 <lgl ~ <dbl ~ <dbl [298]>    
    ##  2 distance              385 high       6421879706 <lgl ~ <dbl ~ <dbl [385]>    
    ##  3 distance              338 high       6417412687 <lgl ~ <dbl ~ <dbl [338]>    
    ##  4 distance            11112 high       6403300429 <lgl ~ <dbl ~ <dbl [11,112]> 
    ##  5 distance              378 high       6397751426 <lgl ~ <dbl ~ <dbl [378]>    
    ##  6 distance              373 high       6384464449 <lgl ~ <dbl ~ <dbl [373]>    
    ##  7 distance             4988 high       6371131703 <lgl ~ <dbl ~ <dbl [4,988]>  
    ##  8 distance             5504 high       6355067791 <lgl ~ <dbl ~ <dbl [5,504]>  
    ##  9 distance              347 high       6345504872 <lgl ~ <dbl ~ <dbl [347]>    
    ## 10 distance              394 high       6332901406 <lgl ~ <dbl ~ <dbl [394]>    
    ## # ... with 609 more rows, and 8 more variables: grade_smooth <list>,
    ## #   cadence <list>, distance <list>, altitude <list>, heartrate <list>,
    ## #   time <list>, temp <list>, watts <list>

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

    ## # A tibble: 2,210,279 x 13
    ##    id    moving velocity_smooth grade_smooth cadence distance altitude heartrate
    ##    <chr> <lgl>            <dbl>        <dbl>   <dbl>    <dbl>    <dbl>     <dbl>
    ##  1 6428~ FALSE             0            -1.6      76      0       520        101
    ##  2 6428~ TRUE              1.95         -1.1      75     11.7     520.       104
    ##  3 6428~ TRUE              1.72         -1.2      75     18.9     520.       107
    ##  4 6428~ TRUE              1.96         -1        75     35.2     520.       110
    ##  5 6428~ TRUE              2.69         -1        75     51.2     519.       113
    ##  6 6428~ TRUE              2.9          -1.2      74     70       519.       115
    ##  7 6428~ TRUE              2.23         -1        76     91.4     519        117
    ##  8 6428~ TRUE              2.23         -0.8      77    112.      519.       119
    ##  9 6428~ TRUE              2.76         -0.7      37    133.      519.       121
    ## 10 6428~ TRUE              2.96         -0.5      75    154.      518.       122
    ## # ... with 2,210,269 more rows, and 5 more variables: time <dbl>, temp <int>,
    ## #   watts <dbl>, lat <dbl>, lng <dbl>

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
