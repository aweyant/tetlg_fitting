# Load packages -----------------------------------------------------------
library(tidyverse)

# create_events Desc ------------------------------------------------------
#' INPUT
#' df: a tidy data.frame
#' metadata_coords: vector of coordinate variables which will be preserved 
#' (e.g. time, lat, lon, plev)
#' unique_id: the subset of metadata coordinates which uniquely determine
#' a location. For the above example, this would be lat, lon, and plev. In
#' the case of station-level data, this might merely be station_id.
#' event_var: the variable by which our "event" is defined (e.g. hourly
#' precipitation, if precipitation events are being studied or temperature,
#' if heatwaves are being studied)
#' event_func: the function to apply to the event_variable when events are
#' summarized (e.g. sum() would be applied to a precipitation rate over an
#' event to create a precipitation total)
#' event_var_threshold: the threshold above which a time period is
#' considered part of an event (for example, 0.1mm might be used as a
#' threshold for 6-hourly precipitation to filter out trace precip)


# create_events Code ------------------------------------------------------
create_events<- function(df,
                         metadata_coords,
                         unique_id_coords,
                         event_var,
                         event_func,
                         event_var_threshold,
                         inequality_direction = "greater") {
  if(inequality_direction == "greater") {
    df <- df %>%
      # filter out instances of is.na(event_var)
      filter(!is.na(.data[[event_var]])) %>%
      # mark time increments as during an event "active" or not; specify a
      #' column of the unique id
      mutate(active = as.numeric(.data[[event_var]] > event_var_threshold)) %>%
      # uniquely identify geographic points by the specified combination of metadata_coords
      unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
      group_by(unique_id) %>%
      # number events at each geographic point
      mutate(event_number = {r <- rle(active)
      r$values <- cumsum(r$values) * r$values
      inverse.rle(r)
      }
      ) %>%
      ungroup() %>%
      # summarize events at each point
      filter(active == 1) %>%
      group_by(interaction(unique_id, event_number)) %>%
      mutate(event_length = sum(active, na.rm = TRUE)) %>%
      summarize(
        unique_id = unique_id,
        total = sum(.data[[event_var]] - event_var_threshold),
        max_rate = max(.data[[event_var]] - event_var_threshold),
        length = min(event_length),
        event_number = min(event_number),
        #event_number = min(event_number, na.rm = TRUE),
        across({{metadata_coords}}, ~min(., na.rm = TRUE))
      ) %>%
      ungroup() %>%
      unique()
    return(df[,-1]) # return data.frame without ugly grouping variable
  }
  else {
    df <- df %>%
      # filter out instances of is.na(event_var)
      filter(!is.na(.data[[event_var]])) %>%
      # mark time increments as during an event "active" or not; specify a
      #' column of the unique id
      mutate(active = as.numeric(.data[[event_var]] < event_var_threshold)) %>%
      # uniquely identify geographic points by the specified combination of metadata_coords
      unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
      group_by(unique_id) %>%
      # number events at each geographic point
      mutate(event_number = {r <- rle(active)
      r$values <- cumsum(r$values) * r$values
      inverse.rle(r)
      }
      ) %>%
      ungroup() %>%
      # summarize events at each point
      filter(active == 1) %>%
      group_by(interaction(unique_id, event_number)) %>%
      mutate(event_length = sum(active, na.rm = TRUE)) %>%
      summarize(
        unique_id = unique_id,
        total = sum(.data[[event_var]] - event_var_threshold),
        max_rate = min(.data[[event_var]]) - event_var_threshold,
        length = min(event_length),
        event_number = min(event_number),
        #event_number = min(event_number, na.rm = TRUE),
        across({{metadata_coords}}, ~min(., na.rm = TRUE))
      ) %>%
      ungroup() %>%
      unique()
    return(df[,-1]) # return data.frame without ugly grouping variable
  }
}


# Experimental create_events code -----------------------------------------
create_events<- function(df,
                         metadata_coords,
                         unique_id_coords,
                         event_var,
                         event_func,
                         event_var_threshold,
                         inequality_direction = "greater",
                         event_var_threshold_type = "absolute") {
  if(event_var_threshold_type == "absolute") {
    if(inequality_direction == "greater") {
      df <- df %>%
        # filter out instances of is.na(event_var)
        filter(!is.na(.data[[event_var]])) %>%
        # mark time increments as during an event "active" or not; specify a
        #' column of the unique id
        mutate(active = as.numeric(.data[[event_var]] > event_var_threshold)) %>%
        # uniquely identify geographic points by the specified combination of metadata_coords
        unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
        group_by(unique_id) %>%
        # number events at each geographic point
        mutate(event_number = {r <- rle(active)
        r$values <- cumsum(r$values) * r$values
        inverse.rle(r)
        }
        ) %>%
        ungroup() %>%
        # summarize events at each point
        filter(active == 1) %>%
        group_by(interaction(unique_id, event_number)) %>%
        mutate(event_length = sum(active, na.rm = TRUE)) %>%
        summarize(
          unique_id = unique_id,
          total = sum(.data[[event_var]] - event_var_threshold),
          max_rate = max(.data[[event_var]] - event_var_threshold),
          length = min(event_length),
          event_number = min(event_number),
          #event_number = min(event_number, na.rm = TRUE),
          across({{metadata_coords}}, ~min(., na.rm = TRUE))
        ) %>%
        ungroup() %>%
        unique()
      return(df[,-1]) # return data.frame without ugly grouping variable
    }
    else {
      df <- df %>%
        # filter out instances of is.na(event_var)
        filter(!is.na(.data[[event_var]])) %>%
        # mark time increments as during an event "active" or not; specify a
        #' column of the unique id
        mutate(active = as.numeric(.data[[event_var]] < event_var_threshold)) %>%
        # uniquely identify geographic points by the specified combination of metadata_coords
        unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
        group_by(unique_id) %>%
        # number events at each geographic point
        mutate(event_number = {r <- rle(active)
        r$values <- cumsum(r$values) * r$values
        inverse.rle(r)
        }
        ) %>%
        ungroup() %>%
        # summarize events at each point
        filter(active == 1) %>%
        group_by(interaction(unique_id, event_number)) %>%
        mutate(event_length = sum(active, na.rm = TRUE)) %>%
        summarize(
          unique_id = unique_id,
          total = sum(.data[[event_var]] - event_var_threshold),
          max_rate = min(.data[[event_var]]) - event_var_threshold,
          length = min(event_length),
          event_number = min(event_number),
          #event_number = min(event_number, na.rm = TRUE),
          across({{metadata_coords}}, ~min(., na.rm = TRUE))
        ) %>%
        ungroup() %>%
        unique()
      return(df[,-1]) # return data.frame without ugly grouping variable
    }
  }
  else {
    if(inequality_direction == "greater") {
      df <- df %>%
        # filter out instances of is.na(event_var)
        filter(!is.na(.data[[event_var]])) %>%
        # uniquely identify geographic points by the specified combination of metadata_coords
        unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
        group_by(unique_id) %>%
        # mark time increments as during an event "active" or not; specify a
        #' column of the unique id
        mutate(active = as.numeric(.data[[event_var]] > quantile(x = .data[[event_var]],
                                                                 probs = event_var_threshold))) %>%
        # number events at each geographic point
        mutate(event_number = {r <- rle(active)
        r$values <- cumsum(r$values) * r$values
        inverse.rle(r)
        }
        ) %>%
        ungroup() %>%
        # summarize events at each point
        filter(active == 1) %>%
        group_by(interaction(unique_id, event_number)) %>%
        mutate(event_length = sum(active, na.rm = TRUE)) %>%
        summarize(
          unique_id = unique_id,
          total = sum(.data[[event_var]] - event_var_threshold),
          max_rate = max(.data[[event_var]] - event_var_threshold),
          length = min(event_length),
          event_number = min(event_number),
          #event_number = min(event_number, na.rm = TRUE),
          across({{metadata_coords}}, ~min(., na.rm = TRUE))
        ) %>%
        ungroup() %>%
        unique()
      return(df[,-1]) # return data.frame without ugly grouping variable
    }
    else {
      df <- df %>%
        # filter out instances of is.na(event_var)
        filter(!is.na(.data[[event_var]])) %>%
        # uniquely identify geographic points by the specified combination of metadata_coords
        unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
        group_by(unique_id) %>%
        # mark time increments as during an event "active" or not; specify a
        #' column of the unique id
        mutate(active = as.numeric(.data[[event_var]] < quantile(x = .data[[event_var]],
                                                                 probs = event_var_threshold))) %>%
        # number events at each geographic point
        mutate(event_number = {r <- rle(active)
        r$values <- cumsum(r$values) * r$values
        inverse.rle(r)
        }
        ) %>%
        ungroup() %>%
        # summarize events at each point
        filter(active == 1) %>%
        group_by(interaction(unique_id, event_number)) %>%
        mutate(event_length = sum(active, na.rm = TRUE)) %>%
        summarize(
          unique_id = unique_id,
          total = sum(.data[[event_var]] - event_var_threshold),
          max_rate = min(.data[[event_var]]) - event_var_threshold,
          length = min(event_length),
          event_number = min(event_number),
          #event_number = min(event_number, na.rm = TRUE),
          across({{metadata_coords}}, ~min(., na.rm = TRUE))
        ) %>%
        ungroup() %>%
        unique()
      return(df[,-1]) # return data.frame without ugly grouping variable
    }
  }
}
