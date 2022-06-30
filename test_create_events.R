# Load packages -----------------------------------------------------------
library(tidync)

# Load needed functions ---------------------------------------------------
source(file = "fun_create_events.R")


# Load and wrangle test file ----------------------------------------------
prec_1915_df <- tidync(x = "./data/livneh_unsplit/prec.1915.nc") %>%
  hyper_filter(lon = index %in% (0:2)) %>%
  hyper_filter(lon = index %in% (0:2)) %>%
  hyper_tibble()

# Apply create_events function --------------------------------------------
ev <- "prec"
uic <- c("lat", "lon")
prec_1915_df %>%  
  filter(!is.na(.data[[ev]])) %>%
  unite("unique_id", {{ uic }}, remove = FALSE) %>%
  group_by(unique_id) 

prec_1915_events_df <- create_events(df = prec_1915_df,
                                     unique_id_coords = c("lon", "lat"),
                                     metadata_coords = c("lon", "lat", "time"),
                                     event_var = "prec",
                                     event_func = "sum",
                                     event_var_threshold = 0.95,
                                     inequality_direction = "greater",
                                     event_var_threshold_type = "percentile")
