# Load packages -----------------------------------------------------------
library(tidync)

# Load needed functions ---------------------------------------------------
source(file = "fun_create_events.R")

# Load and wrangle test file ----------------------------------------------
complete_prec_df <- tidync(x = "./data/livneh_unsplit/complete.prec.nc") %>%
  hyper_filter(lat = (lat == lat_v[100])) %>%
  #hyper_filter(lat = (lat == lat_v[100])) %>%
  hyper_tibble()

# Apply create_events function --------------------------------------------
complete_prec_events_df <- create_events(df = complete_prec_df,
                                     unique_id_coords = c("lon", "lat"),
                                     metadata_coords = c("lon", "lat", "time"),
                                     event_var = "prec",
                                     event_func = "sum",
                                     event_var_threshold = 0.95,
                                     inequality_direction = "greater",
                                     event_var_threshold_type = "percentile")


# Scratch -----------------------------------------------------------------
complete_prec_nc <- tidync(x = "./data/livneh_unsplit/complete.prec.nc")
lat_v <- (complete_prec_nc %>% activate("D0") %>% hyper_array())$lat
lon_v <- (complete_prec_nc %>% activate("D1") %>% hyper_array())$lon
