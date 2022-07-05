# Load packages -----------------------------------------------------------
library(tidync)

# Load needed functions ---------------------------------------------------
source(file = "fun_create_events.R")

# Suppress Unnecessary Warnings -------------------------------------------
options(dplyr.summarise.inform = FALSE)



# List lats and lons ------------------------------------------------------
complete_prec_nc <- tidync(x = "./data/livneh_unsplit/complete.prec.nc")
lat_v <- (complete_prec_nc %>% activate("D0") %>% hyper_array())$lat
lon_v <- (complete_prec_nc %>% activate("D1") %>% hyper_array())$lon


# Apply create_events() function ------------------------------------------
N = 100
for(i in 1:length(lon_v)) {
  for(j in seq(from = 1, to = length(lat_v) - (N-1), by = N)) {
    complete_prec_events_df <- tidync(x = "./data/livneh_unsplit/complete.prec.nc") %>%
      hyper_filter(lon = (lon == lon_v[i])) %>%
      hyper_filter(lat = (lat %in% lat_v[j:j+15])) %>%
      hyper_tibble() %>%
      create_events(unique_id_coords = c("lon", "lat"),
                    metadata_coords = c("lon", "lat", "time"),
                    event_var = "prec",
                    event_func = "sum",
                    event_var_threshold = 0.95,
                    inequality_direction = "greater",
                    event_var_threshold_type = "percentile") %>%
      write_csv(file = "./data/livneh_unsplit/complete.prec.events.csv", append = TRUE)
  }
  print(paste0("Progress: ", signif(100 * i/length(lon_v), digits = 2), "%"))
}


# Rename columns ----------------------------------------------------------
colnames(complete_prec_events_df) <- c("unique_id",
                                       "total",
                                       "max_rate",
                                       "length",
                                       "event_number",
                                       "lon", "lat", "time")

write_csv(x = complete_prec_events_df,
          file = "./data/livneh_unsplit/complete_prec_events.csv")




