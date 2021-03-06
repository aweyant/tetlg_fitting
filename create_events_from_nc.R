# Load packages -----------------------------------------------------------
library(tidync)

# Load needed functions ---------------------------------------------------
source(file = "fun_create_events.R")
source(file = "create_parallel_backend.R")

# Suppress Unnecessary Warnings -------------------------------------------
options(dplyr.summarise.inform = FALSE)



# List lats and lons ------------------------------------------------------
complete_prec_nc <- tidync(x = "./data/livneh_unsplit/complete.prec.nc")
lat_v <- (complete_prec_nc %>% activate("D0") %>% hyper_array())$lat
lon_v <- (complete_prec_nc %>% activate("D1") %>% hyper_array())$lon


# Apply create_events() function ------------------------------------------
#N = 20
foreach(i = (length(lon_v):1)) %dopar% {
#for(i in 1:1) {
  start_time <- Sys.time()
  #for(j in seq(from = 1, to = length(lat_v) - (N-1), by = N)) {
    #print(lat_v[j:(j+(N-1))])
    tidync(x = "./data/livneh_unsplit/complete.prec.nc") %>%
      hyper_filter(lon = (lon == lon_v[i])) %>%
      #hyper_filter(lat = (lat %in% lat_v[j:j+(N-1)])) %>%
      hyper_tibble() %>%
      create_events(unique_id_coords = c("lon", "lat"),
                    metadata_coords = c("lon", "lat", "time"),
                    event_var = "prec",
                    event_func = "sum",
                    event_var_threshold = 0.95,
                    inequality_direction = "greater",
                    event_var_threshold_type = "percentile") %>%
      write_csv(file = "./data/livneh_unsplit/complete.prec.events.csv", append = TRUE)
    gc()
    end_time <- Sys.time()
    print(end_time-start_time)
  #}
  print(paste0("Progress: ", signif(100 * i/length(lon_v), digits = 2), "%"))
}

parallel::stopCluster(cl = my.cluster)


# Rename columns ----------------------------------------------------------
complete_prec_events_df <- read_csv("./data/livneh_unsplit/complete.prec.events.csv",
                                    col_names = c("unique_id",
                                                  "event_var_threshold",
                                                  "total",
                                                  "max_rate",
                                                  "length",
                                                  "event_number",
                                                  "lon", "lat", "time")) %>%
  na.omit() %>%
  # filter(str_detect(unique_id, regex("[0-9]{3}\\.[0-9]*[_][0-9]+\\.[0-9]*"))) %>%
  filter(lat > 27.0, lat < 52.0, lon > 235.0, lon < 248.0) %>%
  unite("unique_id", lon, lat, sep = "_", remove = FALSE)

write_csv(x = complete_prec_events_df,
          file = "./data/livneh_unsplit/complete_prec_events.csv")




