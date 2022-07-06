
# Load packages -----------------------------------------------------------
library(data.table)
library(tidync)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(rootSolve)


# Francesco's Function ----------------------------------------------------
mag_dur_mx <- function(x, heatWave = TRUE) {
  temp_switch <- sign(x * c(x[2:length(x)], x[length(x)]))
  if (heatWave) {
    start_index <- setdiff(which(temp_switch == 1 & x > 0), length(x))
  } else {
    start_index <- setdiff(which(temp_switch == 1 & x <= 0), length(x))
  }
  
  heat_waves <- split(start_index, cumsum(c(1, diff(start_index) != 1)))
  heat_waves <- lapply(heat_waves, function(x) c(x, max(x) + 1))
  
  if (heatWave) {
    start_index <- setdiff(which(temp_switch == 1 & x > 0), length(x))
    hot_days <- as.list(setdiff(which(x > 0), unlist(heat_waves)))
  } else {
    start_index <- setdiff(which(temp_switch == 1 & x <= 0), length(x))
    hot_days <- as.list(setdiff(which(x < 0), unlist(heat_waves)))
  }
  
  heat_waves <- c(heat_waves, hot_days)
  
  heat_wave_summary <- rbindlist(
    lapply(heat_waves, function(wave) {
      data.table(
        duration = length(wave),
        magnitude = sum(x[wave]),
        maximum = max(x[wave])
      )
    })
  )
}

# My function -------------------------------------------------------------
source("./fun_create_events.R")

# Load test dataset -------------------------------------------------------
complete_prec_nc <- tidync(x = "./data/livneh_unsplit/complete.prec.nc")

# Declare constants for test ----------------------------------------------
Ns <- c(1,10,50,100,400)
lat_v <- (complete_prec_nc %>% activate("D0") %>% hyper_array())$lat
lon_v <- (complete_prec_nc %>% activate("D1") %>% hyper_array())$lon


chosen_lon <- lon_v[190]#sample(lon_v, 1)


# Run benchmarks and record results ---------------------------------------
ns <- numeric()
extract_times <- numeric()
francescos_times <- numeric()
my_times <- numeric()

for(i in 1:length(Ns)) {
  N = Ns[i]
  chosen_lats <- tail(lat_v, N)
  
  start_time <- Sys.time()
  subset_prec_df <- (tidync(x = "./data/livneh_unsplit/complete.prec.nc") %>%
                       hyper_filter(lon = (lon == chosen_lon)) %>%
                       hyper_filter(lat = (lat %in% chosen_lats)) %>%
                       hyper_tibble())
  end_time <- Sys.time()
  
  extract_time <- end_time - start_time
  extract_times[i] <- extract_time
  
  n = subset_prec_df$lat %>% unique() %>% length()
  
  ns[i] <- n
  
  print(paste0("Extracting ", n, " points took ", extract_time %>%
                 format(format = "%M:%S",
                        digits = 2)))
  
  # Benchmark Francesco's function 1.393 seconds ---------------------------
  start_time <- Sys.time()
  francesco_prec_events_df <- subset_prec_df %>%
    group_by(interaction(lat,lon)) %>%
    summarize(mag_dur_mx(prec))
  end_time <- Sys.time()

  francescos_time <- end_time - start_time
  francescos_times[i] <- francescos_time

  print(paste0("Applying Francesco's function to ", n, " points took ", francescos_time %>%
                 format(format = "%M:%S", digits = 2)
               )
        )
  
  # Benchmark my function 2.881 seconds ------------------------------------
  start_time <- Sys.time()
  my_prec_events_df <- subset_prec_df %>%
    create_events(unique_id_coords = c("lon", "lat"),
                  metadata_coords = c("lon", "lat", "time"),
                  event_var = "prec",
                  event_func = "sum",
                  event_var_threshold = 0,
                  inequality_direction = "greater",
                  event_var_threshold_type = "absolute")
  end_time <- Sys.time()
  
  my_time <- end_time - start_time
  my_times[i] <- my_time
  
  print(paste0("Applying my function to ", n, " points took ", my_time %>%
                 format(format = "%M:%S",
                        digits = 2)))
}

# [1] "Applying Francesco's function to 282 points took 26.0609986782074"
# `summarise()` has grouped output by 'interaction(unique_id, event_number)'. You can override
# using the `.groups` argument.
# [1] "Applying my function to 282 points took 1.76835696697235" (minutes, I believe)

