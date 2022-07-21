# Load packages -----------------------------------------------------------
library(tidync)
library(tidyverse)

# Load needed functions ---------------------------------------------------
source("./fun_exponential_dts.R")
source("./create_parallel_backend.R")

# Suppress Unnecessary Warnings -------------------------------------------
options(dplyr.summarise.inform = FALSE)

# List lats and lons ------------------------------------------------------
complete_prec_nc <- tidync(x = "./data/livneh_unsplit/complete.prec.nc")
lat_v <- (complete_prec_nc %>% activate("D0") %>% hyper_array())$lat
lon_v <- (complete_prec_nc %>% activate("D1") %>% hyper_array())$lon


# Apply create_events() function ------------------------------------------
#N = 20
#for(i in 1:1){
foreach(i = (length(lon_v):1)) %dopar% {
#for(i in (length(lon_v):1)) {
  #start_time <- Sys.time()
  #for(j in seq(from = 1, to = length(lat_v) - (N-1), by = N)) {
  #print(lat_v[j:(j+(N-1))])
  complete_prec_events_df <- tidync(x = "./data/livneh_unsplit/complete.prec.nc") %>%
    hyper_filter(lon = (lon == lon_v[i])) %>%
    #hyper_filter(lat = (lat %in% lat_v[j:j+(N-1)])) %>%
    hyper_tibble() %>%
    group_by(lat,lon) %>%
    mutate(threshold = quantile(prec, 0.95)) %>%
    filter(prec > threshold) %>%
    mutate(exceedance = prec-threshold) %>%
    summarize(p_val = exponential_dts(exceedance)) %>%
    write_csv(file = "./data/dts_exceedance.csv", append = TRUE)
  gc()
  # end_time <- Sys.time()
  # print(end_time-start_time)
  #}
  #print(paste0("Progress: ", signif(100 * i/length(lon_v), digits = 2), "%"))
}

parallel::stopCluster(my.cluster)


# Rename columns ----------------------------------------------------------
dts_exceedance <- read_csv("./data/dts_exceedance.csv",
                           col_names = c("lat","lon","p_val"))

write_csv(x = dts_exceedance, file = "./data/dts_exceedance.csv")




