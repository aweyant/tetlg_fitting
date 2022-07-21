#' ---
#' title: "DTS Goodness of Fit Test for Event Max Distrubtion"
#' output: github_document
#' ---
#'
#'
#' First, the setup (boring)

# Load packages -----------------------------------------------------------
library(tidyverse)
library(twosamples)

# Load auxiliary functions ------------------------------------------------
source("./gtetlg_utils.R")
source("./fun_dts_wrapper.R")
source("./create_parallel_backend_12.R")


# Load files --------------------------------------------------------------
complete_prec_events_df <- read_csv("./data/livneh_unsplit/complete_prec_events.csv",
                                    lazy = FALSE) %>%
  rename(max = max_rate) %>%
  ungroup()


generated_prec_events_df <- read_csv("./data/livneh_unsplit/generated_prec_events.csv",
                                     lazy = FALSE) %>%
  #rename(gen_length = event_length,
  #       gen_total = event_magnitude,
  #       gen_max = event_max) %>%
  group_by(unique_id) %>%
  mutate(event_number = row_number()) %>%
  ungroup()

# Event total ------------------------------------------------------------
gc()
unique_id_v <- intersect(complete_prec_events_df$unique_id %>% unique(),
                         generated_prec_events_df$unique_id %>% unique())

lon_v <- complete_prec_events_df$lat %>% unique()
lat_v <- complete_prec_events_df$lon %>% unique()

unique_id_chunks <- split(unique_id_v,
                          ceiling(seq_along(unique_id_v)/500))

combined_events_df <- complete_prec_events_df %>%
  left_join(generated_prec_events_df %>%
              select(unique_id, event_number, gen_max),
            by = c("unique_id", "event_number")) %>%
  na.omit() %>%
  select(max, gen_max, lon, lat, unique_id) %>%
  ungroup()

rm(complete_prec_events_df)
rm(generated_prec_events_df)

gc()

foreach(i = 1:length(unique_id_chunks)) %dopar% {
  unique_id_chunk = unique_id_chunks[[i]]
  lapply(X = unique_id_chunk,
         FUN = function(cur_unique_id){
           gc()
           (combined_events_df %>%
               filter(unique_id == cur_unique_id) %>%
               as.data.frame() %>%
               dts_wrapper(col1_ind = 1, col2_ind = 2))[2] %>%
             as.data.frame() %>%
             mutate(unique_id = cur_unique_id)
         }) %>%
    bind_rows() %>%
    write_csv(file = "./data/dts_y_mt.csv", append = TRUE)
}

parallel::stopCluster(my.cluster)
