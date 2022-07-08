#' ---
#' title: "DTS Goodness of Fit Test for Event Total Distrubtion"
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

# Load files --------------------------------------------------------------
complete_prec_events_df <- read_csv("./data/livneh_unsplit/complete_prec_events.csv",
                                    lazy = FALSE) %>%
  rename(max = max_rate) %>%
  ungroup()
# prec_event_parameters_df <- read_csv("./data/prec_event_parameters.csv") %>%
#   rename(n = n_events,
#          q = q_hat,
#          p = p_hat,
#          b = b_hat)


# Generate precip events from estimated parameters ------------------------
# rgtetlg(prec_event_parameters_df[1,-(1:4)])
# 
# generated_prec_events_df <- apply(X = prec_event_parameters_df,
#                                   FUN = function(row) {
#                                     #print(row)
#                                     #print(row[2] %>% as.numeric())
#                                     do.call(Map,
#                                             c(f = rgtetlg,
#                                               row[-(1:4)] %>% as.numeric())
#                                     )[[1]] %>%
#                                       mutate(unique_id = row[1],
#                                              lat = row[2] %>% as.numeric(),
#                                              lon = row[3] %>% as.numeric())
#                                     #print(row)
#                                   },
#                                   MARGIN = 1) %>% bind_rows()

# write_csv(x = generated_prec_events_df,
#           file = "./data/livneh_unsplit/generated_prec_events.csv")

generated_prec_events_df <- read_csv("./data/livneh_unsplit/generated_prec_events.csv",
                                     lazy = FALSE) %>%
  rename(gen_length = event_length,
         gen_total = event_magnitude,
         gen_max = event_max) %>%
  group_by(unique_id) %>%
  mutate(event_number = row_number()) %>%
  ungroup()

# actual_and_gen_events_df <- read_csv("./data/livneh_unsplit/complete_prec_events.csv",
#                                      lazy = TRUE) %>%
#   rename(max = max_rate) %>%
#   select(unique_id, event_number, total, max, length) %>%
#   arrange(unique_id) %>%
#   cbind(read_csv("./data/livneh_unsplit/generated_prec_events.csv",
#                      lazy = TRUE) %>%
#               arrange(unique_id) %>%
#               rename(gen_length = event_length,
#                      gen_total = event_magnitude,
#                      gen_max = event_max) %>%
#               select(unique_id, gen_length,
#                      gen_total, gen_max))

# Event total ------------------------------------------------------------
gc()
unique_id_v <- complete_prec_events_df$unique_id %>% unique()
lon_v <- complete_prec_events_df$lat %>% unique()
lat_v <- complete_prec_events_df$lon %>% unique()

unique_id_chunks <- split(unique_id_v,
                          ceiling(seq_along(unique_id_v)/500))

start_time <- Sys.time()
i = 0
for(unique_id_chunk in unique_id_chunks) {
  lapply(X = unique_id_chunk,
         FUN = function(cur_unique_id){
           (
             complete_prec_events_df %>%
             filter(unique_id == cur_unique_id) %>%
             cbind(generated_prec_events_df %>%
                     filter(unique_id == cur_unique_id) %>%
                     select(gen_total)) %>%
             na.omit() %>%
             select(total, gen_total, lon, lat) %>%
             ungroup() %>%
             dts_wrapper(col1_ind = 1, col2_ind = 2,
                         nboots = 500))[2] %>%
              as.data.frame() %>%
              mutate(unique_id = cur_unique_id)
         }) %>%
    bind_rows() %>%
    #print()
    write_csv(file = "./data/dts_x.csv", append = TRUE)
  
  i = i + length(unique_id_chunk)
  
  if(as.numeric(difftime(Sys.time(), start_time), units = "secs") > (20 * 60)) {
    print(paste0(as.numeric(difftime(Sys.time(), start_time), units = "mins"), " minutes have passed."))
    start_time = Sys.time()
    print(paste0("Progress for the night: ", 
                 100 * signif(i/length(unique_id_v), 2),
                 "% or iteration ",i,"."))
    gc()
  }
  if(i == length(unique_id_v)){
    print(paste0(i, " iterations successfully computed. All done."))
  }
  i <- i + 1
}
