#' ---
#' title: "Chisquare Goodness of Fit Test for Event Length Distribution"
#' output: github_document
#' ---
#'
#'
#' First, the setup (boring)

# Load packages -----------------------------------------------------------
library(tidyverse)

# Load auxiliary functions ------------------------------------------------
source("./gtetlg_utils.R")

# Load files --------------------------------------------------------------
complete_prec_events_df <- read_csv("./data/livneh_unsplit/complete_prec_events.csv")
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

generated_prec_events_df <- read_csv("./data/livneh_unsplit/generated_prec_events.csv") %>%
  rename(length = event_length)

#' *Chi Square Test*
test_lengths <- c(1,5,10)
unique_id_v <- complete_prec_events_df$unique_id %>% unique()

# Event Length ------------------------------------------------------------

number_of_iterations = length(unique_id_v)
operation_duration = 2 # [seconds] - estimated from benchmark
estimated_hours = (number_of_iterations * operation_duration)/60/60
hours_in_night = 15
iterations_per_night = round(hours_in_night/estimated_hours * number_of_iterations)

unique_id_chunks <- split(unique_id_v,
                          ceiling(seq_along(unique_id_v)/iterations_per_night))

#unique_id_v <- unique_id_chunks[[1]]
unique_id_v <- c(unique_id_chunks[[2]], unique_id_chunks[[3]])

start_time <- Sys.time()
i = 0
for(id in unique_id_v) {
  (complete_prec_events_df %>%
     filter(unique_id == id) %>%
     group_by(length) %>%
     summarize(count = n()) %>%
     left_join(generated_prec_events_df %>%
                 filter(unique_id == id) %>%
                 group_by(length) %>%
                 summarize(count = n()),
               by = "length") %>%
     replace(is.na(.), 0) %>%
     select(count.x, count.y) %>%
     chisq.test(simulate.p.value=TRUE,
                B = 500
     ))[3] %>%
    as.data.frame() %>%
    mutate(unique_id = id#,
           #lat = (complete_prec_events_df %>% filter(unique_id == id))$lat[1],
           #lon = (complete_prec_events_df %>% filter(unique_id == id))$lon[1]
    ) %>%
  bind_rows() %>%
  write_csv(file = "./data/chisq_n.csv",
            append = TRUE)
  
  if(as.numeric(difftime(Sys.time(), start_time), units = "secs") > (20 * 60)) {
    start_time = Sys.time()
    print("20 minutes have passed.")
    print(paste0("Progress for the night: ", 
                 100 * signif(i/length(unique_id_v), 2),
                 "% or iteration ",i,"."))
  }
  if(i == length(unique_id_v)){
    print(paste0(i, " iterations successfully computed. All done."))
  }
  i <- i + 1
}