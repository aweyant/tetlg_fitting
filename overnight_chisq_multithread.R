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
  rename(length = gen_length)

#' *Chi Square Test*
unique_id_v <- complete_prec_events_df$unique_id %>% unique()

# Event Length ------------------------------------------------------------

#number_of_iterations = length(unique_id_v)
#operation_duration = 2 # [seconds] - estimated from benchmark
#estimated_hours = (number_of_iterations * operation_duration)/60/60
#hours_in_night = 15
#iterations_per_night = round(hours_in_night/estimated_hours * number_of_iterations)

#unique_id_chunks <- split(unique_id_v,
#                          ceiling(seq_along(unique_id_v)/iterations_per_night))

#unique_id_v <- unique_id_chunks[[1]]
#unique_id_v <- c(unique_id_chunks[[1]],
#                 unique_id_chunks[[2]],
#                 unique_id_chunks[[3]])

combined_events_df <- complete_prec_events_df %>%
	group_by(length, unique_id) %>%
	summarize(count = n()) %>%
	ungroup() %>%
	left_join(generated_prec_events_df %>%
		group_by(length, unique_id) %>%
		summarize(count = n()) %>%
		ungroup(),
		by = c("unique_id", "length")) %>%
		replace(is.na(.), 0)

rm(complete_prec_events_df)
rm(generated_prec_events_df)
gc()

source("./create_parallel_backend_12.R")

foreach(i = 1:length(unique_id_v)) %dopar% {
  id = unique_id_v[i]
  (combined_events_df %>%
     filter(unique_id == id) %>%
     select(count.x, count.y) %>%
     chisq.test(simulate.p.value=TRUE,
                B = 500
     ))[3] %>%
    as.data.frame() %>%
    mutate(unique_id = id) %>%
    bind_rows() %>%
    write_csv(file = "./data/chisq_n_mt.csv",
              append = TRUE)
   gc()
}


#foreach(i = 1:length(unique_id_v)) %dopar% {
#  id = unique_id_v[i]
#  (complete_prec_events_df %>%
#     filter(unique_id == id) %>%
#     group_by(length) %>%
#     summarize(count = n()) %>%
#     left_join(generated_prec_events_df %>%
#                 filter(unique_id == id) %>%
#                 group_by(length) %>%
#                 summarize(count = n()),
#               by = "length") %>%
#     replace(is.na(.), 0) %>%
#     select(count.x, count.y) %>%
#     chisq.test(simulate.p.value=TRUE,
#                B = 500
#     ))[3] %>%
#    as.data.frame() %>%
#    mutate(unique_id = id#,
#           #lat = (complete_prec_events_df %>% filter(unique_id == id))$lat[1],
#           #lon = (complete_prec_events_df %>% filter(unique_id == id))$lon[1]
#    ) %>%
#    bind_rows() %>%
#    write_csv(file = "./data/chisq_n_mt.csv",
#              append = TRUE)
#   gc()
#}

parallel::stopCluster(my.cluster)
