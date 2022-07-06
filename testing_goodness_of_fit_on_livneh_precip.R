#' ---
#' title: "GTETLG Goodness of Fit on Livneh (Unsplit) Precipitation"
#' output: github_document
#' ---
#'
#'
#' First, the setup (boring)

# Load packages -----------------------------------------------------------
library(tidyverse)
library(cramer)
library(Peacock.test)
library(fasano.franceschini.test)

# Load auxiliary functions ------------------------------------------------
source("./gtetlg_utils.R")


# Load files --------------------------------------------------------------
complete_prec_events_df <- read_csv("./data/livneh_unsplit/complete_prec_events.csv")
prec_event_parameters_df <- read_csv("./data/prec_event_parameters.csv") %>%
  rename(n = n_events,
         q = q_hat,
         p = p_hat,
         b = b_hat)


# Generate precip events from estimated parameters ------------------------
rgtetlg(prec_event_parameters_df[1,-(1:4)])

generated_prec_events_df <- apply(X = prec_event_parameters_df,
                                  FUN = function(row) {
                                    #print(row)
                                    #print(row[2] %>% as.numeric())
                                    do.call(Map,
                                            c(f = rgtetlg,
                                              row[-(1:4)] %>% as.numeric())
                                    )[[1]] %>%
                                      mutate(unique_id = row[1],
                                             lat = row[2] %>% as.numeric(),
                                             lon = row[3] %>% as.numeric())
                                    #print(row)
                                  },
                                  MARGIN = 1) %>% bind_rows()

# write_csv(x = generated_prec_events_df,
#           file = "./data/livneh_unsplit/generated_prec_events.csv")

generated_prec_events_df <- read_csv("./data/livneh_unsplit/generated_prec_events.csv")

#' **Now, we assess goodness-of-fit with a multivariate cramer test
unique_id_v <- complete_prec_events_df$unique_id %>% unique()

start_time <- Sys.time()
multivariate_fasano_xyn_df <- lapply(X = unique_id_v[1:3],
                                  FUN = function(id) {
                                    fasano.franceschini.test(S1 = complete_prec_events_df %>%
                                                               filter(unique_id == id) %>%
                                                               select(length, total, max_rate) %>%
                                                               as.matrix(),
                                                             S2 = generated_prec_events_df %>%
                                                               filter(unique_id == id) %>%
                                                               select(event_length, event_magnitude, event_max) %>%
                                                               as.matrix(),
                                                             #threads = 16,
                                                             threads = "auto",
                                                             method = "r")[c(1,2)] %>%
                                      as.data.frame() %>%
  mutate(unique_id = id,
         lat = (complete_prec_events_df %>% filter(unique_id == id))$lat[1],
         lon = (complete_prec_events_df %>% filter(unique_id == id))$lon[1])
}) %>% bind_rows()
Sys.time() - start_time

write_csv(x = multivariate_fasano_xyn_df,
          file = "./data/fasano_xyn.csv")


start_time <- Sys.time()
multivariate_fasano_n_df <- lapply(X = unique_id_v[1:3],
                                     FUN = function(id) {
                                       fasano.franceschini.test(S1 = complete_prec_events_df %>%
                                                                  filter(unique_id == id) %>%
                                                                  select(length) %>%
                                                                  as.matrix(),
                                                                S2 = generated_prec_events_df %>%
                                                                  filter(unique_id == id) %>%
                                                                  select(event_length) %>%
                                                                  as.matrix(),
                                                                #threads = 16,
                                                                threads = "auto",
                                                                method = "r")[c(1,2)] %>%
                                         as.data.frame() %>%
                                         mutate(unique_id = id,
                                                lat = (complete_prec_events_df %>% filter(unique_id == id))$lat[1],
                                                lon = (complete_prec_events_df %>% filter(unique_id == id))$lon[1])
                                     }) %>% bind_rows()
Sys.time() - start_time

# scratch work
id = unique_id_v[1]
cramer_object <- cramer.test(x = complete_prec_events_df %>%
                               filter(unique_id == id) %>%
                               select(length, total, max_rate) %>%
                               as.matrix(),
                             y = generated_prec_events_df %>%
                               filter(unique_id == id) %>%
                               select(event_length, event_magnitude, event_max) %>%
                               as.matrix(),
                             replicates = 200
)

cramer_df <- cramer_object[c(2,5,6,7,8,11)] %>%
  as.data.frame()

peacock_object <- peacock3(x = complete_prec_events_df %>%
                             filter(unique_id == id) %>%
                             select(length, total, max_rate) %>%
                             as.matrix(),
                           y = generated_prec_events_df %>%
                             filter(unique_id == id) %>%
                             select(event_length, event_magnitude, event_max) %>%
                             as.matrix())

fasano_object <- fasano.franceschini.test(S1 = complete_prec_events_df %>%
                                            filter(unique_id == id) %>%
                                            select(length, total, max_rate) %>%
                                            as.matrix(),
                                          S2 = generated_prec_events_df %>%
                                            filter(unique_id == id) %>%
                                            select(event_length, event_magnitude, event_max) %>%
                                            as.matrix(),
                                          threads = "auto",
                                          method = "r")

c(fasano_object[c(1,2)], 
  conf_int_low = fasano_object[[3]][1],
  conf_int_high = fasano_object[[3]][2])%>% as.data.frame()


