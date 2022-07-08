#' ---
#' title: "Bebchmarking Various Goodness-of-Fit Tests"
#' output: github_document
#' ---
#'
#'
#' First, the setup (boring)

# Load packages -----------------------------------------------------------
library(tidyverse)
library(cramer)
#library(Peacock.test)
library(fasano.franceschini.test)
library(twosamples)

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


# Declare constants -------------------------------------------------------
test_lengths <- c(1,5,10)
unique_id_v <- complete_prec_events_df$unique_id %>% unique()

#' *Benchmarks*
# Trivariate --------------------------------------------------------------


#' *Fasano-Franceschni*
fasano_v <- numeric()
for(i in test_lengths) {
  start_time <- Sys.time()
  lapply(X = unique_id_v[1:i],
         FUN = function(id) {
           fasano.franceschini.test(S1 = complete_prec_events_df %>%
                                      filter(unique_id == id) %>%
                                      select(length, total, max_rate) %>%
                                      as.matrix(),
                                    S2 = generated_prec_events_df %>%
                                      filter(unique_id == id) %>%
                                      select(length, event_magnitude, event_max) %>%
                                      as.matrix(),
                                    threads = "auto",
                                    method = "r")[c(1,2)] %>%
             as.data.frame() %>%
             mutate(unique_id = id#,
                    #lat = (complete_prec_events_df %>% filter(unique_id == id))$lat[1],
                    #lon = (complete_prec_events_df %>% filter(unique_id == id))$lon[1]
                    )
         }) %>%
    bind_rows() %>%
    write_csv(file = "./data/fasano_xyn.csv")
  end_time <- Sys.time()
  fasano_v[i] <- as.numeric(difftime(end_time, start_time), units = "secs")
}
fasano_v <- fasano_v %>% na.omit()

benchmark_results_df <- data.frame(test_length = test_lengths, fasano_time = fasano_v)
write_csv(x = benchmark_results_df,
          file = "./data/goodness_of_fit_benchmark_results.csv")

#' *Cramer*
cramer_v <- numeric()
for(i in test_lengths) {
  start_time <- Sys.time()
  lapply(X = unique_id_v[1:i],
         FUN = function(id) {
           (cramer.test(x = complete_prec_events_df %>%
                         filter(unique_id == id) %>%
                         select(length, total, max_rate) %>%
                         as.matrix(),
                       y = generated_prec_events_df %>%
                         filter(unique_id == id) %>%
                         select(event_length, event_magnitude, event_max) %>%
                         as.matrix(),
                       replicates = 200))[c(2,5,6,7,8,11)] %>%
             as.data.frame() %>%
             mutate(unique_id = id,
                    lat = (complete_prec_events_df %>% filter(unique_id == id))$lat[1],
                    lon = (complete_prec_events_df %>% filter(unique_id == id))$lon[1])
         }) %>%
    bind_rows() %>%
    write_csv(file = "./data/cramer_xyn.csv")
  end_time <- Sys.time()
  cramer_v[i] <- as.numeric(difftime(end_time, start_time), units = "secs")
  print(paste0("Cramer test of size ", i, " completed..."))
}
cramer_v <- cramer_v %>% na.omit()

benchmark_results_df$cramer_time <- cramer_v
write_csv(x = benchmark_results_df,
          file = "./data/goodness_of_fit_benchmark_results.csv")

#' *Peacock from "Peakcock.test" package*
#' Forget about this... even testing a single point hung up the computer for many minutes


# Event Length ------------------------------------------------------------
chisq_v <- numeric()
for(i in test_lengths) {
  start_time <- Sys.time()
  lapply(X = unique_id_v[1:i],
         FUN = function(id) {
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
                    )
         }) %>%
    bind_rows() %>%
    write_csv(file = "./data/chisq_n.csv")
  end_time <- Sys.time()
  chisq_v[i] <- as.numeric(difftime(end_time, start_time), units = "secs")
  print(paste0("Chi squared test of size ", i, " completed..."))
}
chisq_v <- chisq_v %>% na.omit()
benchmark_results_df$chisq_time <- chisq_v
write_csv(x = benchmark_results_df,
          file = "./data/goodness_of_fit_benchmark_results.csv")




# Event Total -------------------------------------------------------------
#' *dts test*
dts_v <- numeric()
for(i in test_lengths) {
  start_time <- Sys.time()
  lapply(X = unique_id_v[1:i],
         FUN = function(id) {
           (dts_test(a = complete_prec_events_df %>%
                       filter(unique_id == id) %>%
                       select(total) %>%
                       as.matrix(),
                     b = generated_prec_events_df %>%
                       filter(unique_id == id) %>%
                       select(event_magnitude) %>%
                       as.matrix(),
                     nboots = 1000))[2] %>%
             as.data.frame() %>%
             mutate(unique_id = id#,
                    #lat = (complete_prec_events_df %>% filter(unique_id == id))$lat[1],
                    #lon = (complete_prec_events_df %>% filter(unique_id == id))$lon[1]
             )
         }) %>%
    bind_rows() %>%
    write_csv(file = "./data/dts_x.csv")
  end_time <- Sys.time()
  dts_v[i] <- as.numeric(difftime(end_time, start_time), units = "secs")
  print(paste0("DTS Test of size ", i, " completed..."))
}
dts_v <- dts_v %>% na.omit()
benchmark_results_df$dts_time <- dts_v
# write_csv(x = benchmark_results_df,
#           file = "./data/goodness_of_fit_benchmark_results.csv")

# Event Total -------------------------------------------------------------


# Scratch work ------------------------------------------------------------

id = unique_id_v[10000]
dts_object <- dts_test(a = complete_prec_events_df %>%
                         filter(unique_id == id) %>%
                         select(total) %>%
                         as.matrix(),
                       b = generated_prec_events_df %>%
                         filter(unique_id == id) %>%
                         select(event_magnitude) %>%
                         as.matrix(),
                       nboots = 1000)

plot(density(complete_prec_events_df %>%
               filter(unique_id == id) %>%
               select(total) %>%
               as.matrix()),
     col = "red")

lines(density(generated_prec_events_df %>%
               filter(unique_id == id) %>%
               select(event_magnitude) %>%
               as.matrix()),
      col = "blue")



ks_object <- ks_test(a = complete_prec_events_df %>%
                       filter(unique_id == id) %>%
                       select(total) %>%
                       as.matrix(),
                     b = generated_prec_events_df %>%
                       filter(unique_id == id) %>%
                       select(event_magnitude) %>%
                       as.matrix(),
                     nboots = 1000)


id = unique_id_v[1]
cramer_object <- cramer.test(x = complete_prec_events_df %>%
                               filter(unique_id == id) %>%
                               select(length, total, max_rate) %>%
                               as.matrix(),
                             y = generated_prec_events_df %>%
                               filter(unique_id == id) %>%
                               select(event_length, event_magnitude, event_max) %>%
                               as.matrix(),
                             replicates = 200)

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

chisq_object <-complete_prec_events_df %>%
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
  chisq.test(simulate.p.value=TRUE, B = 500)

chisq_object[[c(3)]]


c1 <- rgb(173,216,230,max = 255, alpha = 50, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 50, names = "lt.pink")
ax = 1:10
plot(hist((complete_prec_events_df %>%
             filter(unique_id == id))$length,
          breaks = ax),
     col = c1)
plot(hist((generated_prec_events_df %>%
             filter(unique_id == id))$length,
          breaks = ax),
     col = c2,
     add = TRUE
     )

chisq_object <- chisq.test(x = )


