# Load packages -----------------------------------------------------------
library(tidyverse)
library(parallel)

# Load auxiliary functions ------------------------------------------------
source("./gtetlg_utils.R")

# Load files --------------------------------------------------------------
prec_event_parameters_df <- read_csv("./data/prec_event_parameters.csv", lazy = TRUE) %>%
  rename(n = n_events,
         q = q_hat,
         p = p_hat,
         b = b_hat)

# Generate precip events from estimated parameters ------------------------
generated_prec_events_df <- apply(X = prec_event_parameters_df %>%
                                    na.omit(),
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

write_csv(x = generated_prec_events_df,
          file = "./data/livneh_unsplit/generated_prec_events.csv")


# Make sure actual and generated prec events are of the same numbe --------
generated_prec_events_df <- read_csv("./data/livneh_unsplit/generated_prec_events.csv",
                                     lazy = FALSE) %>%
  rename(gen_length = event_length,
         gen_total = event_magnitude,
         gen_max = event_max) %>%
  group_by(unique_id) %>%
  mutate(event_number = row_number()) %>%
  ungroup()

complete_prec_events_df <- read_csv("./data/livneh_unsplit/complete_prec_events.csv",
                                    lazy = FALSE) %>%
  rename(max = max_rate) %>%
  ungroup()

number_of_events_summary_df <- generated_prec_events_df %>%
  group_by(unique_id) %>%
  summarize(gen_n_events = max(event_number)) %>%
  ungroup() %>%
  na.omit() %>%
  left_join(complete_prec_events_df %>%
              group_by(unique_id) %>%
              summarize(act_n_events = max(event_number)) %>%
              ungroup() %>%
              na.omit(),
            by = "unique_id") %>%
  left_join(prec_event_parameters_df %>%
              select(unique_id, n) %>%
              rename(par_n_events = n),
            by = "unique_id") %>%
  mutate(diff_n_events = (gen_n_events == act_n_events & act_n_events == par_n_events))

which(number_of_events_summary_df$diff_n_events == FALSE)
number_of_events_summary_df$diff_n_events %>% max()

