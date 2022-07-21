
# Load packages -----------------------------------------------------------
library(tidyverse)
library(foreach)

# Load files --------------------------------------------------------------
complete_prec_events_df <- read_csv("./data/livneh_unsplit/complete_prec_events.csv")

generated_prec_events_df <- read_csv(file = "./data/livneh_unsplit/generated_prec_events.csv")

prec_event_parameters_df <- read_csv("./data/prec_event_parameters.csv") %>%
  rename(n = n_events,
         q = q_hat,
         p = p_hat,
         b = b_hat)

chisq_n_df <- read_csv("./data/chisq_n_mt.csv", col_names = c("p_val", "unique_id")) %>%
  separate(col = unique_id,
           into = c("lon", "lat"),
           sep = "_",
           convert = TRUE,
           remove = FALSE)

gc()

# Cookie-cutter histogram function ----------------------------------------
source("./fun_overlapping_histograms.R")


# Create "representative" subset ------------------------------------------
quantiles_of_interest <- chisq_n_df$p_val %>% quantile(c(0,0.032, 0.15,1))

chisq_n_subset_df <- chisq_n_df %>%
  filter(near(p_val, quantiles_of_interest[1], tol = 10^(-3))) %>%
  slice_sample(n = 10) %>%
  mutate(class = "bad") %>%
  bind_rows(chisq_n_df %>%
              filter(near(p_val, quantiles_of_interest[2], tol = 1/2 * 10^(-1)),
                     p_val > 0.05) %>%
              slice_sample(n = 10) %>%
              mutate(class = "just_passing")) %>%
  bind_rows(chisq_n_df %>%
              filter(near(p_val, quantiles_of_interest[3], tol = 1/2 * 10^(-1))) %>%
              slice_sample(n = 10) %>%
              mutate(class = "midling")) %>%
  bind_rows(chisq_n_df %>%
              filter(near(p_val, quantiles_of_interest[4], tol = 1/2 * 10^(-1))) %>%
              slice_sample(n = 10) %>%
              mutate(class = "nearly_perfect"))
              
print(chisq_n_subset_df %>% head())


# Test function -----------------------------------------------------------
#source("./create_parallel_backend_08.R")
for(i in seq_along(chisq_n_subset_df$unique_id)){
  id = chisq_n_subset_df$unique_id[i]
  png(filename = paste0("./images/event_length_histograms/",
                        "p=",
                        round((chisq_n_subset_df %>%
                                 filter(unique_id == id))$p_val, digits = 3),
                        "_",
                        id,
                        "_",
                        "histogram.png"),
      height = 10,
      width = 10,
      unit = "cm",
      res = 200)
  plot_double_hist(A = (complete_prec_events_df %>%
                          filter(unique_id == id))$length,
                   B = (generated_prec_events_df %>%
                          filter(unique_id == id))$gen_length,
                   title = paste0("Observed and Generated\n Event Length Counts\n", id),
                   subtitle = paste0("p = ",
                                     round((chisq_n_subset_df %>%
                                              filter(unique_id == id))$p_val, digits = 3))
  )
  dev.off()
  gc()
}

#parallel::stopCluster(my.cluster)






