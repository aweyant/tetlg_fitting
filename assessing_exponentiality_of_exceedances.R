
# Load packages -----------------------------------------------------------
library(tidyverse)
library(tidync)
library(lattice)


# Load goodness-of-fit test file ------------------------------------------
dts_exceedance_df <- read_csv("./data/dts_exceedance.csv", col_names = TRUE)
prec_event_parameters <- read_csv("./data/prec_event_parameters.csv", col_names = TRUE)


# Sample bad, average, and good performers --------------------------------
quantiles_of_interest <- dts_exceedance_df$p_val %>% quantile(c(0,0.58, 0.8,1))

dts_exceedance_subset_df <- dts_exceedance_df %>%
  filter(near(p_val, quantiles_of_interest[1], tol = 10^(-3))) %>%
  slice_sample(n = 10) %>%
  mutate(class = "bad") %>%
  bind_rows(dts_exceedance_df %>%
              filter(near(p_val, quantiles_of_interest[2], tol = 1/2 * 10^(-1)),
                     p_val > 0.05) %>%
              slice_sample(n = 10) %>%
              mutate(class = "just_passing")) %>%
  bind_rows(dts_exceedance_df %>%
              filter(near(p_val, quantiles_of_interest[3], tol = 1/2 * 10^(-1))) %>%
              slice_sample(n = 10) %>%
              mutate(class = "midling")) %>%
  bind_rows(dts_exceedance_df %>%
              filter(near(p_val, quantiles_of_interest[4], tol = 1/2 * 10^(-1))) %>%
              slice_sample(n = 10) %>%
              mutate(class = "nearly_perfect"))


# Save qq plots for each sampled point ------------------------------------
lats_of_int <- dts_exceedance_subset_df$lat
lons_of_int <- dts_exceedance_subset_df$lon

i = 35

prec_point_df <- tidync(x = "./data/livneh_unsplit/complete.prec.nc") %>%
  hyper_filter(lon = (lon %in% lons_of_int[i])) %>%
  hyper_filter(lat = (lat %in% lats_of_int[i])) %>%
  hyper_tibble() %>%
  group_by(lat,lon) %>%
  mutate(threshold = quantile(prec, 0.95)) %>%
  filter(prec > threshold) %>%
  mutate(exceedance = prec-threshold)

qqmath(~exceedance,
       data = prec_point_df,
       distribution = function(p) {qexp(p, rate = (prec_event_parameters %>%
                                                     filter(lat == lats_of_int[i],
                                                            lon == lons_of_int[i]))$b_hat)},
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       },
       aspect = "xy",
       xlab = "Theoretical",
       ylab = "Observed",
       sub = paste0("p = ",
                    round((dts_exceedance_df %>%
                             filter(lat == lats_of_int[i],
                                    lon == lons_of_int[i]))$p_val, 3))
       )




  


