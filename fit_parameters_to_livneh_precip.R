#' ---
#' title: "A First Look of GTETLG Applied to Daily Precipitation!"
#' output: github_document
#' ---
#'
#'

#' First, some setup code
# Load packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Run auxiliary scripts ---------------------------------------------------
source("./gtetlg_utils.R")

# Declare constants -------------------------------------------------------
world <- ne_countries(returnclass = "sf")

# Load datasets -----------------------------------------------------------
complete_prec_events_df <- read.csv("~/Documents/research-projects/tetlg_fitting/data/livneh_unsplit/complete_prec_events.csv")


# Temporarily use a subset of this data -----------------------------------
subset_of_unique_ids <- complete_prec_events_df$unique_id %>%
  unique() %>%
  sample(size = 20)

complete_prec_events_df <- complete_prec_events_df %>%
  filter(unique_id %in% subset_of_unique_ids)

#' Now, we will fit the GTETLG parameters on each gridcell
prec_event_parameters <- complete_prec_events_df %>%
  group_by(unique_id) %>%
  summarize(lat = min(lat),
            lon = min(lon),
            event_var_threshold = min(event_var_threshold),
            n_events = max(event_number),
            q_hat = q_est(length),
            p_hat = p_est(length),
            b_hat = b_est(length, total))

#' Maps of parameter estimates

#+ q_hat_california_map
# qhat_vs_position <- ggplot(data = world) +
#   geom_sf(fill = NA) +
#   coord_sf(xlim = c(-122.55,-115.10), ylim = c(33.65,41.99)) +
#   geom_point(data = prec_event_parameters,
#              aes(y = lat,
#                  x = lon,
#                  color = q_hat)) +
#   theme_bw()

ggplot(data = prec_event_parameters %>%
         filter(lat == 40.46875),
       aes(x = lon,
           y = q_hat)) +
  geom_path()

#+ p_hat_california_map
# phat_vs_position <- ggplot(data = world) +
#   geom_sf(fill = NA) +
#   coord_sf(xlim = c(-122.55,-115.10), ylim = c(33.65,41.99)) +
#   geom_point(data = california_precip_dist_summary,
#              aes(y = latitude,
#                  x = longitude,
#                  color = p_hat)) +
#   theme_bw()

ggplot(data = prec_event_parameters %>%
         filter(lat == 40.46875),
       aes(x = lon,
           y = p_hat)) +
  geom_path()

#+ b_hat_california_map
# bhat_vs_position <- ggplot(data = world) +
#   geom_sf(fill = NA) +
#   coord_sf(xlim = c(-122.55,-115.10), ylim = c(33.65,41.99)) +
#   geom_point(data = california_precip_dist_summary,
#              aes(y = latitude,
#                  x = longitude,
#                  color = b_hat)) +
#   theme_bw()

ggplot(data = prec_event_parameters %>%
         filter(lat == 40.46875),
       aes(x = lon,
           y = b_hat)) +
  geom_path()

#+ dotchart
dotchart(VADeaths, main = "Death Rates in Virginia - 1940")