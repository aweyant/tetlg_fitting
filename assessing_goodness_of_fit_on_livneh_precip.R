#' ---
#' title: "GTETLG Goodness of Fit on Livneh (Unsplit) Precipitation"
#' output: github_document
#' ---
#'
#'
#' First, the setup (boring)

# Load packages -----------------------------------------------------------
library(tidyverse)
library(lattice)
library(rnaturalearth)
library(rnaturalearthdata)
library(metR)
library(sf)
#library(fasano.franceschini.test)

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

generated_prec_events_df <- read_csv(file = "./data/livneh_unsplit/generated_prec_events.csv")

# Declare constants -------------------------------------------------------
world <- ne_countries(returnclass = "sf")
#lon_min <- 246
lon_min <- complete_prec_events_df$lon %>% min()
#lon_max<- 247
lon_max <- complete_prec_events_df$lon %>% max()
#lat_min <- 32
lat_min <- complete_prec_events_df$lat %>% min()
#lat_max <- 33
lat_max <- complete_prec_events_df$lat %>% max()


# Event length ------------------------------------------------------------
#' **Event Length Hurdle Geometric Goodness-of-Fit**
chisq_n_df <- read_csv("./data/chisq_n_mt.csv", col_names = FALSE) %>%
  rename('p_val' = 'X1', 'unique_id' = 'X2') %>%
  separate(col = unique_id,
           into = c("lon", "lat"),
           sep = "_",
           convert= TRUE)

#+ goodness_of_fit_chisq_event_length_map
# ggplot(data = world) +
#   geom_contour(data = chisq_n_df %>%
#                  na.omit(),
#                aes(x = lon,
#                    y = lat,
#                    z = p_val)) +
#   geom_contour_fill(data = chisq_n_df %>%
#                       na.omit(),
#                     aes(x = lon,
#                         y = lat,
#                         z = p_val)) +
#   # geom_tile(data = chisq_n_df %>%
#   #             na.omit(),
#   #           aes(x = lon,
#   #               y = lat,
#   #               colour = p_val)) +
#   scale_fill_viridis_c(option = "cividis") +
#   #scale_color_viridis_c(option = "cividis") +
#   geom_sf(fill = NA) +
#   coord_sf(xlim = c(lon_min, lon_max),
#            ylim = c(lat_min, lat_max),
#            #ylim = c(26, lat_max),
#            expand = FALSE) +
#   guides(fill = guide_colorsteps(title = paste0("p"))) +
#   labs(title = "Chisq p-vals for\n Fitted Event Length Dist.") +
#   theme_bw()

#+ goodness_of_fit_chisq_event_length_tile_map
ggplot(data = world) +
  # geom_contour(data = chisq_n_df %>%
  #                na.omit(),
  #              aes(x = lon,
  #                  y = lat,
  #                  z = p_val)) +
  # geom_contour_fill(data = chisq_n_df %>%
  #                     na.omit(),
  #                   aes(x = lon,
  #                       y = lat,
  #                       z = p_val)) +
  # geom_tile(data = chisq_n_df %>%
  #             na.omit(),
  #           aes(x = lon,
  #               y = lat,
  #               colour = p_val)) +
  geom_point(data = chisq_n_df %>%
              na.omit(),
            aes(x = lon,
                y = lat,
                colour = p_val)) +
  
  #scale_fill_viridis_c(option = "cividis") +
  scale_color_viridis_c(option = "cividis") +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(lon_min, lon_max),
           ylim = c(lat_min, lat_max),
           #ylim = c(26, lat_max),
           expand = FALSE) +
  guides(fill = guide_colorsteps(title = paste0("p"))) +
  labs(title = "Chisq p-Values for\n Fitted Hurdle Geometric") +
  theme_bw()


# Event total -------------------------------------------------------------
#' **Event Total Exponential Goodness-of-fit**
dts_x_df <- read_csv("./data/dts_x_mt.csv", col_names = c("p_val", "lon", "lat"))

#+ goodness_of_fit_dts_event_total_map
ggplot(data = world) +
  geom_contour(data = dts_x_df %>%
                 na.omit(),
               aes(x = lon,
                   y = lat,
                   z = p_val)) +
  geom_contour_fill(data = dts_x_df %>%
                      na.omit(),
                    aes(x = lon,
                        y = lat,
                        z = p_val)) +
  # geom_tile(data = chisq_n_df %>%
  #             na.omit(),
  #           aes(x = lon,
  #               y = lat,
  #               colour = p_val)) +
  scale_fill_viridis_c(option = "cividis") +
  #scale_color_viridis_c(option = "cividis") +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(lon_min, lon_max),
           ylim = c(lat_min, lat_max),
           #ylim = c(26, lat_max),
           expand = FALSE) +
  guides(fill = guide_colorsteps(title = paste0("p"))) +
  labs(title = "DTS p-vals for\n Fitted Total Dist.") +
  theme_bw()


# Event Max ---------------------------------------------------------------
#' **Event Max Exponential Goodness-of-fit**
dts_y_df <- read_csv("./data/dts_y_mt.csv", col_names = c("p_val", "unique_id")) %>%
  separate(col = unique_id,
           into = c("lon", "lat"),
           sep = "_",
           convert= TRUE) %>%
  na.omit()

#+ goodness_of_fit_dts_event_max_map
ggplot(data = world) +
  geom_contour(data = dts_y_df %>%
                 na.omit(),
               aes(x = lon,
                   y = lat,
                   z = p_val)) +
  geom_contour_fill(data = dts_y_df %>%
                      na.omit(),
                    aes(x = lon,
                        y = lat,
                        z = p_val)) +
  # geom_tile(data = chisq_n_df %>%
  #             na.omit(),
  #           aes(x = lon,
  #               y = lat,
  #               colour = p_val)) +
  scale_fill_viridis_c(option = "cividis") +
  #scale_color_viridis_c(option = "cividis") +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(lon_min, lon_max),
           ylim = c(lat_min, lat_max),
           #ylim = c(26, lat_max),
           expand = FALSE) +
  guides(fill = guide_colorsteps(title = paste0("p"))) +
  labs(title = "DTS p-vals for\n Fitted Max. Dist.") +
  theme_bw()


# Daily Exceedance --------------------------------------------------------
#' **Daily Exceedance Goodness-of-fit**
dts_exceedance_df <- read_csv("./data/dts_exceedance.csv", col_names = TRUE)

#+ goodness_of_fit_exponential_exceedance_plot
ggplot(data = world) +
  geom_contour(data = dts_exceedance_df %>%
                 filter(p_val > 0.15) %>%
                 na.omit(),
               aes(x = lon,
                   y = lat,
                   z = p_val)) +
  geom_contour_fill(data = dts_exceedance_df %>%
                      filter(p_val > 0.15) %>%
                      na.omit(),
                    aes(x = lon,
                        y = lat,
                        z = p_val)) +
  # geom_tile(data = chisq_n_df %>%
  #             na.omit(),
  #           aes(x = lon,
  #               y = lat,
  #               colour = p_val)) +
  scale_fill_viridis_c(option = "cividis") +
  #scale_color_viridis_c(option = "cividis") +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(lon_min, lon_max),
           ylim = c(lat_min, lat_max),
           #ylim = c(26, lat_max),
           expand = FALSE) +
  guides(fill = guide_colorsteps(title = paste0("p"))) +
  labs(title = "DTS p-Values for\n Fitted Exceedance Dist.") +
  theme_bw()

# To be placed in overnight scripts ---------------------------------------
# generated_prec_events_df <- read_csv("./data/livneh_unsplit/generated_prec_events.csv") %>%
#   rename(length = event_length)
# 
# unique_id_v <- complete_prec_events_df$unique_id %>% unique()
# 
# start_time <- Sys.time()
# multivariate_fasano_xyn_df <- lapply(X = unique_id_v[1:3],
#                                   FUN = function(id) {
#                                     fasano.franceschini.test(S1 = complete_prec_events_df %>%
#                                                                filter(unique_id == id) %>%
#                                                                select(length, total, max_rate) %>%
#                                                                as.matrix(),
#                                                              S2 = generated_prec_events_df %>%
#                                                                filter(unique_id == id) %>%
#                                                                select(event_length, event_magnitude, event_max) %>%
#                                                                as.matrix(),
#                                                              #threads = 16,
#                                                              threads = "auto",
#                                                              method = "r")[c(1,2)] %>%
#                                       as.data.frame() %>%
#   mutate(unique_id = id,
#          lat = (complete_prec_events_df %>% filter(unique_id == id))$lat[1],
#          lon = (complete_prec_events_df %>% filter(unique_id == id))$lon[1])
# }) %>% bind_rows()
# Sys.time() - start_time
# 
# write_csv(x = multivariate_fasano_xyn_df,
#           file = "./data/fasano_xyn.csv")
# 
# 
# start_time <- Sys.time()
# multivariate_fasano_n_df <- lapply(X = unique_id_v[1:3],
#                                      FUN = function(id) {
#                                        fasano.franceschini.test(S1 = complete_prec_events_df %>%
#                                                                   filter(unique_id == id) %>%
#                                                                   select(length) %>%
#                                                                   as.matrix(),
#                                                                 S2 = generated_prec_events_df %>%
#                                                                   filter(unique_id == id) %>%
#                                                                   select(event_length) %>%
#                                                                   as.matrix(),
#                                                                 #threads = 16,
#                                                                 threads = "auto",
#                                                                 method = "r")[c(1,2)] %>%
#                                          as.data.frame() %>%
#                                          mutate(unique_id = id,
#                                                 lat = (complete_prec_events_df %>% filter(unique_id == id))$lat[1],
#                                                 lon = (complete_prec_events_df %>% filter(unique_id == id))$lon[1])
#                                      }) %>% bind_rows()
# Sys.time() - start_time



# Scratch plot - density plot of x at chosen points -----------------------
# subset_of_points <- data.frame(unique_id = complete_prec_events_df$unique_id %>%
#                                  unique %>%
#                                  sample(size = 5))
# 
# combined_prec_events_df <- complete_prec_events_df %>%
#   filter(unique_id %in% subset_of_points$unique_id) %>%
#   select(unique_id, event_number, total) %>%
#   left_join(generated_prec_events_df %>%
#               filter(unique_id %in% subset_of_points$unique_id) %>%
#               select(unique_id, event_number, gen_total),
#             by = c("unique_id", "event_number")) %>%
#   select(-event_number) %>%
#   pivot_longer(cols = c("total", "gen_total"),
#                values_to = "total",
#                names_to = "source")
# 
# 
# qq(source ~ total,
#    data = combined_prec_events_df %>%
#      filter(unique_id == subset_of_points$unique_id[1]),
#    aspect = 1,
#    f.value = seq(0,1, by = 0.01))

# qqmath( ~ total,
#        data = combined_prec_events_df %>%
#          filter(unique_id == subset_of_points$unique_id[1]),
#        distribution = function(probability) {
#          qgenexp(p = probability,
#            p_geom = (prec_event_parameters_df %>% 
#                        filter(unique_id == subset_of_points$unique_id[1]))$p,
#            q = (prec_event_parameters_df %>%
#                   filter(unique_id == subset_of_points$unique_id[1]))$q,
#            b = (prec_event_parameters_df %>%
#                   filter(unique_id == subset_of_points$unique_id[1]))$b)
#        },
#        aspect = 1,
#        f.value = seq(0, 1, by = 0.01)
#        )

# p = seq(0, 1, by = 0.01)
# tot <- (combined_prec_events_df %>%
#   filter(unique_id == subset_of_points$unique_id[2]))$total
# gen_tot <-(combined_prec_events_df %>%
#   filter(unique_id == subset_of_points$unique_id[2]))$gen_total
# 
# 
# ggplot() +
#   geom_point(aes(x = quantile(tot,p),
#                  y = quantile(gen_tot,p))) +
#   scale_x_log10() +
#   scale_y_log10() +
#   geom_abline(intercept = 0, slope = 1) +
#   coord_fixed()


# Scratch work ------------------------------------------------------------
# id = unique_id_v[1]
# cramer_object <- cramer.test(x = complete_prec_events_df %>%
#                                filter(unique_id == id) %>%
#                                select(length, total, max_rate) %>%
#                                as.matrix(),
#                              y = generated_prec_events_df %>%
#                                filter(unique_id == id) %>%
#                                select(event_length, event_magnitude, event_max) %>%
#                                as.matrix(),
#                              replicates = 200
# )
# 
# cramer_df <- cramer_object[c(2,5,6,7,8,11)] %>%
#   as.data.frame()
# 
# peacock_object <- peacock3(x = complete_prec_events_df %>%
#                              filter(unique_id == id) %>%
#                              select(length, total, max_rate) %>%
#                              as.matrix(),
#                            y = generated_prec_events_df %>%
#                              filter(unique_id == id) %>%
#                              select(event_length, event_magnitude, event_max) %>%
#                              as.matrix())
# 
# fasano_object <- fasano.franceschini.test(S1 = complete_prec_events_df %>%
#                                             filter(unique_id == id) %>%
#                                             select(length, total, max_rate) %>%
#                                             as.matrix(),
#                                           S2 = generated_prec_events_df %>%
#                                             filter(unique_id == id) %>%
#                                             select(event_length, event_magnitude, event_max) %>%
#                                             as.matrix(),
#                                           threads = "auto",
#                                           method = "r")
# 
# c(fasano_object[c(1,2)], 
#   conf_int_low = fasano_object[[3]][1],
#   conf_int_high = fasano_object[[3]][2])%>% as.data.frame()


