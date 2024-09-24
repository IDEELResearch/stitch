# Load pkgs ---------------------------------------------------------------

library(sf)
library(raster)
library(tidyverse)

# Read data ---------------------------------------------------------------

covars <- readRDS("analysis/data_raw/final_covariates.rds")
sf_adm0 <- readRDS("analysis/data_derived/sf_admin0_africa.rds") # adm0 map data


# Unlist and combine dfs in long format with map --------------------------

# Use the custom (but very specific) get_map_covar_data() fxn
map_data <- get_map_covar_data(covars)

# Plot all covariates over time facets ------------------------------------

covars_to_plot <- map_data %>% st_drop_geometry() %>% select(-c(iso3c:year)) %>% names()

plot_spatiotemporal_map(map_data, sf_adm0, covars_to_plot)
