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

covars_to_plot <- covars_df %>% select(-c(iso3c:year)) %>% names()

for (c in covars_to_plot) {
  map_data_filtered <- map_data %>% filter(!is.na(.data[[c]]))

  p <- map_data_filtered %>%
    ggplot() +
    geom_sf(aes(fill = !!sym(c)), color = NA) +
    geom_sf(data = sf_adm0, color = "black", fill = NA) +
    scale_fill_viridis_c(option = "viridis", labels = scales::percent_format(accuracy = 1)) +
    coord_sf() +
    facet_wrap(~year) +
    theme_void(base_size = 14) +
    theme(plot.background = element_rect(fill = "white", color = "white"))

  filename <- str_glue("analysis/plots/map_{c}.png")
  ggsave(filename, p, width = 8, height = 6, dpi = 300)
}
