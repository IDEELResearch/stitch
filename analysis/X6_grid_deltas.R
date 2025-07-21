# author: CMS and NWY
# description: Grid over Africa and calculate prevalence change

library(sf)       # for shapefile handling
library(ggplot2)  # for plotting
library(dplyr)    # for data manipulation
library(tidyr)    # for mild clean of stave data
library(usethis)  # req'd for devtools
library(here)     # req'd for devtools
library(devtools) # for github code check
library(countrycode) # for coordinating STAVE country with iso3c code
library(scales)
library(terra)

# Read in prevalence data
prev_data <- read.csv("analysis/data_derived/all_who_get_prevalence.csv")
# I can only do this across all validated mutations i think?
validated_mutations <- c("k13:446:I", "k13:458:Y", "k13:469:Y", "k13:476:I",   "k13:493:H",   "k13:539:T",
                         "k13:543:T",  "k13:553:L",   "k13:561:H",   "k13:574:L",  "k13:580:Y",  "k13:622:I","k13:675:V")

prev_data <- filter(prev_data, mutation %in% validated_mutations)
prev_data <- prev_data %>%
  group_by(latitude, longitude, study_name, country_name, site_name, collection_day, year, denominator) %>%
  summarize(prevalence = sum(prevalence, na.rm = TRUE)) %>%
  ungroup()
prev_data <- st_as_sf(prev_data, coords = c("longitude", "latitude"), crs = 4326)

# Read in Africa shape file generated in 01_generative_base_maps.R
rds_file_admin0 = "analysis/data_derived/sf_admin0_africa.rds"
rds_file_admin1 = "analysis/data_derived/sf_admin1_africa.rds"
africa_shp_admin0 <- readRDS(file = rds_file_admin0)
africa_shp_admin1 <- readRDS(file = rds_file_admin1)
africa_bbox <- st_bbox(africa_shp_admin0)
africa_ext <- ext(africa_bbox["xmin"], africa_bbox["xmax"],
                  africa_bbox["ymin"], africa_bbox["ymax"])

#align CRS
target_crs <- 4326
rds_file_admin0 <-st_transform(africa_shp_admin0, crs = target_crs)

# Make grid for Africa
r <- rast(ext=africa_ext, resolution=100000, crs=st_crs(africa_shp_admin0)$wkt)

grid_polygons <- as.polygons(r)
grid_sf <- st_as_sf(grid_polygons)
grid_sf$grid_id <- 1:nrow(grid_sf)

# Intersect with Africa to clip
grid <- st_intersection(grid_sf, africa_shp_admin0)

grid_deg <- st_make_grid(
  africa_shp_admin0,
  cellsize = c(0.9, 0.9),  # lon x lat (approx 100km x 100km)
  square = TRUE
)
grid <- st_sf(grid_id = 1:length(grid_deg), geometry = grid_deg)
#grid <- st_intersection(grid, africa_shp_admin0)

grid_centroids <- st_centroid(grid)
grid_centroids <- grid_centroids %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  )

#Assign points to grid
prev_binned <- st_join(prev_data, grid, join = st_within)

#Aggregate prevalence per grid cell
grid_prev_summary <- prev_binned %>%
  st_drop_geometry() %>%
  group_by(grid_id, year) %>%
  summarise(
    mean_prev = mean(prevalence, na.rm = TRUE),
    n_samples = n(),
    .groups = "drop"
  )

#plot grid for sanity bc it was not working
ggplot() +
  geom_sf(data = africa_shp_admin0, fill = "gray95", color = "gray80") +
  geom_sf(data = grid, fill = NA, color = "red", size = 0.2) +
  theme_minimal() +
  labs(title = "100x100 km Approximate Grid in EPSG:4326")


#filtering to 10 years bc my brain can't handle the 1990s 0s too confusing
latest_date <- max(prev_binned$year, na.rm = TRUE)
cutoff_date <- latest_date - 10

prev_binned <- prev_binned %>%
  filter(year >= cutoff_date)

#setting 2-year bins?
prev_binned <- prev_binned %>%
  mutate(year_bin = cut(year,
                        breaks = seq(cutoff_date, latest_date + 1, by = 2),
                        include.lowest = TRUE, right = FALSE))
grid_prev_summary <- prev_binned %>%
  st_drop_geometry() %>%
  group_by(grid_id, year_bin) %>%
  summarise(mean_prev = mean(prevalence, na.rm = TRUE),
            n_samples = n(), .groups = 'drop')

#calculate deltas
prev_wide <- grid_prev_summary %>%
  pivot_wider(names_from = year_bin, values_from = mean_prev)

# Calculate delta between earliest and latest time bins
# Replace with correct bin names if known
time_bins <- sort(colnames(prev_wide)[-c(1,2,14)])  # get bin columns
prev_wide$delta_prev <- prev_wide[[time_bins[length(time_bins)]]] - prev_wide[[time_bins[1]]]

grid_with_delta <- grid %>%
  left_join(prev_wide[, c("grid_id", "delta_prev")], by = "grid_id")


delta_summary <- grid_prev_summary %>%
  arrange(grid_id, year) %>%
  group_by(grid_id) %>%
  summarise(
    first_year = year[which(!is.na(mean_prev))[1]],
    last_year  = year[rev(which(!is.na(mean_prev)))[1]],
    first_prev = mean_prev[which(!is.na(mean_prev))[1]],
    last_prev  = mean_prev[rev(which(!is.na(mean_prev)))[1]],
    delta_prev = last_prev - first_prev,
    n_years = last_year - first_year + 1,
    .groups = "drop"
  )
delta_centroids <- grid_centroids %>%
  left_join(delta_summary, by = "grid_id")

ggplot() +
  geom_sf(data = grid_with_delta, aes(fill = delta_prev), color = NA) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       name = "Δ Prevalence") +
  theme_minimal() +
  labs(title = "Change in Prevalence (Last 10 Years)",
       subtitle = "100km x 100km grid across Africa")



ggplot() +
  geom_sf(data = africa_shp_admin0, color = "gray70", alpha=0.5) +
  geom_point(data = delta_centroids %>% filter(n_years > 1),
             aes(x = lon, y = lat, color = delta_prev),
             size = 2) +
  scale_color_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0,
    name = "Δ Prevalence"
  ) +
  coord_sf(crs = target_crs) +
  theme_minimal() +
  labs(title = "Change in Malaria Prevalence (from year of first measure to year of last measure)",
       subtitle = "Each point is the centroid of a 100x100km grid cell")


ggplot() +
  geom_sf(data = africa_shp_admin0, color = "gray70", alpha=0.5) +
  geom_point(data = delta_centroids %>% filter(n_years > 1) %>% mutate(bal_prev = delta_prev/n_years),
             aes(x = lon, y = lat, color = bal_prev),
             size = 2) +
  scale_color_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0,
    name = "Δ Prevalence per year"
  ) +
  coord_sf(crs = target_crs) +
  theme_minimal() +
  labs(title = "Change in Malaria Prevalence (from year of first measure to year of last measure)",
       subtitle = "Each point is the centroid of a 100x100km grid cell")
