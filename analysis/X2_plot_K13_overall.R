# author: CMS and NWY
# description: Plot K13 prevalence data across Africa and East Africa

library(sf)       # for shapefile handling
library(ggplot2)  # for plotting
library(dplyr)    # for data manipulation
library(tidyr)    # for mild clean of stave data
library(usethis)  # req'd for devtools
library(here)     # req'd for devtools
library(devtools) # for github code check
library(countrycode) # for coordinating STAVE country with iso3c code
library(scales)

# Read in prevalence data
prev_data <- read.csv("analysis/data_derived/validated_get_prevalence.csv")

# Read in Africa shape file generated in 01_generative_base_maps.R
rds_file_admin0 = "analysis/data_derived/sf_admin0_africa.rds"
rds_file_admin1 = "analysis/data_derived/sf_admin1_africa.rds"
africa_shp_admin0 <- readRDS(file = rds_file_admin0)
africa_shp_admin1 <- readRDS(file = rds_file_admin1)

# Calculate avg K13 prevalence for each lat, long and study_name
k13_avg_prevalence <- prev_data %>%
  group_by(latitude, longitude, study_name, country_name, site_name, collection_day, year, denominator) %>%
  summarize(k13_prevalence = sum(prevalence, na.rm = TRUE)) %>%
  ungroup()

# Define a custom color palette function
prevalence_palette <- function(n) {
  pal <- colorRampPalette(c("slategray2", "palegreen2", "khaki2", "orange", "red"))
  pal(n)
}

## Generate Africa plot of K13 prevalence with points sized by sample size
africa_map_points <- ggplot() +
  facet_wrap(~year) +
  geom_sf(data = africa_shp_admin0, fill = NA, color = "black", show.legend = FALSE, lwd = 0.1) +
  geom_point(data = filter(k13_avg_prevalence, k13_prevalence == 0),
             aes(x = longitude, y = latitude, size = denominator),
             color = "grey70", alpha = 0.8) +
  geom_point(data = filter(k13_avg_prevalence, k13_prevalence > 0),
             aes(x = longitude, y = latitude, color = k13_prevalence, size = denominator),
             alpha = 0.8) +
  scale_color_gradientn(
    name = "Prevalence (%)",
    colours = prevalence_palette(100),  # Smooth gradient
    na.value = "grey85"
  )+
  scale_size_continuous(name = "Sample Size (N)", range = c(1, 10)) +
  theme_void(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = "white")
  )

# Save plot
ggsave(
  filename = paste0("analysis/plots/africa_map_k13_points_all_years_newcolor.png"),
  plot = africa_map_points,
  width = 12, height = 10, units = "in", dpi = 300
)

## Plot K13 avg prevalence for 2012-2014, 2015-2017, 2018-2020, 2021-2023
# Group data into time and prevalence subsets
k13_avg_prevalence <- k13_avg_prevalence %>%
  mutate (
    year_group = case_when (
      year %in% 2012:2014 ~ "2012-2014",
      year %in% 2015:2017 ~ "2015-2017",
      year %in% 2018:2020 ~ "2018-2020",
      year %in% 2021:2023 ~ "2021-2023",
      TRUE ~ NA_character_  # exclude years outside these groups
    )
  ) %>%
  filter(!is.na(year_group)) %>%# drop early/incomplete years
  mutate(
    prevalence_bin = case_when(
      k13_prevalence == 0 ~ "0",
      k13_prevalence > 0 & k13_prevalence <= 1 ~ "0–1",
      k13_prevalence > 1 & k13_prevalence <= 5 ~ "1–5",
      k13_prevalence > 5 & k13_prevalence <= 10 ~ "5–10",
      k13_prevalence > 10 & k13_prevalence <= 20 ~ "10–20",
      k13_prevalence > 20 & k13_prevalence <= 30 ~ "20–30",
      k13_prevalence > 30 & k13_prevalence <= 40 ~ "30–40",
      k13_prevalence > 40 ~ "40+",
      TRUE ~ NA_character_
    ),
    prevalence_bin = factor(prevalence_bin, levels = c("0", "0–1", "1–5", "5–10", "10–20", "20–30", "30–40", "40+"))
  ) %>%
  arrange(k13_prevalence)

# Define prevlance color map
bins <- c("0", "0-1", "1-5", "5-10", "10-20", "20-30", "30-40", "40+")
prevalence_colors <- prevalence_palette(length(bins))
custom_colors <- c(
  "0" = "grey85",
  "0–1" = prevalence_colors[1],
  "1–5" = prevalence_colors[2],
  "5–10" = prevalence_colors[3],
  "10–20" = prevalence_colors[5],
  "20–30" = prevalence_colors[6],
  "30–40" = prevalence_colors[7],
  "40+" = prevalence_colors[8]
)

# Generate preva;nce plot of K13 points binned for 2012-2014, 2015-2017, 2018-2020, 2021-2023
africa_map_points <- ggplot() +
  facet_wrap(~year_group, nrow = 2) +
  geom_sf(data = africa_shp_admin0, fill = NA, color = "black", show.legend = FALSE, lwd = 0.1) +
  geom_point(
    data = k13_avg_prevalence,
    aes(x = longitude, y = latitude, size = denominator, color = prevalence_bin),
    alpha = 0.8
  ) +
  scale_color_manual(
    name = "Prevalence (%)",
    values = custom_colors,
    drop = FALSE
  ) +
  scale_size_continuous(name = "Sample Size (N)", range = c(1, 10)) + # Change the size of the points
  theme_void(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = "white")
  )

# Save plot
ggsave(
  filename = paste0("analysis/plots/africa_map_k13_points_binned_newcolor.png"),
  plot = africa_map_points,
  width = 12, height = 10, units = "in", dpi = 300
)

## Generate East Africa K13 prevalence plot
# Filter K13 prevalence data for
inset_data <- k13_avg_prevalence %>% filter(
  latitude >= -3.04, latitude <= 13.99,
  longitude >= 28.71, longitude <= 45.37
) %>%
  arrange(k13_prevalence)

# Define East Africa box
bbox <- st_bbox(c(
  xmin = 28.48,
  xmax = 48.43,
  ymin = -4.6,
  ymax = 15.29
), crs = st_crs(africa_shp_admin0))

# Spatial data processing
africa_shp_admin0_v01 <- st_make_valid(africa_shp_admin0)
africa_admin0_inset <- st_crop(africa_shp_admin0_v01, bbox)

# Plot East Africa
africa_inset <- ggplot() +
  facet_wrap(~year_group, nrow = 2) +
  geom_sf(data = africa_admin0_inset, fill = NA, color = "black", show.legend = FALSE, lwd = 0.1) +
  geom_point(
    data = inset_data,
    aes(x = longitude, y = latitude, size = denominator, color = prevalence_bin),
    alpha = 0.8
  ) +
  scale_color_manual(
    name = "Prevalence (%)",
    values = custom_colors,
    drop = FALSE
  ) +
  scale_size_continuous(name = "Sample Size (N)", range = c(1, 7)) +
  theme_void(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = "white")
  )

# Save plot
ggsave(
  filename = paste0("analysis/plots/EA_inset_map_k13_points_binned_new_color.png"),
  plot = africa_inset,
  width = 12, height = 10, units = "in", dpi = 300
)
