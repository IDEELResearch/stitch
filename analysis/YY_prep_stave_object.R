library(sf)       # for shapefile handling
library(ggplot2)  # for plotting
library(dplyr)    # for data manipulation
library(tidyr)    # for mild clean of stave data
library(usethis)  # req'd for devtools
library(here)     # req'd for devtools
library(devtools) # for github code check

devtools::check()
#devtools::check_win_release()
#devtools::document()
#devtools::load_all()

# Read in Africa shape file generated in 01_generative_base_maps.R
rds_file_admin0 = "analysis/data_derived/sf_admin0_africa.rds"
rds_file_admin1 = "analysis/data_derived/sf_admin1_africa.rds"
africa_shp_admin0 <- readRDS(file = rds_file_admin0)
africa_shp_admin1 <- readRDS(file = rds_file_admin1)

#Pull in Stave Data
#stave = readRDS(url("https://github.com/IDEELResearch/scrub/tree/931d07255aadc1e41005ed45c8345d0ac966b272/analysis/data-out"))
stave <- readRDS("/home/nwernsma/Documents/scrub/analysis/data-out/stave_data.rds")

# Clean Stave Data
library(countrycode)  # for coordinating STAVE country with iso3c code

# Get surveys from STAVE & identify countries for each lat lon
survey <- stave$get_surveys()

# Collapse entries to unique coords
sf_use_s2(FALSE)
unique_coords <- survey %>%
  select(latitude, longitude, site_name, country_name, study_key) %>%
  distinct() %>%
  mutate(
    lat = as.numeric(latitude),
    lon = as.numeric(longitude)
  ) %>%
  drop_na(lat, lon) %>%
  rename(study_ID = study_key) %>%
  mutate(entry_iso3c = countrycode(country_name, origin = "country.name", destination = "iso3c"))
unique_coords_sf <- st_as_sf(unique_coords, coords = c("lon", "lat"), crs = 4326)

# Align lat lon with admin shapefiles
coords_with_admin0 <- st_join(unique_coords_sf, africa_shp_admin0, join = st_within) %>%
  rename(admin0_iso3c = iso) %>%
  select(entry_iso3c, study_ID, admin0_iso3c)
  # Retain entry_iso3c and study_ID
coords_with_admin1 <- st_join(unique_coords_sf, africa_shp_admin1, join = st_within) %>%
  mutate(
    longitude = st_coordinates(.)[, 1],  # Extract longitude
    latitude = st_coordinates(.)[, 2]   # Extract latitude
  ) %>%
  select(longitude, latitude, site_name, name_1) %>%
  rename(admin1_name = name_1)

# Combine admin0 and admin1
coords_with_admin <- coords_with_admin1 %>%
  st_join(coords_with_admin0)

# Identify any general mismatches mismatches (currently 2 - 1 GEOFF, 1 WWARN), remove any non-African STAVE data (shape) - 628 studies
mismatch_rows <- coords_with_admin %>%
  filter(entry_iso3c != admin0_iso3c | !(entry_iso3c %in% admin0_iso3c)) %>%  # Compare entry_iso3c with admin0_iso3c
  select(longitude, latitude, site_name, admin1_name, entry_iso3c, admin0_iso3c, study_ID)

# Drop studies that are in mismatches
mismatch_studies <- unique(mismatch_rows$study_ID)
stave_clean = stave$clone()
stave_clean$drop_study(mismatch_studies)
# Confirm studies are dropped
stave$print()
stave_clean$print()

### Pull Validated and Candidate Prevalences ###
survey_clean <- stave_clean$get_surveys()
counts <- stave_clean$get_counts()
studies_clean <- stave_clean$get_studies()
#saveRDS(stave_clean, "analysis/data/stave_intermed_clean.rds")
stave_clean <- readRDS("analysis/data/stave_intermed_clean.rds")

validated_mutations <- c("k13:446:I", "k13:458:Y", "k13:469:Y", "k13:476:I",   "k13:493:H",   "k13:539:T",
                         "k13:543:T",  "k13:553:L",   "k13:561:H",   "k13:574:L",  "k13:580:Y",  "k13:622:I","k13:675:V")
candidate_mutations <- c("k13:441:L", "k13:449:A",   "k13:469:F",   "k13:481:V",
                         "k13:515:K", "k13:527:H",  "k13:537:I", "k13:537:D", "k13:538:V",  "k13:568:G")
all_who_mutations <- c("k13:446:I", "k13:458:Y", "k13:469:Y", "k13:476:I",   "k13:493:H",   "k13:539:T",
                       "k13:543:T",  "k13:553:L",   "k13:561:H",   "k13:574:L",  "k13:580:Y",  "k13:622:I","k13:675:V",
                       "k13:441:L", "k13:449:A",   "k13:469:F",   "k13:481:V",
                       "k13:515:K", "k13:527:H",  "k13:537:I", "k13:537:D", "k13:538:V",  "k13:568:G")

all_prev_data  <- data.frame()
#Loop over STAVE object to pull k13 mutations
# for (selected_mutation in all_who_mutations){
#   #call to stave for selected mutation, drop anything empty, set year from collection day
#   #TO DO: potentially filter by year depending on mutation? mdr1 data extends to 96
#   prevalence_data <- stave_clean$get_prevalence(selected_mutation) %>% drop_na() %>%
#     mutate(year = substring(collection_day,0,4))
#   prevalence_data$mutation <- selected_mutation
#   all_prev_data <- bind_rows(all_prev_data,prevalence_data)
# }
# write.csv(all_prev_data, "analysis/data/validated_get_prevalence.csv", row.names = FALSE)
# write.csv(all_prev_data, "analysis/data/validated_and_candidate_get_prevalence.csv", row.names = FALSE)

all_prev_data <- read.csv("analysis/data/validated_and_candidate_get_prevalence.csv",header = TRUE)

## Manipulating all-prev-data into overall k13 map ##

k13_avg_prevalence <- all_prev_data %>% group_by(latitude, longitude, study_name, country_name, site_name,collection_day, year,denominator) %>%
  summarize(k13_prevalence = sum(prevalence, na.rm = TRUE)) %>%
  ungroup()

africa_map_points <- ggplot() +
  facet_wrap(~year) +
  geom_sf(data = africa_shp_admin0, fill = NA, color = "black", show.legend = FALSE, lwd = 0.1) +

  # Plot zero-prevalence points in grey
  geom_point(data = filter(k13_avg_prevalence, k13_prevalence == 0),
             aes(x = longitude, y = latitude, size = denominator),
             color = "grey70", alpha = 0.8) +

  # Plot non-zero prevalence points with color scale
  geom_point(data = filter(k13_avg_prevalence, k13_prevalence > 0),
             aes(x = longitude, y = latitude, color = k13_prevalence, size = denominator),
             alpha = 0.8) +

  #scale_color_viridis_c(name = "Prevalence") +
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

ggsave(
  filename = paste0("analysis/plots/africa_map_k13_points_all_years_newcolor.png"),
  plot = africa_map_points,
  width = 12, height = 10, units = "in", dpi = 300
)

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


library(scales)

prevalence_palette <- function(n) {
  pal <- colorRampPalette(c("slategray2", "palegreen2", "khaki2", "orange", "red"))
  pal(n)
}

bins <- c("0", "0-1", "1-5", "5-10", "10-20", "20-30", "30-40", "40+")

prevalence_colors <- prevalence_palette(length(bins))
#names(prevalence_colors) <- bins

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
  #continuous color scale
  # scale_color_gradientn(
  #   name = "Prevalence (%)",
  #   colours = prevalence_palette(100),  # Smooth gradient
  #   na.value = "grey85"
  # )
  scale_size_continuous(name = "Sample Size (N)", range = c(1, 10)) + # Change the size of the points
  theme_void(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = "white")
  )

ggsave(
  filename = paste0("analysis/plots/africa_map_k13_points_binned_newcolor.png"),
  plot = africa_map_points,
  width = 12, height = 10, units = "in", dpi = 300
)


##INSET OF EAST AFRICA ##
inset_data <- k13_avg_prevalence %>% filter(
  latitude >= -3.04, latitude <= 13.99,
  longitude >= 28.71, longitude <= 45.37
) %>%
  arrange(k13_prevalence)

bbox <- st_bbox(c(
  xmin = 28.48,
  xmax = 48.43,
  ymin = -4.6,
  ymax = 15.29
), crs = st_crs(africa_shp_admin0))

africa_shp_admin0_v01 <- st_make_valid(africa_shp_admin0)
africa_admin0_inset <- st_crop(africa_shp_admin0_v01, bbox)

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

ggsave(
  filename = paste0("analysis/plots/EA_inset_map_k13_points_binned_new_color.png"),
  plot = africa_inset,
  width = 12, height = 10, units = "in", dpi = 300
)



## general stats ##

#how many countries have observed ArtR markers
#first and last observed
temporal <- all_prev_data %>% filter(mutation %in% all_who_mutations) %>%
  filter(prevalence > 0) %>%
  group_by(mutation) %>%
  summarize (
    first_year = min(collection_start, na.rm = TRUE),
    last_year = max(collection_start, na.rm = TRUE),
    n_sites = n(),
    n_countries = n_distinct(country_name),
    .groups = "drop"
  ) %>%
  arrange(first_year)
