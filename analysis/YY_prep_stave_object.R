library(sf)       # for shapefile handling
library(ggplot2)  # for plotting
library(dplyr)    # for data manipulation
library(tidyr)    # for mild clean of stave data
library(here)
library(devtools)

devtools::check()
devtools::check_win_release()

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
saveRDS(stave_clean, "analysis/data/stave_intermed_clean.rds")

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

  scale_color_viridis_c(name = "Prevalence") +
  scale_size_continuous(name = "Sample Size (N)", range = c(1, 10)) +
  theme_void(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = "white")
  )

ggsave(
  filename = paste0("analysis/plots/africa_map_k13_points_all_years_facet.png"),
  plot = africa_map_points,
  width = 12, height = 10, units = "in", dpi = 300
)

k13_avg_prevalence <- k13_avg_prevalence %>%
  mutate (
    year_group = case_when (
      year %in% 2010:2014 ~ "2010-2014",
      year %in% 2015:2019 ~ "2015-2019",
      year == 2020 ~ "2020",
      year == 2021 ~ "2021",
      year == 2022 ~ "2022",
      year == 2023 ~ "2023",
      TRUE ~ NA_character_  # exclude years outside these groups
    )
  ) %>%
    filter(!is.na(year_group))  # drop early/incomplete years

africa_map_points <- ggplot() +
  facet_wrap(~year_group) +
  geom_sf(data = africa_shp_admin0, fill = NA, color = "black", show.legend = FALSE, lwd = 0.1) +

  # Plot zero-prevalence points in grey
  geom_point(data = filter(k13_avg_prevalence, k13_prevalence == 0),
             aes(x = longitude, y = latitude, size = denominator),
             color = "grey70", alpha = 0.8) +

  # Plot non-zero prevalence points with color scale
  geom_point(data = filter(k13_avg_prevalence, k13_prevalence > 0),
             aes(x = longitude, y = latitude, color = k13_prevalence, size = denominator),
             alpha = 0.8) +

  scale_color_viridis_c(name = "Prevalence") +
  scale_size_continuous(name = "Sample Size (N)", range = c(1, 10)) +
  theme_void(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = "white")
  )

ggsave(
  filename = paste0("analysis/plots/africa_map_k13_points_grey_zeros_facet.png"),
  plot = africa_map_points,
  width = 12, height = 10, units = "in", dpi = 300
)
