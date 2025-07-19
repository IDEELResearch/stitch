# author: CMS and NWY
# description: This file converts the stave object into prevalence data for selected mutations

library(sf)       # for shapefile handling
library(ggplot2)  # for plotting
library(dplyr)    # for data manipulation
library(tidyr)    # for mild clean of stave data
library(usethis)  # req'd for devtools
library(here)     # req'd for devtools
library(devtools) # for github code check
library(countrycode) # for coordinating STAVE country with iso3c code

# disables the use of the S2 geometry engine for spatial operations
sf_use_s2(FALSE)

#Pull in Stave Data
stave <- readRDS("analysis/data_derived/stave_final_data.rds")

# Get surveys from STAVE & identify countries for each lat lon
survey <- stave$get_surveys()

# Collapse entries to unique coords
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

# To-do: delete lines 60-73
# # Identify any general mismatches mismatches (currently 2 - 1 GEOFF, 1 WWARN), remove any non-African STAVE data (shape) - 628 studies
# mismatch_rows <- coords_with_admin %>%
#   filter(entry_iso3c != admin0_iso3c | !(entry_iso3c %in% admin0_iso3c)) %>%  # Compare entry_iso3c with admin0_iso3c
#   select(longitude, latitude, site_name, admin1_name, entry_iso3c, admin0_iso3c, study_ID)
#
# # Drop studies that are in mismatches
# mismatch_studies <- unique(mismatch_rows$study_ID)
# stave_clean = stave$clone()
# stave_clean$drop_study(mismatch_studies)
# # Confirm studies are dropped
# stave$print()
# stave_clean$print()
# stave_clean$get_counts()

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
start_time <- Sys.time()

for (i in seq_along(validated_mutations)) {
  selected_mutation <- validated_mutations[i]
  print(paste0("Processing: ", selected_mutation))

  prevalence_data <- stave_clean$get_prevalence(selected_mutation) %>%
    drop_na() %>%
    mutate(year = substring(collection_day, 0, 4))
  prevalence_data$mutation <- selected_mutation
  all_prev_data <- bind_rows(all_prev_data, prevalence_data)

  # Mid-loop time logging
  current_time <- Sys.time()
  elapsed <- round(difftime(current_time, start_time, units = "secs"), 2)
  print(paste("Elapsed time:", elapsed, "seconds (", i, "of", length(validated_mutations), ")"))
}

end_time <- Sys.time()
print(paste("Total time taken:", round(difftime(end_time, start_time, units = "secs"), 2), "seconds"))

# write.csv(all_prev_data, "analysis/data/validated_get_prevalence.csv", row.names = FALSE)
# write.csv(all_prev_data, "analysis/data/validated_and_candidate_get_prevalence.csv", row.names = FALSE)

all_prev_data <- read.csv("analysis/data/validated_and_candidate_get_prevalence.csv",header = TRUE)
