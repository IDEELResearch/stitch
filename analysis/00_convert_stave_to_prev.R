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
stave <- readRDS("analysis/data_raw/stave_final_data.rds")

# Get surveys from STAVE & identify countries for each lat lon
survey <- stave_df$get_surveys()

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

### Pull All Candidate and Validated Prevalences ###
all_who_mutations <- c("k13:446:I", "k13:458:Y", "k13:469:Y", "k13:476:I",   "k13:493:H",   "k13:539:T",
                       "k13:543:T",  "k13:553:L",   "k13:561:H",   "k13:574:L",  "k13:580:Y",  "k13:622:I","k13:675:V",
                       "k13:441:L", "k13:449:A",   "k13:469:F",   "k13:481:V",
                       "k13:515:K", "k13:527:H",  "k13:537:I", "k13:537:D", "k13:538:V",  "k13:568:G")

all_who_prev_data  <- data.frame()
#Loop over STAVE object to pull k13 mutations
start_time <- Sys.time()

for (i in seq_along(all_who_mutations)) {
  mutation_time <- Sys.time()
  selected_mutation <- all_who_mutations[i]
  print(paste0("Processing: ", selected_mutation))

  prevalence_data <- stave$get_prevalence(selected_mutation) %>%
    drop_na() %>%
    mutate(year = substring(collection_day, 0, 4))
  prevalence_data$mutation <- selected_mutation
  all_who_prev_data <- bind_rows(all_who_prev_data, prevalence_data)

  # Mid-loop time logging
  current_time <- Sys.time()
  elapsed <- round(difftime(current_time, mutation_time, units = "secs"), 2)
  print(paste("Elapsed time:", elapsed, "seconds (", i, "of", length(all_who_mutations), ")"))
}

end_time <- Sys.time()
print(paste("Total time taken:", round(difftime(end_time, start_time, units = "secs"), 2), "seconds"))

write.csv(all_who_prev_data, "analysis/data_derived/all_who_get_prevalence.csv", row.names = FALSE)

all_who_prev_data <- read.csv("analysis/data_derived/all_who_get_prevalence.csv")
# Filter to only include African countries
african_countries <- c(
  "Mali", "Mozambique", "Senegal", "Ghana", "Ethiopia", "Kenya", "Madagascar",
  "Gambia", "Guinea-Bissau", "Congo - Brazzaville", "Burundi", "South Africa",
  "Guinea", "Chad", "Tanzania", "Sierra Leone", "Zimbabwe", "Somalia",
  "South Sudan", "Sudan", "Rwanda", "Congo - Kinshasa", "Gabon", "Malawi",
  "Togo", "Angola", "Zambia", "Equatorial Guinea", "Uganda", "Liberia",
  "Comoros", "Niger", "Mauritania", "Burkina Faso", "Benin", "Côte d’Ivoire",
  "Algeria", "Nigeria", "Cameroon", "Central African Republic",
  "Democratic Republic of the Congo", "Eritrea", "São Tomé & Príncipe",
  "Côte d'Ivoire", "Libya"
)
all_who_prev_data_africa <- all_who_prev_data %>% filter(country_name %in% african_countries)

# Check that an African country was not excluded
all_who_prev_data_NOTafrica <- all_who_prev_data %>% filter(!country_name %in% african_countries)

write.csv(all_who_prev_data_africa, "analysis/data_derived/all_who_get_prevalence_africa.csv", row.names = FALSE)

### Pull Validated Prevalences ###
validated_mutations <- c("k13:446:I", "k13:458:Y", "k13:469:Y", "k13:476:I",   "k13:493:H",   "k13:539:T",
                         "k13:543:T",  "k13:553:L",   "k13:561:H",   "k13:574:L",  "k13:580:Y",  "k13:622:I","k13:675:V")

validate_prev_data  <- data.frame()
#Loop over STAVE object to pull k13 mutations
start_time <- Sys.time()

for (i in seq_along(validated_mutations)) {
  selected_mutation <- validated_mutations[i]
  print(paste0("Processing: ", selected_mutation))

  prevalence_data <- stave$get_prevalence(selected_mutation) %>%
    drop_na() %>%
    mutate(year = substring(collection_day, 0, 4))
  prevalence_data$mutation <- selected_mutation
  validate_prev_data <- bind_rows(validate_prev_data, prevalence_data)

  # Mid-loop time logging
  current_time <- Sys.time()
  elapsed <- round(difftime(current_time, start_time, units = "secs"), 2)
  print(paste("Elapsed time:", elapsed, "seconds (", i, "of", length(validated_mutations), ")"))
}

end_time <- Sys.time()
print(paste("Total time taken:", round(difftime(end_time, start_time, units = "secs"), 2), "seconds"))

write.csv(validate_prev_data, "analysis/data-derived/validated_get_prevalence.csv",header = TRUE)

### Pull Candidate Prevalence ###

candidate_mutations <- c("k13:441:L", "k13:449:A",   "k13:469:F",   "k13:481:V",
                         "k13:515:K", "k13:527:H",  "k13:537:I", "k13:537:D", "k13:538:V",  "k13:568:G")

candidate_prev_data  <- data.frame()
#Loop over STAVE object to pull k13 mutations
start_time <- Sys.time()

for (i in seq_along(candidate_mutations)) {
  selected_mutation <- candidate_mutations[i]
  print(paste0("Processing: ", selected_mutation))

  prevalence_data <- stave$get_prevalence(selected_mutation) %>%
    drop_na() %>%
    mutate(year = substring(collection_day, 0, 4))
  prevalence_data$mutation <- selected_mutation
  candidate_prev_data <- bind_rows(candidate_prev_data, prevalence_data)

  # Mid-loop time logging
  current_time <- Sys.time()
  elapsed <- round(difftime(current_time, start_time, units = "secs"), 2)
  print(paste("Elapsed time:", elapsed, "seconds (", i, "of", length(candidate_mutations), ")"))
}

end_time <- Sys.time()
print(paste("Total time taken:", round(difftime(end_time, start_time, units = "secs"), 2), "seconds"))

write.csv(candidate_prev_data, "analysis/data-derived/candidate_get_prevalence.csv",header = TRUE)
