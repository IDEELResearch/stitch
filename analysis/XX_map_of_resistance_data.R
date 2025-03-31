library(sf)       # for shapefile handling
library(ggplot2)  # for plotting
library(dplyr)    # for data manipulation
library(tidyr)    # for mild clean of stave data


#neeva learns how to commit with git 

# Expanded dataframe with 15 African countries and varying admin 1 regions
# PRACTICE DATA
#####
# prevalence_data <- data.frame(
#   country = c("Uganda", "Uganda", "Uganda",
#               "Kenya", "Kenya", "Kenya",
#               "Tanzania", "Tanzania", "Tanzania",
#               "Nigeria", "Nigeria", "Nigeria", "Nigeria",
#               "Ghana", "Ghana",
#               "South Africa", "South Africa", "South Africa",
#               "Zambia", "Zambia",
#               "Zimbabwe", "Zimbabwe", "Zimbabwe",
#               "Malawi",
#               "Ethiopia", "Ethiopia", "Ethiopia", "Ethiopia",
#               "Senegal", "Senegal",
#               "Côte d'Ivoire", "Côte d'Ivoire", "Côte d'Ivoire", "Côte d'Ivoire",
#               "Angola", "Angola", "Angola", "Angola", "Angola",
#               "Rwanda", "Rwanda", "Rwanda",
#               "Burkina Faso", "Burkina Faso", "Burkina Faso", "Burkina Faso",
#               "Togo", "Togo",
#               "Cameroon", "Cameroon", "Cameroon", "Cameroon", "Cameroon",
#               "Mali", "Mali", "Mali",
#               "Gambia",
#               "Somalia", "Somalia", "Somalia",
#               "Namibia", "Namibia", "Namibia", "Namibia"),
#   name_1 = c("Central", "Eastern", "Northern",  # Uganda
#              "Nairobi", "Mombasa", "Kisumu",  # Kenya
#              "Dodoma", "Arusha", "Mwanza",  # Tanzania
#              "Lagos", "Kano", "Abuja", "Port Harcourt",  # Nigeria
#              "Greater Accra", "Ashanti",  # Ghana
#              "Gauteng", "Western Cape", "KwaZulu-Natal",  # South Africa
#              "Lusaka", "Copperbelt",  # Zambia
#              "Harare", "Bulawayo", "Manicaland",  # Zimbabwe
#              "Blantyre",  # Malawi
#              "Addis Ababa", "Oromia", "Amhara", "Tigray",  # Ethiopia
#              "Dakar", "Thies",  # Senegal
#              "Abidjan", "Bouaké", "Yamoussoukro", "Korhogo",  # Côte d'Ivoire
#              "Luanda", "Huambo", "Benguela", "Malanje", "Kwanza Sul",  # Angola
#              "Kigali", "Western", "Northern",  # Rwanda
#              "Centre", "Boucle du Mouhoun", "Hauts-Bassins", "Sud-Ouest",  # Burkina Faso
#              "Maritime", "Plateaux",  # Togo
#              "Littoral", "Ouest", "Nord-Ouest", "Sud-Ouest", "Adamaoua",  # Cameroon
#              "Bamako", "Kayes", "Sikasso",  # Mali
#              "Banjul",  # Gambia
#              "Mogadishu", "South West", "Galmudug",  # Somalia
#              "Khomas", "Erongo", "Otjozondjupa", "Hardap"),  # Namibia,  # Namibia
#   year = sample(2018:2022, 64, replace = TRUE),  # Random years between 2018 and 2022
#   sample_size = sample(0:180, 64, replace = TRUE),
#   prevalence = sample(seq(0,1,0.001), 64, replace = TRUE)
# )
#####

# Read in Africa shape file generated in 01_generative_base_maps.R
rds_file_admin0 = "analysis/data_derived/sf_admin0_africa.rds"
rds_file_admin1 = "analysis/data_derived/sf_admin1_africa.rds"
africa_shp_admin0 <- readRDS(file = rds_file_admin0)
africa_shp_admin1 <- readRDS(file = rds_file_admin1)

#Pull in Stave Data 
stave = readRDS(url("https://github.com/IDEELResearch/scrub/blob/main/analysis/data-out/stave_data.rds"))

# Clean Stave Data
library(countrycode)  # for coordinating STAVE country with iso3c code 

# Get surveys from STAVE & identify countries for each lat lon
survey <- stave$get_surveys()
# Collapse entries to unique coords
unique_coords <- survey %>%
  select(lat, lon, site_name, country_name, study_key) %>%
  distinct() %>%
  mutate(
    lat = as.numeric(lat),
    lon = as.numeric(lon)
  ) %>%
  drop_na(lat, lon) %>%
  rename(study_ID = study_key) %>%
  mutate(entry_iso3c = countrycode(country_name, origin = "country.name", destination = "iso3c"))
unique_coords_sf <- st_as_sf(unique_coords, coords = c("lon", "lat"), crs = 4326)
# Align lat lon with admin shapefiles
coords_with_admin0 <- st_join(unique_coords_sf, africa_shp_admin0, join = st_within) %>%
  select(entry_iso3c, study_ID, iso) %>%  # Retain entry_iso3c and study_ID
  rename(admin0_iso3c = iso)
coords_with_admin1 <- st_join(unique_coords_sf, africa_shp_admin1, join = st_within) %>%
  mutate(
    lon = st_coordinates(.)[, 1],  # Extract longitude
    lat = st_coordinates(.)[, 2]   # Extract latitude
  ) %>%
  select(lon, lat, site_name, name_1) %>%
  rename(admin1_name = name_1)
# Combine admin0 and admin1 
coords_with_admin <- coords_with_admin1 %>%
  st_join(coords_with_admin0)
# Identify any general mismatches mismatches, remove any non-African STAVE data (shape)
mismatch_rows <- coords_with_admin %>%
  filter(entry_iso3c != admin0_iso3c | !(entry_iso3c %in% admin0_iso3c)) %>%  # Compare entry_iso3c with admin0_iso3c
  select(lon, lat, site_name, admin1_name, entry_iso3c, admin0_iso3c, study_ID)
# Drop studies that are in mismatches
mismatch_studies <- unique(mismatch_rows$study_ID)
stave_clean = stave$clone()
stave_clean$drop_study(mismatch_studies)
# Confirm studies are dropped
stave$print()
stave_clean$print()

### START MAPPING ###
survey_clean <- stave_clean$get_surveys()
counts <- stave_clean$get_counts()
studies_clean <- stave_clean$get_studies()






# Calculate the centroids for each MULTIPOLYGON
sf_use_s2(FALSE)
africa_admin1_longlat <- africa_shp_admin1 %>%
  st_geometry() %>% #obtain geometry
  st_centroid() %>% #obtain centroid of geometry
  st_coordinates() %>% #obtain coordinates of the centroids (long, lat)
  cbind(africa_shp_admin1) %>%
  rename("lon" = "X",
         "lat" = "Y")

# Add prevalence data to admin 1 shape file
africa_admin1_longlat_prev <- africa_admin1_longlat %>%
  left_join(prevalence_data, by = "name_1") %>%
  filter(!is.na(sample_size))

# Plot the Africa map coloring districts by sample_size
africa_map_sample_fill <- ggplot() +
  facet_wrap(~year) +
  geom_sf(data = africa_admin1_longlat_prev, aes(fill = sample_size), color = "darkgrey", lwd = 0.05) +
  geom_sf(data = africa_shp_admin0, fill = NA, color = "black", show.legend = FALSE, lwd = 0.05) +
  theme_void(base_size = 14) +
  labs(fill = "Sample Size (N)") +
  scale_fill_viridis_c() +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color="white"))

ggsave(filename="analysis/plots/africa_map_sample_fill.png", africa_map_sample_fill)


#Loop over calls to cleaned stave product to map validated_mutations by year -> Supplemental Figure

validated_mutations <- c("k13:469:Y", "k13:469:F", "k13:622:I", "k13:561:H", "k13:441:L", "k13:675:V")
selected_mutation = "mdr1:86:Y"

#prep stave objects by aligning coordinates and names
#where survey_clean is the get_survey output of a clean STAVE object
coords_clean <- survey_clean %>%
  select(lat, lon, site_name, country_name, study_key) %>%
  distinct() %>%
  mutate(
    lat = as.numeric(lat),
    lon = as.numeric(lon)
  ) %>%
  drop_na(lat, lon) %>%
  rename(study_ID = study_key) %>%
  mutate(entry_iso3c = countrycode(country_name, origin = "country.name", destination = "iso3c"))
coords_clean_sf <- st_as_sf(coords_clean, coords = c("lon", "lat"), crs = 4326)
clean_admin1 <- st_join(coords_clean_sf, africa_shp_admin1, join = st_within) %>%
  mutate(
    lon = st_coordinates(.)[, 1],  # Extract longitude
    lat = st_coordinates(.)[, 2]   # Extract latitude
  ) %>%
  select(study_ID, lon, lat, site_name, name_1)



for (selected_mutation in validated_mutations){
  #call to stave for selected mutation, drop anything empty, set year from collection day
  #TO DO: potentially filter by year depending on mutation? mdr1 data extends to 96
  prevalence_data <- stave_clean$get_prevalence(selected_mutation) %>% drop_na() %>%
    mutate(year = substring(collection_day,0,4))
  
  #associate prevalence data lat lons with clean admin_1 names
  prevalence_data <- left_join(prevalence_data, clean_admin1) %>% drop_na()
  
  # Add prevalence data to admin 1 shape file
  africa_admin1_longlat_prev <- africa_admin1_longlat %>%
    left_join(prevalence_data, by = "name_1") %>%
    filter(!is.na(denominator))
  
  # Plot the Africa map coloring districts by prevalence_size
  africa_map_sample_fill <- ggplot() +
    facet_wrap(~year) +
    geom_sf(data = africa_admin1_longlat_prev, aes(fill = prevalence), color = "darkgrey", lwd = 0.05) +
    geom_sf(data = africa_shp_admin0, fill = NA, color = "black", show.legend = FALSE, lwd = 0.05) +
    theme_void(base_size = 14) +
    labs(fill = "Prevalence (N)") +
    scale_fill_viridis_c() +
    theme(legend.position = "bottom",
          plot.background = element_rect(fill = "white", color="white"))
  
  ggsave(
    filename=paste0("analysis/plots/africa_map_", gsub(":","_", selected_mutation), "prev_facet.png"),
    plot = africa_map_sample_fill,
    width = 12, height = 10, units = "in", dpi = 300)
  
  # Plot the Africa map coloring districts by sample size
  africa_map_sample_fill <- ggplot() +
    facet_wrap(~year) +
    geom_sf(data = africa_admin1_longlat_prev, aes(fill = denominator), color = "darkgrey", lwd = 0.05) +
    geom_sf(data = africa_shp_admin0, fill = NA, color = "black", show.legend = FALSE, lwd = 0.05) +
    theme_void(base_size = 14) +
    labs(fill = "Sample Size (N)") +
    scale_fill_viridis_c() +
    theme(legend.position = "bottom",
          plot.background = element_rect(fill = "white", color="white"))
  
  ggsave(
    filename=paste0("analysis/plots/africa_map_", gsub(":","_", selected_mutation), "facet.png"),
    plot = africa_map_sample_fill,
    width = 12, height = 10, units = "in", dpi = 300)
  
  
  print(paste("Save plot for:", selected_mutation))
}
