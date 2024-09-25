library(sf)       # for shapefile handling
library(ggplot2)  # for plotting
library(dplyr)    # for data manipulation

# Expanded dataframe with 15 African countries and varying admin 1 regions
prevalence_data <- data.frame(
  country = c("Uganda", "Uganda", "Uganda",
              "Kenya", "Kenya", "Kenya",
              "Tanzania", "Tanzania", "Tanzania",
              "Nigeria", "Nigeria", "Nigeria", "Nigeria",
              "Ghana", "Ghana",
              "South Africa", "South Africa", "South Africa",
              "Zambia", "Zambia",
              "Zimbabwe", "Zimbabwe", "Zimbabwe",
              "Malawi",
              "Ethiopia", "Ethiopia", "Ethiopia", "Ethiopia",
              "Senegal", "Senegal",
              "Côte d'Ivoire", "Côte d'Ivoire", "Côte d'Ivoire", "Côte d'Ivoire",
              "Angola", "Angola", "Angola", "Angola", "Angola",
              "Rwanda", "Rwanda", "Rwanda",
              "Burkina Faso", "Burkina Faso", "Burkina Faso", "Burkina Faso",
              "Togo", "Togo",
              "Cameroon", "Cameroon", "Cameroon", "Cameroon", "Cameroon",
              "Mali", "Mali", "Mali",
              "Gambia",
              "Somalia", "Somalia", "Somalia",
              "Namibia", "Namibia", "Namibia", "Namibia"),
  name_1 = c("Central", "Eastern", "Northern",  # Uganda
             "Nairobi", "Mombasa", "Kisumu",  # Kenya
             "Dodoma", "Arusha", "Mwanza",  # Tanzania
             "Lagos", "Kano", "Abuja", "Port Harcourt",  # Nigeria
             "Greater Accra", "Ashanti",  # Ghana
             "Gauteng", "Western Cape", "KwaZulu-Natal",  # South Africa
             "Lusaka", "Copperbelt",  # Zambia
             "Harare", "Bulawayo", "Manicaland",  # Zimbabwe
             "Blantyre",  # Malawi
             "Addis Ababa", "Oromia", "Amhara", "Tigray",  # Ethiopia
             "Dakar", "Thies",  # Senegal
             "Abidjan", "Bouaké", "Yamoussoukro", "Korhogo",  # Côte d'Ivoire
             "Luanda", "Huambo", "Benguela", "Malanje", "Kwanza Sul",  # Angola
             "Kigali", "Western", "Northern",  # Rwanda
             "Centre", "Boucle du Mouhoun", "Hauts-Bassins", "Sud-Ouest",  # Burkina Faso
             "Maritime", "Plateaux",  # Togo
             "Littoral", "Ouest", "Nord-Ouest", "Sud-Ouest", "Adamaoua",  # Cameroon
             "Bamako", "Kayes", "Sikasso",  # Mali
             "Banjul",  # Gambia
             "Mogadishu", "South West", "Galmudug",  # Somalia
             "Khomas", "Erongo", "Otjozondjupa", "Hardap"),  # Namibia,  # Namibia
  year = sample(2018:2022, 64, replace = TRUE),  # Random years between 2018 and 2022
  sample_size = sample(0:180, 64, replace = TRUE),
  prevalence = sample(seq(0,1,0.001), 64, replace = TRUE)
)

# Read in Africa shape file
rds_file_admin0 = "analysis/data_derived/sf_admin0_africa.rds"
rds_file_admin1 = "analysis/data_derived/sf_admin1_africa.rds"
africa_shp_admin0 <- readRDS(file = rds_file_admin0)
africa_shp_admin1 <- readRDS(file = rds_file_admin1)

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



