library(sf)       # for shapefile handling
library(ggplot2)  # for plotting
library(dplyr)    # for data manipulation
library(tidyr)    # for mild clean of stave data

# Read in Africa shape file generated in 01_generative_base_maps.R
rds_file_admin0 = "analysis/data_derived/sf_admin0_africa.rds"
rds_file_admin1 = "analysis/data_derived/sf_admin1_africa.rds"
africa_shp_admin0 <- readRDS(file = rds_file_admin0)
africa_shp_admin1 <- readRDS(file = rds_file_admin1)

#Pull in Stave Data from cleaned & selected prevalence script
stave_clean <- readRDS("analysis/data/stave_intermed_clean.rds")
all_prev_data <- read.csv("analysis/data/validated_and_candidate_get_prevalence.csv", header = TRUE)

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
# africa_admin1_longlat_prev <- africa_admin1_longlat %>%
#   left_join(prevalence_data, by = "name_1") %>%
#   filter(!is.na(denominator))


#Loop over calls to cleaned stave product to map validated_mutations by year -> Supplemental Figure

#validated_mutations <- c("k13:469:Y", "k13:469:F", "k13:622:I", "k13:561:H", "k13:441:L", "k13:675:V")
all_who_mutations <- c("k13:446:I", "k13:458:Y", "k13:469:Y", "k13:476:I",   "k13:493:H",   "k13:539:T",
                       "k13:543:T",  "k13:553:L",   "k13:561:H",   "k13:574:L",  "k13:580:Y",  "k13:622:I","k13:675:V",
                       "k13:441:L", "k13:449:A",   "k13:469:F",   "k13:481:V",
                       "k13:515:K", "k13:527:H",  "k13:537:I", "k13:537:D", "k13:538:V",  "k13:568:G")

selected_mutation = "k13:561:H"

#prep stave objects by aligning coordinates and names
#where survey_clean is the get_survey output of a clean STAVE object
library(countrycode)
coords_clean <- survey_clean %>%
  select(latitude, longitude, site_name, country_name, study_key) %>%
  distinct() %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
#  drop_na(lat, lon) %>%
  rename(study_ID = study_key) %>%
  mutate(entry_iso3c = countrycode(country_name, origin = "country.name", destination = "iso3c"))
coords_clean_sf <- st_as_sf(coords_clean, coords = c("longitude", "latitude"), crs = 4326)
clean_admin1 <- st_join(coords_clean_sf, africa_shp_admin1, join = st_within) %>%
  mutate(
    longitude = st_coordinates(.)[, 1],  # Extract longitude
    latitude = st_coordinates(.)[, 2]   # Extract latitude
  ) %>%
  select(study_ID, longitude, latitude, site_name, name_1)

#need to get loop to find the year of the first mutation then select around that
#' ie we need to skip 580Y etc
#'
prevalence_palette <- function(n) {
  pal <- colorRampPalette(c("slategray2", "palegreen2", "khaki2", "orange", "red"))
  pal(n)
}
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

bin_years <- function(prevalence_data, bin_size = 3) { #take in dataframe with column "prevalence" and column year
  prevalence_data <- prevalence_data %>% mutate(year = as.numeric(as.character(year)))

  first_year <- as.character(as.numeric(min(prevalence_data$year[which(prevalence_data$prevalence > 0)]))-1)
  current_year <- max(prevalence_data$year[which(prevalence_data$prevalence > 0)])

  breaks <- seq(first_year, current_year, by = bin_size)
  if (current_year > max(breaks)) {
    breaks <- c(breaks, current_year + 1)  # Add a final bin for leftovers
  } else {
    breaks <- c(breaks, max(breaks) + bin_size)
  }

  labels <- sapply(seq_along(breaks[-1]), function(i) {
    start <- breaks[i]
    end <- breaks[i + 1] - 1
    if (start == end) as.character(start) else paste0(start, "-", end)
  })
  prevalence_data <- prevalence_data %>%
    mutate(
      year_group = cut(
        year,
        breaks = breaks,
        labels = labels,
        include.lowest = TRUE,
        right = TRUE
      )
    )  %>% filter(!is.na(year_group)) # drop early/incomplete years
  return(prevalence_data)
}

selected_mutation = "k13:561:H"

for (selected_mutation in all_who_mutations){
  #call to stave for selected mutation, drop anything empty, set year from collection day
  #TO DO: potentially filter by year depending on mutation? mdr1 data extends to 96
  prevalence_data <- all_prev_data %>% filter(mutation == selected_mutation)
  #associate prevalence data lat lons with clean admin_1 names
  prevalence_data <- left_join(prevalence_data, clean_admin1) %>% drop_na() %>%
    mutate(year = substring(collection_day,0,4))

  if(length(which(prevalence_data$prevalence >0)) == 0 ) {
    next
  }
  prevalence_data <- bin_years(prevalence_data)

  prevalence_data <- prevalence_data %>%   mutate(
      prevalence_bin = case_when(
        prevalence == 0 ~ "0",
        prevalence > 0 & prevalence <= 1 ~ "0–1",
        prevalence > 1 & prevalence <= 5 ~ "1–5",
        prevalence > 5 & prevalence <= 10 ~ "5–10",
        prevalence > 10 & prevalence <= 20 ~ "10–20",
        prevalence > 20 & prevalence <= 30 ~ "20–30",
        prevalence > 30 & prevalence <= 40 ~ "30–40",
        prevalence > 40 ~ "40+",
        TRUE ~ NA_character_
      ),
      prevalence_bin = factor(prevalence_bin, levels = c("0", "0–1", "1–5", "5–10", "10–20", "20–30", "30–40", "40+"))
    ) %>%
    mutate(prevalence = as.numeric(prevalence)) %>%
    arrange(prevalence)

  # Plot the Africa map coloring districts by prevalence_size
  africa_map_sample_fill <- ggplot() +
    facet_wrap(~year_group) +
    geom_sf(data = africa_shp_admin0, fill = NA, color = "black", show.legend = FALSE, lwd = 0.05) +
    geom_point(
      data = prevalence_data,
      aes(x = longitude, y = latitude, size = denominator, color = prevalence_bin),
      alpha = 0.8
    ) +
    scale_color_manual(
      name = "Prevalence (%)",
      values = custom_colors,
      drop = FALSE
    ) +
    scale_size_continuous(name = "Sample Size (N)", range = c(1, 6)) + # Change the size of the points
    theme_void(base_size = 14) +
    theme(legend.position = "bottom",
          plot.background = element_rect(fill = "white", color="white"))

  ggsave(
    filename=paste0("analysis/plots/Indivduals/africa_map_", gsub(":","_", selected_mutation), "prev_facet.png"),
    plot = africa_map_sample_fill,
    width = 12, height = 10, units = "in", dpi = 300)

  # # Plot the Africa map coloring districts by sample size
  # africa_map_sample_fill <- ggplot() +
  #   facet_wrap(~year_group) +
  #   #geom_sf(data = africa_admin1_longlat_prev, aes(fill = denominator), color = "darkgrey", lwd = 0.05) +
  #   geom_sf(data = africa_shp_admin0, fill = NA, color = "black", show.legend = FALSE, lwd = 0.05) +
  #   geom_point(
  #     data = africa_admin1_longlat_prev,
  #     aes(x = longitude, y = latitude, size = denominator, color = prevalence_bin),
  #     alpha = 0.8
  #   ) +
  #   theme_void(base_size = 14) +
  #   labs(fill = "Sample Size (N)") +
  #   scale_fill_viridis_c() +
  #   theme(legend.position = "bottom",
  #         plot.background = element_rect(fill = "white", color="white"))
  #
  # ggsave(
  #   filename=paste0("analysis/plots/Indivduals/africa_map_", gsub(":","_", selected_mutation), "facet.png"),
  #   plot = africa_map_sample_fill,
  #   width = 12, height = 10, units = "in", dpi = 300)


  print(paste("Save plot for:", selected_mutation))
}

#prep stave objects by aligning coordinates and names
#where survey_clean is the get_survey output of a clean STAVE object
coords_clean <- survey_clean %>%
  select(latitude, longitude, site_name, country_name, study_key) %>%
  distinct() %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
  drop_na(latitude, longitude) %>%
  rename(study_ID = study_key) %>%
  mutate(entry_iso3c = countrycode(country_name, origin = "country.name", destination = "iso3c"))
coords_clean_sf <- st_as_sf(coords_clean, coords = c("longitude", "latitude"), crs = 4326)
clean_admin1 <- st_join(coords_clean_sf, africa_shp_admin1, join = st_within) %>%
  mutate(
    longitude = st_coordinates(.)[, 1],  # Extract longitude
    latitude = st_coordinates(.)[, 2]   # Extract latitude
  ) %>%
  select(study_ID, longitude, latitude, site_name, name_1)

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
  left_join(all_prev_data, by = "name_1") %>%
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


selected_mutation = "k13:561:H"

for (selected_mutation in all_who_mutations){
  #filter all_prev data to selected mutation
  prevalence_data <- all_prev_data %>% filter(mutation == selected_mutation)

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

