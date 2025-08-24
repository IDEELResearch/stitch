# author: CMS and NWY
# description: plot individual mutation prevalence map

# ── Packages ────────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(countrycode)
  library(STAVE)
})

# Load all functions in R
load_all()

# ── I/O paths ───────────────────────────────────────────────────────────────────
in_csv   <- "analysis/data_derived/all_who_get_prevalence_africa.csv"
rds_admin0 = "analysis/data_derived/sf_admin0_africa.rds"
rds_admin1 = "analysis/data_derived/sf_admin1_africa.rds"
out_plot_dir <- "K13_individual_mutations"
manuscript_dir <- "manuscript_fig"
supplement_dir <- "manuscript_fig/supplement_fig"
stave_obj <- "analysis/data_raw/stave_final_data.rds"

# ── Load data ───────────────────────────────────────────────────────────────────
prev_raw <- readr::read_csv(in_csv, show_col_types = FALSE)
africa_admin0 <- readRDS(rds_admin0)
africa_admin1 <- readRDS(rds_admin1)
stave <- readRDS(stave_obj)

# ── Fetch data from stave ───────────────────────────────────────────────────────
survey_clean  <- stave$get_surveys()
counts        <- stave$get_counts()
studies_clean <- stave$get_studies()

# ── Geometry: centroids for admin1 (optional) ───────────────────────────────────
# Make s2 toggle local and restored afterwards
.old_s2 <- sf::sf_use_s2()
on.exit(sf::sf_use_s2(.old_s2), add = TRUE)
sf::sf_use_s2(FALSE)

# Calculate centroids of each polygon
africa_admin1_longlat <- africa_admin1 |>
  sf::st_geometry() |>
  sf::st_centroid() |>
  sf::st_coordinates() |>
  cbind(africa_admin1) |>
  dplyr::rename(lon = X, lat = Y)

# ── Clean survey coords & join to admin1 ────────────────────────────────────────
coords_clean <- survey_clean %>%
  dplyr::select(latitude, longitude, site_name, country_name, study_key) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    latitude  = as.numeric(latitude),
    longitude = as.numeric(longitude),
    entry_iso3c = countrycode::countrycode(country_name, origin = "country.name", destination = "iso3c")
  ) %>%
  dplyr::rename(study_ID = study_key)

coords_clean_sf <- sf::st_as_sf(coords_clean, coords = c("longitude", "latitude"), crs = 4326)

clean_admin1 <- sf::st_join(coords_clean_sf, africa_admin1, join = sf::st_within) %>%
  dplyr::mutate(
    longitude = sf::st_coordinates(.)[, 1],
    latitude  = sf::st_coordinates(.)[, 2]
  ) %>%
  dplyr::select(study_ID, longitude, latitude, site_name, name_1)

# ── Mutation sets ───────────────────────────────────────────────────────────────
validated_mutations <- c(
  "k13:446:I","k13:458:Y","k13:469:Y","k13:476:I","k13:493:H","k13:539:T",
  "k13:543:T","k13:553:L","k13:561:H","k13:574:L","k13:580:Y","k13:622:I","k13:675:V"
)
all_who_mutations <- c(
  validated_mutations,
  "k13:441:L","k13:449:A","k13:469:F","k13:481:V",
  "k13:515:K","k13:527:H","k13:537:I","k13:537:D","k13:538:V","k13:568:G"
)
candidate_mutations <- setdiff(all_who_mutations, validated_mutations)

# ── Plot loop per mutation ──────────────────────────────────────────────────────
# Extract year from date
extract_year <- function(x) {
  y <- suppressWarnings(as.integer(substr(x, 1, 4)))
  y
}

for (selected_mutation in all_who_mutations) {
  # Filter data for mutation
  prev_data_mut <- prev_raw %>%
    dplyr::filter(mutation == selected_mutation) %>%
    dplyr::select(-longitude, -latitude) %>%
    dplyr::left_join(clean_admin1, by = dplyr::join_by(study_id == study_ID, site_name == site_name)) %>%
    tidyr::drop_na(longitude, latitude) %>%
    dplyr::mutate(year = extract_year(collection_day))

  # Skip if no positive prevalence found
  if (!any(prev_data_mut$prevalence > 0, na.rm = TRUE)) {
    message("Skipping ", selected_mutation, " (all zero prevalence).")
    next
  }

  # Bin by year ranges (3-year bins) computed from first/last non-zero prevalence
  prev_data_mut_bin <- bin_years(prev_data_mut, bin_size = 3)

  # Bin prevalence to categories for discrete colour mapping
  prev_data_mut_bin <- prev_data_mut_bin %>%
    dplyr::mutate(
      prevalence = as.numeric(prevalence),
      prevalence_bin = bin_prevalence(prevalence),
      prevalence_bin = factor(prevalence_bin,
                              levels = PREV_LEVELS(),
                              ordered = TRUE)
    )

  # Build discrete colour map
  colors_map <- prev_bin_colors()

  # Plot all years
  all_years <- ggplot() +
    geom_sf(data = africa_admin0, fill = NA, colour = "black",
            show.legend = FALSE, linewidth = 0.05) +
    geom_point(
      data = prev_data_mut %>% filter(prevalence == 0),
      aes(x = longitude, y = latitude, size = denominator),
      colour = "grey70", alpha = 0.8
    ) +
    geom_point(
      data = prev_data_mut %>% filter(prevalence > 0) %>% arrange(prevalence),
      aes(x = longitude, y = latitude, colour = prevalence, size = denominator),
      alpha = 0.8
    ) +
    facet_wrap(~year) +
    scale_color_gradientn(
      name = "Prevalence (%)",
      colours = prevalence_palette(100),
      na.value = "grey85"
    ) +
    scale_size_continuous(name = "Sample Size (N)", range = c(0.2, 4)) +
    theme_void() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      plot.background = element_rect(fill = "white", colour = "white")
    )

  # Save
  out_file <- file.path(
    out_plot_dir,
    paste0("all_years/", selected_mutation, "_africa_map_prev_all_years")
  )
  save_figs(out_file, all_years)
  message("Saved plot for: ", selected_mutation, " -> ", out_file)

  # Plot binned years
  binned_years <- ggplot() +
    geom_sf(data = africa_admin0, fill = NA, colour = "black",
            show.legend = FALSE, linewidth = 0.05) +
    geom_point(
      data = prev_data_mut_bin,
      aes(x = longitude, y = latitude, size = denominator, colour = prevalence_bin),
      alpha = 0.8
    ) +
    facet_wrap(~year_group) +
    scale_color_manual(
      name    = "Prevalence (%)",
      values  = colors_map,               # full set of colors
      limits  = PREV_LEVELS(),         # <- force all levels
      breaks  = PREV_LEVELS(),         # <- order and show even unused
      drop    = FALSE,
      na.translate = FALSE
    ) +
    scale_size_continuous(name = "Sample Size (N)", range = c(0.2, 4)) +
    theme_void() +
    theme(
      legend.position = "right",
      legend.text  = element_text(size = 8),
      legend.title = element_text(size = 9),
      plot.background = element_rect(fill = "white", colour = "white")
    )

  # Save
  out_file <- file.path(
    out_plot_dir,
    paste0("binned_years/", selected_mutation, "_africa_map_prev_binned_years")
  )
  save_figs(out_file, binned_years)
  message("Saved plot for: ", selected_mutation, " -> ", out_file)
}
