# author: CMS and NWY
# description: Plot individual mutation prevalence grid for 2-year averages across Africa

# --- Packages --------------------------------------------------------------------
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

# --- Define output paths -------------------------------------------------------------------
manuscript_dir <- "manuscript_fig"
supplement_dir <- "manuscript_fig/supplement_fig"
stave_obj <- "analysis/data_raw/stave_final_data.rds"

# --- Load data -------------------------------------------------------------------
prev_raw <- readr::read_csv("analysis/data_derived/all_who_get_prevalence.csv", show_col_types = FALSE)
africa_admin0 <- readRDS("analysis/data_derived/sf_admin0_africa.rds")
africa_admin1 <- readRDS("analysis/data_derived/sf_admin1_africa.rds")

# --- Geometry: centroids for admin1 (optional) -----------------------------------
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

# --- Grey out non-endemic malaria African countries ---------------------------
non_malaria_countries <- c(
  "Egypt", "Morocco", "Libya", "Tunisia", "Algeria",
  "Cabo Verde", "Lesotho", "Mauritius", "Seychelles"
)

shape_non_malaria <- africa_admin0 |>
  dplyr::filter(name_0 %in% non_malaria_countries)

# --- Mutation sets ---------------------------------------------------------------
all_who_mutations <- c(
  "k13:446:I","k13:458:Y","k13:469:Y","k13:476:I","k13:493:H","k13:539:T",
  "k13:543:T","k13:553:L","k13:561:H","k13:574:L","k13:580:Y","k13:622:I","k13:675:V",
  "k13:441:L","k13:449:A","k13:469:F","k13:481:V",
  "k13:515:K","k13:527:H","k13:537:I","k13:537:D","k13:538:V","k13:568:G"
)

# --- Plot loop per mutation ------------------------------------------------------
for (mut in all_who_mutations) {
  message("Processing ", mut, "...")
  # Filter data for mutation
  prev_data_mut <- prev_raw |>
    dplyr::filter(mutation == mut)
    # dplyr::select(-longitude, -latitude) |>
    # dplyr::left_join(clean_admin1, by = dplyr::join_by(study_id == study_ID, site_name == site_name)) |>
    # tidyr::drop_na(longitude, latitude) |>
    # dplyr::mutate(year = extract_year(collection_day))

  # Skip if no positive prevalence found
  if (!any(prev_data_mut$prevalence > 0, na.rm = TRUE)) {
    message("Skipping ", mut, " (all zero prevalence).")
    next
  }

  # --- Plot avg 2year prevalence data --------------------------------------------
  # Bin prevalence and obtain 2-avg prevalence data
  prev_data_mut_bin <- prev_data_mut |>
    add_year_group(year) |>
    filter(!is.na(year_group),
           denominator > 0) |>
    mutate(
      prevalence = bin_prevalence(prevalence, "k13"),
      prevalence = factor(prevalence, levels = PREV_LEVELS("k13"))
    ) |>
    arrange(prevalence)

  mut_binned_prev_plot <- data_binned_year_plot(
    prev_df = prev_data_mut_bin,
    mut = "k13",
    africa_admin0 = africa_admin0,
    shp_non_malaria = shape_non_malaria,
    size_scale = c(0.1,10),
    x_axis_break = 20,
    facet_n_row = 2)
  save_figs(file.path(supplement_dir, "K13_individual_prev_data", "binned_years", paste0(mut, "_africa_map_prev_binned_years")), mut_binned_prev_plot)
  message("Saved plot for: ", mut, " -> ", file.path(supplement_dir, "K13_individual_prev_data", "binned_years", paste0(mut, "_africa_map_prev_binned_years")))

  # --- Plot annual prevalence data --------------------------------------------
  # Bin prevalence column
  prev_data_mut_per_year <- prev_data_mut |>
    mutate(
      prevalence = bin_prevalence(prevalence, "k13"),
      prevalence = factor(prevalence, levels = PREV_LEVELS("k13"))
    ) |>
    arrange(prevalence)

  # Plot annual prev for all available years
  mut_per_years_prev_plot <- data_per_year_plot(
    prev_df = prev_data_mut_per_year,
    mut = "k13",
    africa_admin0 = africa_admin0,
    shp_non_malaria = shape_non_malaria,
    size_scale = c(0.1,2),
    x_axis_break = 25,
    facet_n_row = 6)
  save_figs(file.path(supplement_dir, "K13_individual_prev_data", "all_years", paste0(mut, "_africa_map_prev_binned_years")), mut_per_years_prev_plot)
  message("Saved plot for: ", mut, " -> ", file.path(supplement_dir, "K13_individual_prev_data", "all_years", paste0(mut, "_africa_map_prev_binned_years")))

  # Plot annual prev for 2010-2024
  mut_per_years_prev_plot_2010_2024 <- data_per_year_plot(
    prev_df = prev_data_mut_per_year %>% filter(year %in% c(2010:2024)),
    mut = "k13",
    africa_admin0 = africa_admin0,
    shp_non_malaria = shape_non_malaria,
    size_scale = c(0.1, 5),
    x_axis_break = 22,
    y_axis_break = 10,
    facet_n_row = 5)
  save_figs(file.path(supplement_dir, "K13_individual_prev_data", "all_years", paste0(mut, "_africa_map_prev_binned_years_2010_2014")), mut_per_years_prev_plot_2010_2024)
  message("Saved plot for: ", mut, " -> ", file.path(supplement_dir, "K13_individual_prev_data", "all_years", paste0(mut, "_africa_map_prev_binned_years_2010_2014")))

}
