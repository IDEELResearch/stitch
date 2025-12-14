# author: CMS and NWY
# description: Plot K13 prevalence data across Africa and East Africa per year and 2-year averages

# -- Packages --------------------------------------------------------------------
suppressPackageStartupMessages({
  library(sf)
  library(grid)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(usethis)
  library(here)
  library(devtools)
  library(countrycode)
  library(scales)
})

# Load all functions in R
load_all()

sf::sf_use_s2(FALSE)  # temporarily disable s2

# --- Define Output paths ------------------------------------------------------
manuscript_dir <- "manuscript_fig"
supplement_dir <- "manuscript_fig/supplement_fig"

# Prevalence binning (levels fixed for legend order)
PREV_LEVELS <- c("0", "0-1", "1-5", "5-10", "10-20", "20-30", "30-40", "40+")

# -- Load data -----------------------------------------------------------------
prev_raw <- readr::read_csv("analysis/data_derived/all_who_get_prevalence.csv", show_col_types = FALSE)
africa_admin0 <- readRDS("analysis/data_derived/sf_admin0_africa.rds")
africa_admin1 <- readRDS("analysis/data_derived/sf_admin1_africa.rds")

# --- Aggregate site-level prevalence ------------------------------------------
k13_site <- prev_raw |>
  group_by(latitude, longitude, study_id, country_name,
           site_name, collection_day, year, denominator) |>
  summarise(k13_prevalence = sum(prevalence, na.rm = TRUE), .groups = "drop") |>
  filter(denominator > 0)

# Bin prevalences by years
k13_prev_2_year_grouped <- k13_site |>
  add_year_group(year) |>
  filter(!is.na(year_group)) |>
  mutate(
    prevalence = bin_prevalence(k13_prevalence),
    prevalence = factor(prevalence, levels = PREV_LEVELS())
  ) |>
  arrange(k13_prevalence)

# --- Create East Africa Box ---------------------------------------------------
# build bbox for East Africa
bbox_ea <- sf::st_bbox(
  c(xmin = 28.48, xmax = 44.5, ymin = -4.60, ymax = 16.00),
  crs = sf::st_crs(africa_admin0)
)

# crop background with the same bbox
bbox_east_africa_sf <- sf::st_as_sfc(bbox_ea)   # convert bbox to polygon
africa_admin0_ea <- sf::st_intersection(
  sf::st_make_valid(africa_admin0),
  bbox_east_africa_sf
)
africa_admin0_ea <- africa_admin0 |> sf::st_make_valid()

# pull x/y limits from the bbox
east_africa_lims <- sf::st_bbox(bbox_ea)  # named vector: xmin xmax ymin ymax

# --- Grey out non-endemic malaria African countries ---------------------------
non_malaria_countries <- c(
  "Egypt", "Morocco", "Libya", "Tunisia", "Algeria",
  "Cabo Verde", "Lesotho", "Mauritius", "Seychelles"
)

shape_non_malaria <- africa_admin0 %>%
  dplyr::filter(name_0 %in% non_malaria_countries)

################################################################################
# Plotting Manuscript Figures
################################################################################

# --- Figure 1: Africa, year groups, binned colours ----------------------------
africa_binned_prev_plot <- data_binned_year_plot(k13_prev_2_year_grouped, africa_admin0, shape_non_malaria, x_axis_break = 15)
save_figs(file.path(manuscript_dir, "Fig2A_africa_map_k13_points_binned"), africa_binned_prev_plot, width = 12, res =600)

africa_binned_prev_plot_no_legend <- data_binned_year_plot(k13_prev_2_year_grouped, africa_admin0, shape_non_malaria, legend = FALSE, x_axis_break = 15)
save_figs(file.path(manuscript_dir, "Fig2A_africa_map_k13_points_binned_no_legend"), africa_binned_prev_plot_no_legend, width = 12, res =600)

# -- Figure 2: East Africa inset, year groups, binned colours -------------------
east_africa_k13_prev_2_year_grouped <- k13_prev_2_year_grouped |>
  dplyr::filter(
    longitude >= bbox_ea["xmin"],
    longitude <= bbox_ea["xmax"],
    latitude >= bbox_ea["ymin"],
    latitude <= bbox_ea["ymax"]
  )

east_africa_binned_prev_plot <- data_binned_year_plot(east_africa_k13_prev_2_year_grouped, africa_admin0_ea, shape_non_malaria, east_africa_lims, legend = TRUE, east_africa = TRUE, x_axis_break = 5, y_axis_break = 5)
save_figs(file.path(manuscript_dir, "Fig2B_east_africa_inset_map_k13_points_binned"), east_africa_binned_prev_plot, width = 12, res =600)

east_africa_binned_prev_plot_no_legend <- data_binned_year_plot(east_africa_k13_prev_2_year_grouped, africa_admin0_ea, shape_non_malaria, east_africa_lims, legend = FALSE, east_africa = TRUE, x_axis_break = 5, y_axis_break = 5)
save_figs(file.path(manuscript_dir, "Fig2B_east_africa_inset_map_k13_points_binned_no_legend"), east_africa_binned_prev_plot_no_legend, width = 12, res =600)

################################################################################
# Plotting Supplemental Manuscript Figures
################################################################################
# -- Supplemental Figure 1: Africa, faceted by year, binned colours ------------
k13_prev_per_year <- prev_raw |>
  group_by(latitude, longitude, study_id, country_name,
           site_name, collection_day, year, denominator) |>
  summarise(k13_prevalence = sum(prevalence, na.rm = TRUE), .groups = "drop") |>
  mutate(
    prevalence = bin_prevalence(k13_prevalence),
    prevalence = factor(prevalence, levels = PREV_LEVELS())
  ) |>
  arrange(k13_prevalence)

africa_all_years_prev_plot <- data_per_year_plot(k13_prev_per_year, africa_admin0, shape_non_malaria, size_scale = c(0.2, 4), x_axis_break = 30, y_axis_break = 20, facet_n_row = 6)
save_figs(file.path(supplement_dir, "SFig1_africa_map_k13_points_all_years"), africa_all_years_prev_plot)

africa_all_years_prev_plot_2010_2024 <- data_per_year_plot(k13_prev_per_year %>% filter(year %in% c(2010:2024)), africa_admin0, shape_non_malaria, size_scale = c(0.5, 5), x_axis_break = 22, y_axis_break = 10, facet_n_row = 5)
save_figs(file.path(supplement_dir, "SFig1_africa_map_k13_points_2010_2024"), africa_all_years_prev_plot_2010_2024)

# -- Supplemental Figure 2: East Africa inset, faceted by year, binned colours -------------------
east_africa_k13_prev_per_year<- k13_prev_per_year |>
  dplyr::filter(
    longitude >= bbox_ea["xmin"],
    longitude <= bbox_ea["xmax"],
    latitude >= bbox_ea["ymin"],
    latitude <= bbox_ea["ymax"]
  )

east_africa_all_years_prev_plot <- data_per_year_plot(k13_prev_per_year, africa_admin0, shape_non_malaria, east_africa_lims, size_scale = c(0.2, 4), y_axis_break = 5, facet_n_row = 6, crop = TRUE)
save_figs(file.path(supplement_dir, "SFig2_east_africa_map_k13_points_all_years"), east_africa_all_years_prev_plot)

east_africa_all_years_prev_plot_2010_2024 <- data_per_year_plot(k13_prev_per_year %>% filter(year %in% c(2010:2024)), africa_admin0, shape_non_malaria, east_africa_lims, size_scale = c(0.5, 7), y_axis_break = 5, x_axis_break = 8, facet_n_row = 5, crop = TRUE)
save_figs(file.path(supplement_dir, "SFig2_east_africa_map_k13_points_2010_2024"), east_africa_all_years_prev_plot_2010_2024)
