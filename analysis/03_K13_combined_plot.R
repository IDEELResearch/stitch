# author: CMS
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
devtools::load_all()

sf::sf_use_s2(FALSE)  # temporarily disable s2

# --- Define Output paths ------------------------------------------------------
manuscript_dir <- "manuscript_fig"
supplement_dir <- "manuscript_fig/supplement_fig"

# -- Load data -----------------------------------------------------------------
prev_raw <- readr::read_csv("analysis/data_derived/all_mutations_get_prevalence.csv", show_col_types = FALSE)
africa_admin0 <- readRDS("analysis/data_derived/sf_admin0_africa.rds")
africa_admin1 <- readRDS("analysis/data_derived/sf_admin1_africa.rds")

# --- Add combined K13 data ----------------------------------------------------
all_who_mutations <- c("k13:446:I", "k13:458:Y", "k13:469:Y", "k13:476:I", "k13:493:H", "k13:539:T",
                       "k13:543:T",  "k13:553:L", "k13:561:H", "k13:574:L", "k13:580:Y", "k13:622:I","k13:675:V",
                       "k13:441:L", "k13:449:A", "k13:469:F", "k13:481:V",
                       "k13:515:K", "k13:527:H", "k13:537:I", "k13:537:D", "k13:538:V",  "k13:568:G")

prev_raw_K13 <- prev_raw %>%
  filter(mutation %in% all_who_mutations)

dat_with_k13 <- add_combined_k13(prev_raw_K13)
dat_k13 <- dat_with_k13 %>%
  filter(mutation == "k13:comb")

# --- Create East Africa Box ---------------------------------------------------
# build bbox for East Africa
bbox_ea <- get_east_africa_bbox(sf::st_crs(africa_admin0))

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

# --- Avg every 2 years --------------------------------------------------------
k13_prev_2_year_grouped <- avg_2_year(dat_k13) |>
  dplyr::mutate(prevalence = bin_prevalence(prevalence, "k13")) |>
  dplyr::filter(denominator > 0)

################################################################################
# Plotting Manuscript Figures
################################################################################

# --- Figure 2A: Africa, year groups, binned colours ----------------------------
africa_binned_prev_plot <- data_binned_year_plot(
  prev_df = k13_prev_2_year_grouped,
  mut = "k13",
  africa_admin0 = africa_admin0,
  shp_non_malaria = shape_non_malaria,
  x_axis_break = 20,
  x_break_lims = c(-25, 45),
  padding_lon_lat = 3,
  size_scale = c(0.1, 8)
  )

save_figs(file.path(manuscript_dir, "Fig2A_africa_map_k13_points_binned"), africa_binned_prev_plot, width = 12, height = 4, res =600)

africa_binned_prev_plot_no_legend <- data_binned_year_plot(
  prev_df = k13_prev_2_year_grouped,
  mut = "k13",
  africa_admin0 = africa_admin0,
  shp_non_malaria = shape_non_malaria,
  legend = FALSE,
  x_axis_break = 20,
  x_break_lims = c(-25, 45),
  padding_lon_lat = 3,
  size_scale = c(0.1, 8)
  )
save_figs(file.path(manuscript_dir, "Fig2A_africa_map_k13_points_binned_no_legend"), africa_binned_prev_plot_no_legend, width = 12, height = 2.5, res =600)

# -- Figure 2B: East Africa inset, year groups, binned colours -------------------
east_africa_k13_prev_2_year_grouped <- k13_prev_2_year_grouped |>
  dplyr::filter(
    longitude >= bbox_ea["xmin"],
    longitude <= bbox_ea["xmax"],
    latitude >= bbox_ea["ymin"],
    latitude <= bbox_ea["ymax"]
  )

east_africa_binned_prev_plot <- data_binned_year_plot(
  prev_df = east_africa_k13_prev_2_year_grouped,
  mut = "k13",
  africa_admin0 = africa_admin0,
  shp_non_malaria = shape_non_malaria,,
  lims = east_africa_lims,
  legend = TRUE,
  crop = TRUE,
  x_axis_break = 5,
  y_axis_break = 5,
  size_scale = c(0.1, 8)
  )
save_figs(file.path(manuscript_dir, "Fig2B_east_africa_inset_map_k13_points_binned"), east_africa_binned_prev_plot, width = 12, height = 6, res =600)

east_africa_binned_prev_plot_no_legend <- data_binned_year_plot(
  prev_df = east_africa_k13_prev_2_year_grouped,
  mut = "k13",
  africa_admin0 = africa_admin0,
  shp_non_malaria = shape_non_malaria,
  lims = east_africa_lims,
  legend = FALSE,
  crop = TRUE,
  x_axis_break = 5,
  y_axis_break = 5,
  size_scale = c(0.1, 8)
  )
save_figs(file.path(manuscript_dir, "Fig2B_east_africa_inset_map_k13_points_binned_no_legend"), east_africa_binned_prev_plot_no_legend, width = 12, height = 3.6, res =600)

################################################################################
# Plotting Supplemental Manuscript Figures
################################################################################
# -- Supplemental Figure 1: Africa, faceted by year, binned colours ------------
africa_all_years_prev_plot_2010_2024 <- data_per_year_plot(
  prev_df = k13_prev_per_year %>% filter(year %in% c(2010:2024)),
  mut = "k13",
  africa_admin0 = africa_admin0,
  shp_non_malaria = shape_non_malaria,
  size_scale = c(0.1, 5),
  x_axis_break = 22,
  y_axis_break = 10,
  padding_lon_lat = 3,
  facet_n_row = 5
  )
save_figs(file.path(supplement_dir, "SFig1_africa_map_k13_points_2010_2024"), africa_all_years_prev_plot_2010_2024, height = 8)

# -- Supplemental Figure 2: East Africa inset, faceted by year, binned colours -------------------
east_africa_all_years_prev_plot_2010_2024 <- data_per_year_plot(
  prev_df = k13_prev_per_year %>% filter(year %in% c(2010:2024)),
  mut = "k13",
  africa_admin0 = africa_admin0,
  shp_non_malaria = shape_non_malaria,
  lims = east_africa_lims,
  size_scale = c(0.1, 8),
  y_axis_break = 5,
  x_axis_break = 8,
  facet_n_row = 5,
  crop = TRUE
  )
save_figs(file.path(supplement_dir, "SFig2_east_africa_map_k13_points_2010_2024"), east_africa_all_years_prev_plot_2010_2024, height = 8)
