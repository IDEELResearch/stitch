# author: CMS and NWY
# description: Plot mdr1 prevalence data across Africa and East Africa

# --- Packages --------------------------------------------------------------------
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

# --- Define output paths ----------------------------------------------------------------
manuscript_dir <- "manuscript_fig"
supplement_dir <- "supplement_fig"

# --- Load data ----------------------------------------------------------------
prev_raw <- readr::read_csv("analysis/data_derived/all_mutations_get_prevalence.csv", show_col_types = FALSE)
africa_admin0 <- readRDS("analysis/data_derived/sf_admin0_africa.rds")
africa_admin1 <- readRDS("analysis/data_derived/sf_admin1_africa.rds")

# --- Grey out non-endemic malaria African countries ---------------------------
non_malaria_countries <- c(
  "Egypt", "Morocco", "Libya", "Tunisia", "Algeria",
  "Cabo Verde", "Lesotho", "Mauritius", "Seychelles"
)

shape_non_malaria <- africa_admin0 |>
  dplyr::filter(name_0 %in% non_malaria_countries)

# --- Mutation sets ------------------------------------------------------------
pd_mutations <- c("mdr1:86:Y", "crt:76:T")

# --- Supplemental Figure unk: Africa, faceted by year, gradient colour --------
for (mut in pd_mutations) {
  message("Processing ", mut, "...")
  if (mut == "mdr1:86:Y"){
    fig_suffix = paste0("SFig19_", mut)
  } else if (mut == "crt:76:T"){
    fig_suffix = paste0("SFig18_", mut)
  }
  print(paste0("fig_suffix: ", fig_suffix))

  gene = stringr::str_split(mut, ":")[[1]][1]

  # Filter data for mutation
  prev_data_mut <- prev_raw |>
    dplyr::filter(mutation == mut)

  # --- Plot avg 2year prevalence data --------------------------------------------
  # Bin prevalence and obtain 2-avg prevalence data
  prev_data_mut_bin <- prev_data_mut |>
    add_year_group_pd(year) |>
    filter(!is.na(year_group),
           denominator > 0) |>
    mutate(
      prevalence = bin_prevalence(prevalence, gene),
      prevalence = factor(prevalence, levels = PREV_LEVELS(gene))
    ) |>
    arrange(prevalence)

  mut_binned_prev_plot <- data_binned_year_plot(prev_data_mut_bin,
                                                mut = gene,
                                                africa_admin0 = africa_admin0,
                                                shp_non_malaria = shape_non_malaria,
                                                size_scale = c(0.1,10),
                                                x_axis_break = 20,
                                                facet_n_row = 4,
                                                sample_size_legend = "right",
                                                prev_legend = "right")
  save_figs(file.path(manuscript_dir, supplement_dir, paste0(fig_suffix, "_africa_map_prev_binned_years")), mut_binned_prev_plot)
  message("Saved plot for: ", mut, " -> ", file.path(manuscript_dir, supplement_dir, "SFigX", paste0(mut, "_africa_map_prev_binned_years")))

  # prev_data_mut_per_year <- prev_data_mut |>
  #   mutate(
  #     prevalence = bin_prevalence(prevalence, gene),
  #     prevalence = factor(prevalence, levels = PREV_LEVELS(gene))
  #   ) |>
  #   arrange(prevalence)
  #
  # # Plot annual prev for all available years
  # mut_per_years_prev_plot <- data_per_year_plot(
  #   prev_df = prev_data_mut_per_year %>% filter(year >= 2001),
  #   mut = gene,
  #   africa_admin0 = africa_admin0,
  #   shp_non_malaria = shape_non_malaria,
  #   size_scale = c(0.1,10),
  #   x_axis_break = 25,
  #   facet_n_row = 6)
  #
  # save_figs(file.path(manuscript_dir, supplement_dir, paste0(fig_suffix, "_africa_map_prev_per_years")), mut_per_years_prev_plot)
  # message("Saved plot for: ", mut, " -> ", file.path(supplement_dir, "K13_individual_prev_data", "all_years", paste0(mut, "_africa_map_prev_binned_years")))

}
