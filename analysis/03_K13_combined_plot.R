# author: CMS and NWY
# description: Plot K13 prevalence data across Africa and East Africa

# ── Packages ────────────────────────────────────────────────────────────────────
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

# ── I/O paths ───────────────────────────────────────────────────────────────────
in_csv   <- "analysis/data_derived/all_who_get_prevalence_africa.csv"
rds_admin0 = "analysis/data_derived/sf_admin0_africa.rds"
rds_admin1 = "analysis/data_derived/sf_admin1_africa.rds"
out_plot_dir <- "K13_overall_plots"
manuscript_dir <- "manuscript_fig"
supplement_dir <- "manuscript_fig/supplement_fig"

# Prevalence binning (levels fixed for legend order)
PREV_LEVELS <- c("0", "0-1", "1-5", "5-10", "10-20", "20-30", "30-40", "40+")

# ── Load data ───────────────────────────────────────────────────────────────────
prev_raw <- readr::read_csv(in_csv, show_col_types = FALSE)
africa_admin0 <- readRDS(rds_admin0)
africa_admin1 <- readRDS(rds_admin1)

# ── Aggregate site-level prevalence ─────────────────────────────────────────────
k13_site <- prev_raw %>%
  group_by(latitude, longitude, study_name, country_name,
           site_name, collection_day, year, denominator) %>%
  summarise(k13_prevalence = sum(prevalence, na.rm = TRUE), .groups = "drop")

# Bin prevalences by years
k13_grouped <- k13_site %>%
  add_year_group(year) %>%
  filter(!is.na(year_group)) %>%
  mutate(
    prevalence_bin = bin_prevalence(k13_prevalence),
    prevalence_bin = factor(prevalence_bin, levels = PREV_LEVELS())
  ) %>%
  arrange(k13_prevalence)

# ── Figure 1: Africa, year groups, binned colours ──────────────────────────────
africa_binned_prev_plot <- ggplot() +
  facet_wrap(~year_group, nrow = 2) +
  geom_sf(data = africa_admin0, fill = NA, colour = "black",
          show.legend = FALSE, linewidth = 0.1) +
  geom_point(
    data = k13_grouped,
    aes(x = longitude, y = latitude,
        size = denominator,
        colour = prevalence_bin),
    alpha = 0.8
  ) +
  scale_color_manual(
    name   = "Prevalence (%)",
    values = prev_bin_colors(),
    limits = PREV_LEVELS(),
    drop   = FALSE
  ) +
  scale_size_continuous(
    name   = "Sample Size (N)",
    range  = c(0.2, 5),
    limits = c(min(k13_grouped$denominator, na.rm = TRUE),
               max(k13_grouped$denominator, na.rm = TRUE)),
    breaks = pretty(k13_grouped$denominator, n = 5)
  ) +
  guides(
    size   = guide_legend(order = 1,   # ← sample size first
                          direction = "horizontal",
                          title.position = "top"),
    colour = guide_legend(order = 2,   # ← prevalence second
                          nrow = 2, byrow = TRUE,
                          title.position = "top")
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.box      = "vertical",
    legend.title    = element_text(size = 9),
    legend.text     = element_text(size = 8),
    plot.background = element_rect(fill = "white", colour = "white"),
    strip.text      = element_text(size = 9)
  )

save_figs(file.path(out_plot_dir, "africa_map_k13_points_binned"), africa_binned_prev_plot)
save_figs(file.path(manuscript_dir, "Fig1A_africa_map_k13_points_all_years"), africa_binned_prev_plot)

# ── Figure 2: East Africa inset, year groups, binned colours ───────────────────
# build bbox for East Africa
bbox_ea <- sf::st_bbox(
  c(xmin = 28.48, xmax = 44.5, ymin = -4.60, ymax = 16.00),
  crs = sf::st_crs(africa_admin0)
)

# crop background with the same bbox
bbox_ea_sf <- sf::st_as_sfc(bbox_ea)   # convert bbox to polygon
africa_admin0_ea <- sf::st_intersection(
  sf::st_make_valid(africa_admin0),
  bbox_ea_sf
)
africa_admin0_ea <- africa_admin0 %>% sf::st_make_valid()

# pull x/y limits from the bbox
lims <- sf::st_bbox(bbox_ea)  # named vector: xmin xmax ymin ymax

east_africa_binned_prev_plot <- ggplot() +
  facet_wrap(~year_group, nrow = 2) +
  geom_sf(data = africa_admin0_ea, fill = NA, colour = "black",
          show.legend = FALSE, linewidth = 0.1) +
  geom_point(
    data = k13_grouped,
    aes(x = longitude, y = latitude,
        size = denominator,
        colour = prevalence_bin),
    alpha = 0.8,
    show.legend = TRUE
  ) +
  scale_color_manual(
    values = prev_bin_colors(),
    limits = PREV_LEVELS(),
    drop   = FALSE,
    guide  = "none"
  ) +
  scale_size_continuous(
    name  = "Sample Size (N)",
    range = c(0.2, 10)
  ) +
  coord_sf(
    xlim = c(lims["xmin"], lims["xmax"]),
    ylim = c(lims["ymin"], lims["ymax"]),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",      # <- put legend at bottom
    legend.text     = element_text(size = 8),
    legend.title    = element_text(size = 9),
    plot.background = element_rect(fill = "white", colour = "white"),
    strip.text      = element_text(size = 9, margin = margin(b = 10))
  )

save_figs(file.path(out_plot_dir, "EA_inset_map_k13_points_binned"), east_africa_binned_prev_plot)
save_figs(file.path(manuscript_dir, "Fig1B_EA_inset_map_k13_points_binned"), east_africa_binned_prev_plot)

# ── Supplemental Figure 1: Africa, faceted by year, gradient colour ─────────────────────────
africa_all_years_prev_plot <- ggplot() +
  facet_wrap(~year) +
  geom_sf(data = africa_admin0, fill = NA, colour = "black",
          show.legend = FALSE, linewidth = 0.1) +
  geom_point(
    data = filter(k13_site, k13_prevalence == 0),
    aes(x = longitude, y = latitude, size = denominator),
    colour = "grey70", alpha = 0.8
  ) +
  geom_point(
    data = k13_site %>% filter(k13_prevalence > 0) %>% arrange(k13_prevalence),
    aes(x = longitude, y = latitude, colour = k13_prevalence, size = denominator),
    alpha = 0.8
  ) +
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
    plot.background = element_rect(fill = "white", colour = "white"),
    strip.text = element_text(size = 9)
  )

save_figs(file.path(out_plot_dir, "africa_map_k13_points_all_years"), africa_all_years_prev_plot)
save_figs(file.path(supplement_dir, "SFig1_africa_map_k13_points_all_years"), africa_all_years_prev_plot)

# ── Supplemental Figure 2: East Africa inset, year groups, binned colours ───────────────────
# Get first year with any samples inside the bbox
first_year_in_box <- k13_site %>%
  dplyr::filter(
    dplyr::between(longitude, lims["xmin"], lims["xmax"]),
    dplyr::between(latitude,  lims["ymin"], lims["ymax"])
  ) %>%
  dplyr::summarise(first_year = min(year, na.rm = TRUE)) %>%
  dplyr::pull(first_year)

# Keep only points inside the bbox and from that year onward
k13_site_inbox <- k13_site %>%
  dplyr::filter(
    dplyr::between(longitude, lims["xmin"], lims["xmax"]),
    dplyr::between(latitude,  lims["ymin"], lims["ymax"]),
    year >= first_year_in_box
  )

east_africa_all_years_prev_plot <- ggplot() +
  facet_wrap(~year) +
  geom_sf(data = africa_admin0, fill = NA, colour = "black",
          show.legend = FALSE, linewidth = 0.1) +   # full layer is fine
  geom_point(
    data = dplyr::filter(k13_site_inbox, k13_prevalence == 0),
    aes(x = longitude, y = latitude, size = denominator),
    colour = "grey70", alpha = 0.8
  ) +
  geom_point(
    data = k13_site_inbox %>% dplyr::filter(k13_prevalence > 0) %>% dplyr::arrange(k13_prevalence),
    aes(x = longitude, y = latitude, colour = k13_prevalence, size = denominator),
    alpha = 0.8
  ) +
  scale_color_gradientn(name = "Prevalence (%)", colours = prevalence_palette(100), na.value = "grey85") +
  scale_size_continuous(name = "Sample Size (N)", range = c(0.2, 3)) +
  coord_sf(
    xlim = c(lims["xmin"], lims["xmax"]),
    ylim = c(lims["ymin"], lims["ymax"]),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.text  = element_text(size = 8),
    legend.title = element_text(size = 9),
    plot.background = element_rect(fill = "white", colour = "white"),
    strip.text = element_text(size = 9, margin = margin(b = 10))
  )

save_figs(file.path(out_plot_dir, "EA_inset_map_k13_points_all_years"), east_africa_all_years_prev_plot)
save_figs(file.path(supplement_dir, "SFig2_EA_inset_map_k13_points_all_years"), east_africa_all_years_prev_plot)
