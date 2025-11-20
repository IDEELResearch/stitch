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
  facet_wrap(~year_group, nrow = 1) +
  geom_sf(data = africa_admin0, fill = NA, colour = "black",
          show.legend = FALSE, linewidth = 0.1) +
  geom_point(
    data = k13_grouped,
    aes(x = longitude, y = latitude,
        size = denominator,
        fill = prevalence_bin), # Use 'fill' for the colour scale with shape 21
    shape = 21,                   # Use shape 21 for points with fill and border
    colour = "dimgrey",          # Set the border colour to black
    stroke = 0.4,                 # Set the thickness of the border
    alpha = 0.5                   # Set the opacity to 50%
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  scale_fill_manual(
    name   = "Prevalence (%)",
    values = prev_bin_colors(),
    limits = PREV_LEVELS(),
    drop   = FALSE
  ) +
  scale_size_continuous(
    name   = "Sample Size (N)",
    range  = c(1, 10),
    limits = c(min(k13_grouped$denominator, na.rm = TRUE),
               max(k13_grouped$denominator, na.rm = TRUE)),
    breaks = pretty(k13_grouped$denominator, n = 5),
    guide = guide_legend(position = "right")
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title    = element_text(size = 9),
    legend.text     = element_text(size = 8),
    strip.background = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    strip.text      = element_text(size = 9, margin = margin(b = 10)),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
  )

save_figs(file.path(out_plot_dir, "binned_years/africa_map_k13_points_binned"), africa_binned_prev_plot, width = 12, res =600)
save_figs(file.path(manuscript_dir, "Fig2A_africa_map_k13_points_binned"), africa_binned_prev_plot, width = 12, res =600)

africa_binned_prev_plot_sample_size_only <- ggplot() +
  facet_wrap(~year_group, nrow = 1) +
  geom_sf(data = africa_admin0, fill = NA, colour = "black",
          show.legend = FALSE, linewidth = 0.1) +
  geom_point(
    data = k13_grouped,
    aes(x = longitude, y = latitude,
        size = denominator,
        fill = prevalence_bin), # Use 'fill' for the colour scale with shape 21
    shape = 21,                   # Use shape 21 for points with fill and border
    colour = "dimgrey",          # Set the border colour to black
    stroke = 0.4,                 # Set the thickness of the border
    alpha = 0.5                   # Set the opacity to 50%
  ) +
  scale_fill_manual(
    name   = "Prevalence (%)",
    values = prev_bin_colors(),
    limits = PREV_LEVELS(),
    drop   = FALSE,
    guide = "none"
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  scale_size_continuous(
    name   = "Sample Size (N)",
    range  = c(1, 10),
    limits = c(min(k13_grouped$denominator, na.rm = TRUE),
               max(k13_grouped$denominator, na.rm = TRUE)),
    breaks = pretty(k13_grouped$denominator, n = 5),
    guide = guide_legend(position = "right")
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.title    = element_text(size = 9),
    legend.text     = element_text(size = 8),
    strip.background = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    strip.text      = element_text(size = 9, margin = margin(b = 10)),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
  )

save_figs(file.path(out_plot_dir, "binned_years/africa_map_k13_points_sampleSize_legend"), africa_binned_prev_plot_sample_size_only, width = 12, res =600)
save_figs(file.path(manuscript_dir, "Fig2A_africa_map_k13_points_binned_sampleSize_legend"), africa_binned_prev_plot_sample_size_only, width = 12, res =600)

africa_binned_prev_plot_no_legend <- ggplot() +
  facet_wrap(~year_group, nrow = 1) +
  geom_sf(data = africa_admin0, fill = NA, colour = "black",
          show.legend = FALSE, linewidth = 0.1) +
  geom_point(
    data = k13_grouped,
    aes(x = longitude, y = latitude,
        size = denominator,
        fill = prevalence_bin), # Use 'fill' for the colour scale with shape 21
    shape = 21,                   # Use shape 21 for points with fill and border
    colour = "dimgrey",          # Set the border colour to black
    stroke = 0.4,                 # Set the thickness of the border
    alpha = 0.5                   # Set the opacity to 50%
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  scale_fill_manual(
    name   = "Prevalence (%)",
    values = prev_bin_colors(),
    limits = PREV_LEVELS(),
    drop   = FALSE,
    guide = "none"
  ) +
  scale_size_continuous(
    name   = "Sample Size (N)",
    range  = c(1, 10),
    limits = c(min(k13_grouped$denominator, na.rm = TRUE),
               max(k13_grouped$denominator, na.rm = TRUE)),
    breaks = pretty(k13_grouped$denominator, n = 5),
    guide = "none"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.title    = element_text(size = 9),
    legend.text     = element_text(size = 8),
    strip.background = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    strip.text      = element_text(size = 9, margin = margin(b = 10)),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
  )

save_figs(file.path(out_plot_dir, "binned_years/africa_map_k13_points_no_legend"), africa_binned_prev_plot_no_legend, width = 12, res =600)
save_figs(file.path(manuscript_dir, "Fig2A_africa_map_k13_points_binned_no_legend"), africa_binned_prev_plot_no_legend, width = 12, res =600)

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

ea_k13_grouped <- k13_grouped %>%
  dplyr::filter(
    longitude >= bbox_ea["xmin"],
    longitude <= bbox_ea["xmax"],
    latitude >= bbox_ea["ymin"],
    latitude <= bbox_ea["ymax"]
  )

east_africa_binned_prev_plot <- ggplot() +
  facet_wrap(~year_group, nrow = 1) +
  geom_sf(data = africa_admin0_ea, fill = NA, colour = "black",
          show.legend = FALSE, linewidth = 0.1) +
  geom_point(
    data = ea_k13_grouped,
    aes(x = longitude, y = latitude,
        size = denominator,
        fill = prevalence_bin), # Use 'fill' for the colour scale with shape 21
    shape = 21,                   # Use shape 21 for points with fill and border
    colour = "dimgrey",          # Set the border colour to black
    stroke = 0.4,                 # Set the thickness of the border
    alpha = 0.5                   # Set the opacity to 50%
  ) +
  scale_fill_manual(
    name   = "Prevalence (%)",
    values = prev_bin_colors(),
    limits = PREV_LEVELS(),
    drop   = FALSE
  ) +
  scale_size_continuous(
    name   = "Sample Size (N)",
    range  = c(0.5, 5),
    limits = c(min(ea_k13_grouped$denominator, na.rm = TRUE),
               max(ea_k13_grouped$denominator, na.rm = TRUE)),
    breaks = pretty(ea_k13_grouped$denominator, n = 5),
    guide = guide_legend(position = "right")
  ) +
  scale_x_continuous(
    breaks = seq(
      from = floor(lims["xmin"] / 4) * 4,
      to   = ceiling(lims["xmax"] / 4) * 4,
      by   = 4
    )
  ) +
  coord_sf(
    xlim = c(lims["xmin"], lims["xmax"]),
    ylim = c(lims["ymin"], lims["ymax"]),
    expand = FALSE
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.text     = element_text(size = 8),
    legend.title    = element_text(size = 9),
    strip.background = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    strip.text      = element_text(size = 9, margin = margin(b = 10)),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
  )

save_figs(file.path(out_plot_dir, "binned_years/EA_inset_map_k13_points_binned"), east_africa_binned_prev_plot, width = 12, res =600)
save_figs(file.path(manuscript_dir, "Fig2B_EA_inset_map_k13_points_binned"), east_africa_binned_prev_plot, width = 12, res =600)

east_africa_binned_prev_plot_sampleSize_legend <- ggplot() +
  facet_wrap(~year_group, nrow = 1) +
  geom_sf(data = africa_admin0_ea, fill = NA, colour = "black",
          show.legend = FALSE, linewidth = 0.1) +
  geom_point(
    data = ea_k13_grouped,
    aes(x = longitude, y = latitude,
        size = denominator,
        fill = prevalence_bin), # Use 'fill' for the colour scale with shape 21
    shape = 21,                   # Use shape 21 for points with fill and border
    colour = "dimgrey",          # Set the border colour to black
    stroke = 0.4,                 # Set the thickness of the border
    alpha = 0.5                   # Set the opacity to 50%
  ) +
  scale_fill_manual(
    name   = "Prevalence (%)",
    values = prev_bin_colors(),
    limits = PREV_LEVELS(),
    drop   = FALSE,
    guide = "none"
  ) +
  scale_size_continuous(
    name   = "Sample Size (N)",
    range  = c(0.5, 5),
    limits = c(min(ea_k13_grouped$denominator, na.rm = TRUE),
               max(ea_k13_grouped$denominator, na.rm = TRUE)),
    breaks = pretty(ea_k13_grouped$denominator, n = 5),
    guide = guide_legend(position = "right")
  ) +
  scale_x_continuous(
    breaks = seq(
      from = floor(lims["xmin"] / 4) * 4,
      to   = ceiling(lims["xmax"] / 4) * 4,
      by   = 4
    )
  ) +
  coord_sf(
    xlim = c(lims["xmin"], lims["xmax"]),
    ylim = c(lims["ymin"], lims["ymax"]),
    expand = FALSE
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    legend.position = "right",
    legend.text     = element_text(size = 8),
    legend.title    = element_text(size = 9),
    strip.background = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    strip.text      = element_text(size = 9, margin = margin(b = 10)),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
  )

save_figs(file.path(out_plot_dir, "binned_years/EA_inset_map_k13_points_binned_sampleSize_legend"), east_africa_binned_prev_plot_sampleSize_legend, width = 12, res =600)
save_figs(file.path(manuscript_dir, "Fig2B_EA_inset_map_k13_points_binned_sampleSize_legend"), east_africa_binned_prev_plot_sampleSize_legend, width = 12, res =600)


east_africa_binned_prev_plot_no_legend <- ggplot() +
  facet_wrap(~year_group, nrow = 1) +
  geom_sf(data = africa_admin0_ea, fill = NA, colour = "black",
          show.legend = FALSE, linewidth = 0.1) +
  geom_point(
    data = ea_k13_grouped,
    aes(x = longitude, y = latitude,
        size = denominator,
        fill = prevalence_bin), # Use 'fill' for the colour scale with shape 21
    shape = 21,                   # Use shape 21 for points with fill and border
    colour = "dimgrey",          # Set the border colour to black
    stroke = 0.4,                 # Set the thickness of the border
    alpha = 0.5                   # Set the opacity to 50%
  ) +
  scale_fill_manual(
    name   = "Prevalence (%)",
    values = prev_bin_colors(),
    limits = PREV_LEVELS(),
    drop   = FALSE,
    guide = "none"
  ) +
  scale_size_continuous(
    name   = "Sample Size (N)",
    range  = c(0.5, 5),
    limits = c(min(ea_k13_grouped$denominator, na.rm = TRUE),
               max(ea_k13_grouped$denominator, na.rm = TRUE)),
    breaks = pretty(ea_k13_grouped$denominator, n = 5),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = seq(
      from = floor(lims["xmin"] / 4) * 4,
      to   = ceiling(lims["xmax"] / 4) * 4,
      by   = 4
    )
  ) +
  coord_sf(
    xlim = c(lims["xmin"], lims["xmax"]),
    ylim = c(lims["ymin"], lims["ymax"]),
    expand = FALSE
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.text     = element_text(size = 8),
    legend.title    = element_text(size = 9),
    strip.background = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    strip.text      = element_text(size = 9, margin = margin(b = 10)),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
  )

save_figs(file.path(out_plot_dir, "binned_years/EA_inset_map_k13_points_binned_no_legend"), east_africa_binned_prev_plot_no_legend, width = 12, res =600)
save_figs(file.path(manuscript_dir, "Fig2B_EA_inset_map_k13_points_binned_no_legend"), east_africa_binned_prev_plot_no_legend, width = 12, res =600)

# ── Supplemental Figure 1: Africa, faceted by year, gradient colour ─────────────────────────
k13_site <- prev_raw %>%
  group_by(latitude, longitude, study_name, country_name,
           site_name, collection_day, year, denominator) %>%
  summarise(k13_prevalence = sum(prevalence, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    prevalence_bin = bin_prevalence(k13_prevalence),
    prevalence_bin = factor(prevalence_bin, levels = PREV_LEVELS())
  ) %>%
  arrange(k13_prevalence)

lon_breaks <- seq(
  from = floor(min(k13_site$longitude, na.rm = TRUE)),
  to   = ceiling(max(k13_site$longitude, na.rm = TRUE)),
  by   = 15
)

##Supplement 2010-2024 option
africa_2010_2024_per_year_prev_plot <- ggplot() +
  facet_wrap(~year, ncol = 3) +
  geom_sf(data = africa_admin0, fill = NA, colour = "black",
          show.legend = FALSE, linewidth = 0.1) +
  geom_point(
    data = k13_site %>% filter(year >= 2010),
    aes(x = longitude, y = latitude,
        fill = prevalence_bin, size = denominator),
    shape = 21,                   # Use shape 21 for points with fill and border
    colour = "dimgrey",          # Set the border colour to black
    stroke = 0.4,                 # Set the thickness of the border
    alpha = 0.5
  ) +
  scale_fill_manual(
    name   = "Prevalence (%)",
    values = prev_bin_colors(),
    limits = PREV_LEVELS(),
    drop   = FALSE
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  scale_size_continuous(name = "Sample Size (N)", range = c(0.2, 8)) +
  scale_x_continuous(breaks = lon_breaks) +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    title = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 30, hjust = 1),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    plot.title = element_text(hjust = 0),
    strip.background = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    strip.text = element_text(size = 9)
  )

save_figs(file.path(out_plot_dir, "all_years/africa_map_k13_points_2010_2024"), africa_2010_2024_per_year_prev_plot, width = 9, height = 12)
save_figs(file.path(supplement_dir, "SFig1_africa_map_k13_points_2010_2024"), africa_2010_2024_per_year_prev_plot, width = 9, height = 12)

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

lon_breaks_east <- seq(
  from = floor(min(k13_site$longitude, na.rm = TRUE)),
  to   = ceiling(max(k13_site$longitude, na.rm = TRUE)),
  by   = 5
)

east_africa_2010_2024_per_year_prev_plot <- ggplot() +
  facet_wrap(~year, ncol = 4) +
  geom_sf(data = africa_admin0, fill = NA, colour = "black",
          show.legend = FALSE, linewidth = 0.1) +   # full layer is fine
  geom_point(
    data = k13_site_inbox %>% filter(year >= 2010),
    aes(x = longitude, y = latitude, fill = prevalence_bin , size = denominator),
    shape = 21,                   # Use shape 21 for points with fill and border
    colour = "dimgrey",          # Set the border colour to black
    stroke = 0.4,                 # Set the thickness of the border
    alpha = 0.5
  ) +
  scale_fill_manual(
    name   = "Prevalence (%)",
    values = prev_bin_colors(),
    limits = PREV_LEVELS(),
    drop   = FALSE
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  scale_size_continuous(name = "Sample Size (N)", range = c(0.5, 12)) +
  scale_x_continuous(breaks = lon_breaks_east) +
  coord_sf(
    xlim = c(lims["xmin"], lims["xmax"]),
    ylim = c(lims["ymin"], lims["ymax"]),
    expand = FALSE
  ) +
 theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    title = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 30, hjust = 1),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    plot.title = element_text(hjust = 0),
    strip.background = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    strip.text = element_text(size = 9, margin = margin(b = 10))
  )

save_figs(file.path(out_plot_dir, "all_years/EA_inset_map_k13_points_all_years_2010_2024"), east_africa_2010_2024_per_year_prev_plot, width = 9, height = 12)
save_figs(file.path(supplement_dir, "SFig2_EA_inset_map_k13_points_all_years_2010_2024"), east_africa_2010_2024_per_year_prev_plot, width = 9, height = 12)
