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
out_plot_dir <- "PD_overall_plots"
manuscript_dir <- "manuscript_fig"
supplement_dir <- "manuscript_fig/supplement_fig"


# --- Load data ----------------------------------------------------------------
prev_raw <- readr::read_csv("analysis/data_derived/partner_drug_get_prevalence.csv", show_col_types = FALSE)
africa_admin0 <- readRDS("analysis/data_derived/sf_admin0_africa.rds")
africa_admin1 <- readRDS("analysis/data_derived/sf_admin1_africa.rds")

# --- Mutation sets ------------------------------------------------------------
pd_mutations <- c("mdr1C:86:N", "crt:76:T")

# --- Supplemental Figure unk: Africa, faceted by year, gradient colour --------
for (mut in pd_mutations) {
  pd_grouped <- prev_raw %>% filter(mutation == mut) %>%
    mutate(
      prevalence_bin = bin_prevalence(prevalence, PREV_LEVELS(mut)),
      prevalence_bin = factor(prevalence_bin, levels = PREV_LEVELS(mut))
    ) %>%
    arrange(prevalence)|>
    filter(denominator > 0)

  # To-do: once cecile has final data; redo individual plots
  africa_all_years_prev_plot <- ggplot() +
    facet_wrap(~year, nrow = 6) +
    geom_sf(data = africa_admin0, fill = NA, colour = "black",
            show.legend = FALSE, linewidth = 0.1) +
    geom_point(
      data = pd_grouped,
      aes(x = longitude, y = latitude, fill = prevalence_bin, size = denominator),
      shape = 21,                   # Use shape 21 for points with fill and border
      colour = "dimgrey",          # Set the border colour to black
      stroke = 0.4,                 # Set the thickness of the border
      alpha = 0.5
    ) +
    scale_fill_manual(
      name   = "Prevalence (%)",
      values = prev_bin_colors(PREV_LEVELS(mut)),
      limits = PREV_LEVELS(mut),
      drop   = FALSE
    ) +
    scale_size_continuous(name = "Sample Size (N)", range = c(0.2, 4),
                          limits = c(min(pd_grouped$denominator, na.rm = TRUE),
                                     max(pd_grouped$denominator, na.rm = TRUE)),
                          breaks = pretty(pd_grouped$denominator, n = 5),
                          guide = guide_legend(position = "right")) +
    theme_void() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      plot.background = element_rect(fill = "white", colour = "white"),
      strip.text = element_text(size = 9)
    )

  save_figs(file.path(out_plot_dir, paste0("all_years/africa_map_", mut,"_points_all_years")), africa_all_years_prev_plot, res =600)
  #save_figs(file.path(supplement_dir, paste0("SFigXX_africa_map_",mut,"_points_all_years")), africa_all_years_prev_plot,res = 600)

  ##Supplement 20XX-20YY option
  africa_all_years_prev_plot <- ggplot() +
    facet_wrap(~year, nrow = 6) +
    geom_sf(data = africa_admin0, fill = NA, colour = "black",
            show.legend = FALSE, linewidth = 0.1) +
    geom_point(
      data = pd_grouped |> filter(year > 1999 & year < 2024), #change YEAR FILTER HERE

      aes(x = longitude, y = latitude,
          fill = prevalence_bin, size = denominator),
      shape = 21,                   # Use shape 21 for points with fill and border
      colour = "dimgrey",          # Set the border colour to black
      stroke = 0.4,                 # Set the thickness of the border
      alpha = 0.5
    ) +
    scale_fill_manual(
      name   = "Prevalence (%)",
      values = prev_bin_colors(PREV_LEVELS(mut)),
      limits = PREV_LEVELS(mut),
      drop   = FALSE
    ) +
    scale_size_continuous(name = "Sample Size (N)", range = c(0.2, 5),
                          limits = c(min(pd_grouped$denominator, na.rm = TRUE),
                                     max(pd_grouped$denominator, na.rm = TRUE)),
                          breaks = pretty(pd_grouped$denominator, n = 5),
                          guide = guide_legend(position = "right")) +
    theme_void() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      plot.background = element_rect(fill = "white", colour = "white"),
      strip.text = element_text(size = 9)
    )

  save_figs(file.path(out_plot_dir, paste0("all_years/africa_map_",mut,"_points_00to23")), africa_all_years_prev_plot, res = 600)
  save_figs(file.path(supplement_dir, paste0("SFigXX_africa_map_",mut,"_points_00to23")), africa_all_years_prev_plot, res = 600)
  message("Saved africa sup plot for: ", mut)
}

# --- Figure 1: Africa, year groups, binned colours ----------------------------
########################SETUP FOR PD BUT NOT PUTTING ANY BINNED IMAGES IN FINAL################33
# for (mut in pd_mutations) {
#   pd_grouped <- prev_raw %>% filter( mutation == mut) %>%
#     add_year_group(year) %>%
#     filter(!is.na(year_group)) %>%
#     mutate(
#       prevalence_bin = bin_prevalence(prevalence, PREV_LEVELS(mut)),
#       prevalence_bin = factor(prevalence_bin, levels = PREV_LEVELS(mut))
#     ) %>%
#     arrange(prevalence)
#
#   africa_binned_prev_plot <- ggplot() +
#     facet_wrap(~year_group, nrow = 1) +
#     geom_sf(data = africa_admin0, fill = NA, colour = "black",
#             show.legend = FALSE, linewidth = 0.1) +
#     geom_point(
#       data = pd_grouped,
#       aes(x = longitude, y = latitude,
#           size = denominator,
#           fill = prevalence_bin), # Use 'fill' for the colour scale with shape 21
#       shape = 21,                   # Use shape 21 for points with fill and border
#       colour = "dimgrey",          # Set the border colour to black
#       stroke = 0.4,                 # Set the thickness of the border
#       alpha = 0.5                   # Set the opacity to 50%
#     ) +
#     scale_fill_manual(
#       name   = "Prevalence (%)",
#       values = prev_bin_colors(PREV_LEVELS(mut)),
#       limits = PREV_LEVELS(mut),
#       drop   = FALSE
#     ) +
#     scale_size_continuous(
#       name   = "Sample Size (N)",
#       range  = c(1, 10),
#       limits = c(min(pd_grouped$denominator, na.rm = TRUE),
#                  max(pd_grouped$denominator, na.rm = TRUE)),
#       breaks = pretty(pd_grouped$denominator, n = 5),
#       guide = guide_legend(position = "right")
#     ) +
#     theme_void() +
#     # labs(
#     #   title = paste("Data points for", mut),
#     # )+
#     theme(
#       legend.position = "bottom",
#       legend.title    = element_text(size = 9),
#       legend.text     = element_text(size = 8),
#       plot.background = element_rect(fill = "white", colour = "white"),
#       strip.text      = element_text(size = 9)
#     )
#
#   save_figs(file.path(out_plot_dir, paste0("binned_years/africa_map_",mut, "_points_binned")), africa_binned_prev_plot, width = 12, res =600)
#   #save_figs(file.path(manuscript_dir, paste0("Fig7A_africa_map_", mut,"_points_binned")), africa_binned_prev_plot, width = 12, res =600)
#   message("Saved africa binned plot for: ", mut)
#
#   africa_binned_prev_plot_sample_size_only <- ggplot() +
#     facet_wrap(~year_group, nrow = 1) +
#     geom_sf(data = africa_admin0, fill = NA, colour = "black",
#             show.legend = FALSE, linewidth = 0.1) +
#     geom_point(
#       data = pd_grouped,
#       aes(x = longitude, y = latitude,
#           size = denominator,
#           fill = prevalence_bin), # Use 'fill' for the colour scale with shape 21
#       shape = 21,                   # Use shape 21 for points with fill and border
#       colour = "dimgrey",          # Set the border colour to black
#       stroke = 0.4,                 # Set the thickness of the border
#       alpha = 0.5                   # Set the opacity to 50%
#     )+
#     scale_fill_manual(
#       name   = "Prevalence (%)",
#       values = prev_bin_colors(PREV_LEVELS(mut)),
#       limits = PREV_LEVELS(mut),
#       drop   = FALSE,
#       guide = "none"
#     ) +
#     scale_size_continuous(
#       name   = "Sample Size (N)",
#       range  = c(1, 10),
#       limits = c(min(pd_grouped$denominator, na.rm = TRUE),
#                  max(pd_grouped$denominator, na.rm = TRUE)),
#       breaks = pretty(pd_grouped$denominator, n = 5),
#       guide = guide_legend(position = "right")
#     ) +
#     theme_void() +
#     theme(
#       legend.position = "right",
#       legend.title    = element_text(size = 9),
#       legend.text     = element_text(size = 8),
#       plot.background = element_rect(fill = "white", colour = "white"),
#       strip.text      = element_text(size = 9)
#     )
#
#   save_figs(file.path(out_plot_dir, paste0("binned_years/africa_map_" ,mut,"_points_sampleSize_legend")), africa_binned_prev_plot_sample_size_only, width = 12, res =600)
#   #save_figs(file.path(manuscript_dir, paste0("Fig7A_africa_map_",mut,"_points_binned_sampleSize_legend")), africa_binned_prev_plot_sample_size_only, width = 12, res =600)
#   message("Saved afSampleSize binned plot for: ", mut)
#
#   africa_binned_prev_plot_no_legend <- ggplot() +
#     facet_wrap(~year_group, nrow = 1) +
#     geom_sf(data = africa_admin0, fill = NA, colour = "black",
#             show.legend = FALSE, linewidth = 0.1) +
#     geom_point(
#       data = pd_grouped,
#       aes(x = longitude, y = latitude,
#           size = denominator,
#           fill = prevalence_bin), # Use 'fill' for the colour scale with shape 21
#       shape = 21,                   # Use shape 21 for points with fill and border
#       colour = "dimgrey",          # Set the border colour to black
#       stroke = 0.4,                 # Set the thickness of the border
#       alpha = 0.5                   # Set the opacity to 50%
#     ) +
#     scale_fill_manual(
#       name   = "Prevalence (%)",
#       values = prev_bin_colors(PREV_LEVELS(mut)),
#       limits = PREV_LEVELS(mut),
#       drop   = FALSE,
#       guide = "none"
#     ) +
#     scale_size_continuous(
#       name   = "Sample Size (N)",
#       range  = c(1, 10),
#       limits = c(min(k13_grouped$denominator, na.rm = TRUE),
#                  max(k13_grouped$denominator, na.rm = TRUE)),
#       breaks = pretty(k13_grouped$denominator, n = 5),
#       guide = "none"
#     ) +
#     theme_void() +
#     theme(
#       legend.position = "right",
#       legend.title    = element_text(size = 9),
#       legend.text     = element_text(size = 8),
#       plot.background = element_rect(fill = "white", colour = "white"),
#       strip.text      = element_text(size = 9)
#     )
#
#   save_figs(file.path(out_plot_dir, paste0("binned_years/africa_map_",mut,"_points_no_legend")), africa_binned_prev_plot_no_legend, width = 12, res =600)
#   #save_figs(file.path(manuscript_dir, paste0("Fig7A_africa_map_",mut,"_points_binned_no_legend")), africa_binned_prev_plot_no_legend, width = 12, res =600)
#   message("Saved afPlotNoLegend binned plot for: ", mut)
# }

# --- Figure 2: East Africa inset, year groups, binned colours -------------------
####################### UPDATED FOR PD BUT NOT PLANNING TO INCLUDE#########################
# build bbox for East Africa
# bbox_ea <- sf::st_bbox(
#   c(xmin = 28.48, xmax = 44.5, ymin = -4.60, ymax = 16.00),
#   crs = sf::st_crs(africa_admin0)
# )
#
# # crop background with the same bbox
# bbox_ea_sf <- sf::st_as_sfc(bbox_ea)   # convert bbox to polygon
# africa_admin0_ea <- sf::st_intersection(
#   sf::st_make_valid(africa_admin0),
#   bbox_ea_sf
# )
# africa_admin0_ea <- africa_admin0 %>% sf::st_make_valid()
#
# # pull x/y limits from the bbox
# lims <- sf::st_bbox(bbox_ea)  # named vector: xmin xmax ymin ymax
#
# for (mut in pd_mutations) {
#   pd_grouped <- prev_raw %>% filter( mutation == mut) %>%
#     add_year_group(year) %>%
#     filter(!is.na(year_group)) %>%
#     mutate(
#       prevalence_bin = bin_prevalence(prevalence, PREV_LEVELS(mut)),
#       prevalence_bin = factor(prevalence_bin, levels = PREV_LEVELS(mut))
#     ) %>%
#     arrange(prevalence)
#
#   ea_pd_grouped <- pd_grouped %>%
#     dplyr::filter(
#       longitude >= bbox_ea["xmin"],
#       longitude <= bbox_ea["xmax"],
#       latitude >= bbox_ea["ymin"],
#       latitude <= bbox_ea["ymax"]
#     )
#
#   east_africa_binned_prev_plot <- ggplot() +
#     facet_wrap(~year_group, nrow = 1) +
#     geom_sf(data = africa_admin0_ea, fill = NA, colour = "black",
#             show.legend = FALSE, linewidth = 0.1) +
#     geom_point(
#       data = ea_pd_grouped,
#       aes(x = longitude, y = latitude,
#           size = denominator,
#           fill = prevalence_bin), # Use 'fill' for the colour scale with shape 21
#       shape = 21,                   # Use shape 21 for points with fill and border
#       colour = "dimgrey",          # Set the border colour to black
#       stroke = 0.4,                 # Set the thickness of the border
#       alpha = 0.5                   # Set the opacity to 50%
#     ) +
#     scale_fill_manual(
#       name   = "Prevalence (%)",
#       values = prev_bin_colors(PREV_LEVELS(mut)),
#       limits = PREV_LEVELS(mut),
#       drop   = FALSE
#     ) +
#     scale_size_continuous(
#       name   = "Sample Size (N)",
#       range  = c(0.5, 5),
#       limits = c(min(ea_pd_grouped$denominator, na.rm = TRUE),
#                  max(ea_pd_grouped$denominator, na.rm = TRUE)),
#       breaks = pretty(ea_pd_grouped$denominator, n = 5),
#       guide = guide_legend(position = "right")
#     ) +
#     coord_sf(
#       xlim = c(lims["xmin"], lims["xmax"]),
#       ylim = c(lims["ymin"], lims["ymax"]),
#       expand = FALSE
#     ) +
#     labs(
#       x = "Longitude",
#       y = "Latitude"
#     ) +
#     theme_classic() +
#     theme(
#       strip.background = element_blank(),
#       legend.position = "bottom",
#       legend.text     = element_text(size = 8),
#       legend.title    = element_text(size = 9),
#       plot.background = element_rect(fill = "white", colour = "white"),
#       strip.text      = element_text(size = 9, margin = margin(b = 10)),
#       panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
#     )
#
#   save_figs(file.path(out_plot_dir, paste0("binned_years/EA_inset_map_",mut,"_points_binned")), east_africa_binned_prev_plot, width = 12, res =600)
#   save_figs(file.path(manuscript_dir, paste0("Fig7B_EA_inset_map_",mut,"_points_binned")), east_africa_binned_prev_plot, width = 12, res =600)
#   message("Saved EAinset binned plot for: ", mut)
#
#   east_africa_binned_prev_plot_sampleSize_legend <- ggplot() +
#     facet_wrap(~year_group, nrow = 1) +
#     geom_sf(data = africa_admin0_ea, fill = NA, colour = "black",
#             show.legend = FALSE, linewidth = 0.1) +
#     geom_point(
#       data = ea_pd_grouped,
#       aes(x = longitude, y = latitude,
#           size = denominator,
#           fill = prevalence_bin), # Use 'fill' for the colour scale with shape 21
#       shape = 21,                   # Use shape 21 for points with fill and border
#       colour = "dimgrey",          # Set the border colour to black
#       stroke = 0.4,                 # Set the thickness of the border
#       alpha = 0.5                   # Set the opacity to 50%
#     ) +
#     scale_fill_manual(
#       name   = "Prevalence (%)",
#       values = prev_bin_colors(PREV_LEVELS(mut)),
#       limits = PREV_LEVELS(mut),
#       drop   = FALSE,
#       guide = "none"
#     ) +
#     scale_size_continuous(
#       name   = "Sample Size (N)",
#       range  = c(0.5, 5),
#       limits = c(min(ea_pd_grouped$denominator, na.rm = TRUE),
#                  max(ea_pd_grouped$denominator, na.rm = TRUE)),
#       breaks = pretty(ea_pd_grouped$denominator, n = 5),
#       guide = guide_legend(position = "right")
#     ) +
#     coord_sf(
#       xlim = c(lims["xmin"], lims["xmax"]),
#       ylim = c(lims["ymin"], lims["ymax"]),
#       expand = FALSE
#     ) +
#     labs(
#       x = "Longitude",
#       y = "Latitude"
#     ) +
#     theme_classic() +
#     theme(
#       strip.background = element_blank(),
#       legend.position = "right",
#       legend.text     = element_text(size = 8),
#       legend.title    = element_text(size = 9),
#       plot.background = element_rect(fill = "white", colour = "white"),
#       strip.text      = element_text(size = 9, margin = margin(b = 10)),
#       panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
#     )
#
#   save_figs(file.path(out_plot_dir, paste0("binned_years/EA_inset_map_",mut,"_points_binned_sampleSize_legend")), east_africa_binned_prev_plot_sampleSize_legend, width = 12, res =600)
#   save_figs(file.path(manuscript_dir, paste0("Fig7B_EA_inset_map_",mut,"_points_binned_sampleSize_legend")), east_africa_binned_prev_plot_sampleSize_legend, width = 12, res =600)
#   message("Saved EASampleSize binned plot for: ", mut)
#
#
#   east_africa_binned_prev_plot_no_legend <- ggplot() +
#     facet_wrap(~year_group, nrow = 1) +
#     geom_sf(data = africa_admin0_ea, fill = NA, colour = "black",
#             show.legend = FALSE, linewidth = 0.1) +
#     geom_point(
#       data = ea_pd_grouped,
#       aes(x = longitude, y = latitude,
#           size = denominator,
#           fill = prevalence_bin), # Use 'fill' for the colour scale with shape 21
#       shape = 21,                   # Use shape 21 for points with fill and border
#       colour = "dimgrey",          # Set the border colour to black
#       stroke = 0.4,                 # Set the thickness of the border
#       alpha = 0.5                   # Set the opacity to 50%
#     ) +
#     scale_fill_manual(
#       name   = "Prevalence (%)",
#       values = prev_bin_colors(PREV_LEVELS(mut)),
#       limits = PREV_LEVELS(mut),
#       drop   = FALSE,
#       guide = "none"
#     ) +
#     scale_size_continuous(
#       name   = "Sample Size (N)",
#       range  = c(0.5, 5),
#       limits = c(min(ea_pd_grouped$denominator, na.rm = TRUE),
#                  max(ea_pd_grouped$denominator, na.rm = TRUE)),
#       breaks = pretty(ea_pd_grouped$denominator, n = 5),
#       guide = "none"
#     ) +
#     coord_sf(
#       xlim = c(lims["xmin"], lims["xmax"]),
#       ylim = c(lims["ymin"], lims["ymax"]),
#       expand = FALSE
#     ) +
#     labs(
#       x = "Longitude",
#       y = "Latitude"
#     ) +
#     theme_classic() +
#     theme(
#       strip.background = element_blank(),
#       legend.position = "bottom",
#       legend.text     = element_text(size = 8),
#       legend.title    = element_text(size = 9),
#       plot.background = element_rect(fill = "white", colour = "white"),
#       strip.text      = element_text(size = 9, margin = margin(b = 10)),
#       panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
#     )
#
#   save_figs(file.path(out_plot_dir, paste0("binned_years/EA_inset_map_",mut,"_points_binned_no_legend")), east_africa_binned_prev_plot_no_legend, width = 12, res =600)
#   save_figs(file.path(manuscript_dir, paste0("Fig7B_EA_inset_map_",mut,"_points_binned_no_legend")), east_africa_binned_prev_plot_no_legend, width = 12, res =600)
#   message("Saved EAPlotNoLegend binned plot for: ", mut)
# }




# --- Supplemental Figure 2: East Africa inset, year groups, binned colours -------------------
######################THIS IS FOR K13 NOT PD##################################################################

# Get first year with any samples inside the bbox
# first_year_in_box <- k13_site %>%
#   dplyr::filter(
#     dplyr::between(longitude, lims["xmin"], lims["xmax"]),
#     dplyr::between(latitude,  lims["ymin"], lims["ymax"])
#   ) %>%
#   dplyr::summarise(first_year = min(year, na.rm = TRUE)) %>%
#   dplyr::pull(first_year)
#
# # Keep only points inside the bbox and from that year onward
# k13_site_inbox <- k13_site %>%
#   dplyr::filter(
#     dplyr::between(longitude, lims["xmin"], lims["xmax"]),
#     dplyr::between(latitude,  lims["ymin"], lims["ymax"]),
#     year >= first_year_in_box
#   )
#
# east_africa_all_years_prev_plot <- ggplot() +
#   facet_wrap(~year) +
#   geom_sf(data = africa_admin0, fill = NA, colour = "black",
#           show.legend = FALSE, linewidth = 0.1) +   # full layer is fine
#   geom_point(
#     data = k13_site_inbox,
#     aes(x = longitude, y = latitude, fill = prevalence_bin , size = denominator),
#     shape = 21,                   # Use shape 21 for points with fill and border
#     colour = "dimgrey",          # Set the border colour to black
#     stroke = 0.4,                 # Set the thickness of the border
#     alpha = 0.5
#   ) +
#   scale_fill_manual(
#     name   = "Prevalence (%)",
#     values = prev_bin_colors(PREV_LEVELS(mut)),
#     limits = PREV_LEVELS(mut),
#     drop   = FALSE
#   ) +
#   scale_size_continuous(name = "Sample Size (N)", range = c(0.5, 5)) +
#   coord_sf(
#     xlim = c(lims["xmin"], lims["xmax"]),
#     ylim = c(lims["ymin"], lims["ymax"]),
#     expand = FALSE
#   ) +
#   theme_void() +
#   theme(
#     legend.position = "right",
#     legend.text  = element_text(size = 8),
#     legend.title = element_text(size = 9),
#     plot.background = element_rect(fill = "white", colour = "white"),
#     strip.text = element_text(size = 9, margin = margin(b = 10))
#   )
#
# save_figs(file.path(out_plot_dir, "all_years/EA_inset_map_k13_points_all_years"), east_africa_all_years_prev_plot)
# save_figs(file.path(supplement_dir, "SFig2_EA_inset_map_k13_points_all_years"), east_africa_all_years_prev_plot)
#
# first_year_in_box <- k13_grouped %>%
#   dplyr::filter(
#     dplyr::between(longitude, lims["xmin"], lims["xmax"]),
#     dplyr::between(latitude,  lims["ymin"], lims["ymax"])
#   ) %>%
#   dplyr::summarise(first_year = min(year, na.rm = TRUE)) %>%
#   dplyr::pull(first_year)
#
# # Keep only points inside the bbox and from that year onward
# k13_site_inbox <- k13_grouped %>%
#   dplyr::filter(
#     dplyr::between(longitude, lims["xmin"], lims["xmax"]),
#     dplyr::between(latitude,  lims["ymin"], lims["ymax"]),
#     year >= first_year_in_box
#   )
#
# ##Second Option 2012-2023
# east_africa_all_years_prev_plot <- ggplot() +
#   facet_wrap(~year) +
#   geom_sf(data = africa_admin0, fill = NA, colour = "black",
#           show.legend = FALSE, linewidth = 0.1) +   # full layer is fine
#   geom_point(
#     data = k13_site_inbox,
#     #%>% dplyr::filter(k13_prevalence > 0) %>% dplyr::arrange(k13_prevalence),
#     aes(x = longitude, y = latitude, fill = prevalence_bin , size = denominator),
#     shape = 21,                   # Use shape 21 for points with fill and border
#     colour = "dimgrey",          # Set the border colour to black
#     stroke = 0.4,                 # Set the thickness of the border
#     alpha = 0.5
#   ) +
#   scale_fill_manual(
#     name   = "Prevalence (%)",
#     values = prev_bin_colors(PREV_LEVELS(mut)),
#     limits = PREV_LEVELS(mut),
#     drop   = FALSE
#   ) +
#   scale_size_continuous(name = "Sample Size (N)", range = c(0.5, 5)) +
#   coord_sf(
#     xlim = c(lims["xmin"], lims["xmax"]),
#     ylim = c(lims["ymin"], lims["ymax"]),
#     expand = FALSE
#   ) +
#   theme_void() +
#   theme(
#     legend.position = "right",
#     legend.text  = element_text(size = 8),
#     legend.title = element_text(size = 9),
#     plot.background = element_rect(fill = "white", colour = "white"),
#     strip.text = element_text(size = 9, margin = margin(b = 10))
#   )
#
# save_figs(file.path(out_plot_dir, "all_years/EA_inset_map_k13_points_12to23"), east_africa_all_years_prev_plot)
# save_figs(file.path(supplement_dir, "SFig2_EA_inset_map_k13_points_12to23"), east_africa_all_years_prev_plot)
