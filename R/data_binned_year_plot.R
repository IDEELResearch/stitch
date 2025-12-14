#' Plot binned K13 prevalence across Africa (optionally East Africa inset)
#'
#' Creates a faceted map of site-level K13 prevalence binned into discrete
#' categories, with point size proportional to sample size. Optionally crops to
#' an East Africa bounding box and reduces longitude tick density.
#'
#' @param prev_df A data frame of site-level observations containing at least
#'   `longitude`, `latitude`, `denominator`, `prevalence_bin`, and `year_group`.
#'   `prevalence_bin` should be a factor whose levels match `PREV_LEVELS()`.
#' @param africa_admin0 An `sf` object of Africa administrative boundaries
#'   (admin0) used as a background outline.
#' @param lims Optional named numeric vector (or `sf::st_bbox`) with elements
#'   `xmin`, `xmax`, `ymin`, `ymax`. Required when `east_africa = TRUE`.
#' @param legend Logical; if `TRUE` (default), show legends. If `FALSE`, hide
#'   all legends.
#' @param east_africa Logical; if `TRUE`, crop the map to `lims` via
#'   `ggplot2::coord_sf()` and show longitude axis ticks every 2 degrees.
#'
#' @details
#' The fill scale uses `prev_bin_colors()` and the bin ordering from
#' `PREV_LEVELS()`, which are expected to be available in the package namespace.
#' Point size is mapped to `denominator` and is scaled to a fixed range
#' (`1` to `15`).
#'
#' When `east_africa = TRUE`, `lims` must be supplied and should be in the same
#' coordinate reference system as the plotted longitude/latitude (typically
#' EPSG:4326).
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' p <- africa_overall_plot(k13_grouped, africa_admin0, legend = TRUE)
#' p
#'
#' ea_lims <- sf::st_bbox(c(xmin = 28.48, xmax = 44.5, ymin = -4.60, ymax = 16.00),
#'                        crs = sf::st_crs(africa_admin0))
#' p_ea <- africa_overall_plot(ea_k13_grouped, africa_admin0, lims = ea_lims,
#'                             legend = TRUE, east_africa = TRUE)
#' p_ea
#' }
#'
#' @importFrom ggplot2 ggplot facet_wrap geom_sf geom_point aes
#' @importFrom ggplot2 scale_fill_manual scale_size_continuous theme_bw labs theme
#' @importFrom ggplot2 element_blank element_rect element_line element_text
#' @importFrom ggplot2 guide_legend coord_sf scale_x_continuous
#'
#' @export
data_binned_year_plot <- function(
    prev_df,
    africa_admin0,
    shp_non_malaria,
    lims = NULL,
    size_scale,
    legend = TRUE,
    east_africa = FALSE,
    x_axis_break = 10,
    y_axis_break = 10,
    n_facet_wrap = 1
    ){

  # Enforce factor levels
  prev_df <- prev_df %>%
    dplyr::mutate(
      prevalence = factor(prevalence, levels = PREV_LEVELS(), ordered = TRUE)
    )
  cols <- prev_bin_colors()
  cols <- cols[PREV_LEVELS()]      # reorder to match levels
  names(cols) <- PREV_LEVELS()

  # If lims not provided, derive from data (or sf background)
  if (is.null(lims)) {
    lims <- c(
      xmin = min(prev_df$longitude, na.rm = TRUE),
      xmax = max(prev_df$longitude, na.rm = TRUE),
      ymin = min(prev_df$latitude,  na.rm = TRUE),
      ymax = max(prev_df$latitude,  na.rm = TRUE)
    )
  }

  # Breaks (in degrees) based on lims
  round_down <- function(x, by) floor(x / 5) * 5
  round_up   <- function(x, by) ceiling(x / 5) * 5
  x_min <- round_down(lims["xmin"], x_axis_break)
  x_max <- round_up(  lims["xmax"], x_axis_break)

  y_min <- round_down(lims["ymin"], y_axis_break)
  y_max <- round_up(  lims["ymax"], y_axis_break)

  x_breaks <- seq(x_min, x_max, by = x_axis_break)
  y_breaks <- seq(y_min, y_max, by = y_axis_break)

  p <- ggplot() +
    facet_wrap(~year_group, nrow = n_facet_wrap) +
    geom_sf(data = africa_admin0, fill = "white", colour = "black", show.legend = FALSE, linewidth = 0.1) +
    geom_sf(data = shp_non_malaria, fill = "grey80", colour = NA) +
    geom_point(
      data = prev_df %>% arrange(prevalence),
      aes(
        x = longitude, y = latitude,
        size = denominator,
        fill = prevalence
      ),
      shape = 21,
      colour = "dimgrey",
      stroke = 0.4,
      alpha = 0.5
    ) +
    scale_fill_manual(
      name   = "Prevalence (%)",
      values = cols,
      limits = PREV_LEVELS(),
      drop   = FALSE
    ) +
    scale_size_continuous(
      name   = "Sample Size (N)",
      range  = size_scale,
      limits = c(min(prev_df$denominator, na.rm = TRUE),
                 max(prev_df$denominator, na.rm = TRUE)),
      breaks = pretty(prev_df$denominator, n = 5),
      guide  = guide_legend(position = "right")
    ) +
    theme_bw() +
    labs(x = "Longitude", y = "Latitude") +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.border     = element_rect(colour = "black", fill = NA, linewidth = 0.6),
      axis.line        = element_line(colour = "black"),
      strip.background = element_blank(),
      plot.background  = element_rect(fill = "white", colour = NA)
    )

  # Legend handling
  if (legend) {
    p <- p + theme(legend.position = "bottom")
  } else {
    p <- p + theme(legend.position = "none")
  }

  # Spatial cropping (optional)
  if (east_africa) {
    p <- p + coord_sf(
      xlim = c(lims["xmin"], lims["xmax"]),
      ylim = c(lims["ymin"], lims["ymax"]),
      expand = FALSE
    )
  } else {
    p <- p + coord_sf(expand = FALSE)
  }

  plot_theme_text_size(p)
}
