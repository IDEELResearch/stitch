#' Plot binned K13 prevalence faceted by year
#'
#' Creates a faceted map (one panel per calendar year) of site-level K13
#' prevalence binned into discrete categories, with point size proportional
#' to sample size. Axis tick spacing is explicitly controlled and can be
#' optionally cropped to a specified spatial bounding box.
#'
#' @param prev_df A data frame of site-level observations containing at least
#'   the columns `longitude`, `latitude`, `year`, `denominator`, and
#'   `prevalence`. The `prevalence` column should be a factor whose levels
#'   match `PREV_LEVELS()`.
#' @param africa_admin0 An `sf` object of administrative boundaries (e.g.,
#'   Africa admin0) used as a background outline.
#' @param lims Optional named numeric vector or `sf::st_bbox` with elements
#'   `xmin`, `xmax`, `ymin`, `ymax`. If `NULL`, limits are inferred from
#'   `prev_df`.
#' @param size_scale Numeric vector of length two specifying the minimum and
#'   maximum point sizes passed to
#'   `ggplot2::scale_size_continuous(range = size_scale)`.
#' @param x_axis_break Numeric; spacing (in degrees) between longitude axis
#'   tick marks. Default is `10`.
#' @param y_axis_break Numeric; spacing (in degrees) between latitude axis
#'   tick marks. Default is `10`.
#' @param facet_n_row Integer; number of rows used in `facet_wrap()` for the
#'   year panels. Default is `4`.
#' @param crop Logical; if `TRUE`, the plot is cropped to `lims` using
#'   `ggplot2::coord_sf()`. If `FALSE` (default), the full spatial extent
#'   is shown.
#'
#' @details
#' Axis tick locations are computed from `lims` and expanded to integer
#' boundaries before applying the specified spacing. The function expects
#' `prev_bin_colors()`, `PREV_LEVELS()`, and `plot_theme_text_size()` to be
#' available in the package namespace.
#'
#' The function assumes longitude/latitude coordinates in degrees
#' (typically EPSG:4326). If the background `sf` object is in a projected CRS,
#' it should be transformed prior to plotting.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' # Full Africa, all years
#' p <- africa_per_year_plot(
#'   prev_df = k13_prev_per_year,
#'   africa_admin0 = africa_admin0,
#'   size_scale = c(0.2, 4)
#' )
#' p
#'
#' # Cropped East Africa inset with finer tick spacing
#' ea_lims <- sf::st_bbox(
#'   c(xmin = 28.48, xmax = 44.5, ymin = -4.60, ymax = 16.00),
#'   crs = sf::st_crs(africa_admin0)
#' )
#'
#' p_ea <- africa_per_year_plot(
#'   prev_df = east_africa_k13_prev_per_year,
#'   africa_admin0 = africa_admin0_ea,
#'   lims = ea_lims,
#'   size_scale = c(0.5, 5),
#'   x_axis_break = 5,
#'   y_axis_break = 5,
#'   crop = TRUE
#' )
#' p_ea
#' }
#'
#' @importFrom ggplot2 ggplot facet_wrap geom_sf geom_point aes
#' @importFrom ggplot2 scale_fill_manual scale_size_continuous scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 labs theme_bw theme coord_sf
#' @importFrom ggplot2 element_blank element_rect element_line
#'
#' @export
data_per_year_plot <- function(
    prev_df,
    africa_admin0,
    lims = NULL,
    size_scale,
    x_axis_break = 10,
    y_axis_break = 10,
    facet_n_row = 4,
    crop = FALSE
) {

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
  x_breaks <- seq(ceiling(lims["xmin"]), floor(lims["xmax"]), by = x_axis_break)
  y_breaks <- seq(ceiling(lims["ymin"]), floor(lims["ymax"]), by = y_axis_break)

  p <- ggplot() +
    facet_wrap(~year, nrow = facet_n_row) +
    geom_sf(data = africa_admin0, fill = NA, colour = "black",
            show.legend = FALSE, linewidth = 0.1) +
    geom_point(
      data = prev_df,
      aes(x = longitude, y = latitude, fill = prevalence, size = denominator),
      shape = 21,
      colour = "dimgrey",
      stroke = 0.4,
      alpha = 0.5
    ) +
    scale_fill_manual(
      name   = "Prevalence (%)",
      values = prev_bin_colors(),
      limits = PREV_LEVELS(),
      drop   = FALSE
    ) +
    scale_size_continuous(name = "Sample Size (N)", range = size_scale) +
    labs(x = "Longitude", y = "Latitude") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.border     = element_rect(colour = "black", fill = NA, linewidth = 0.6),
      axis.line        = element_line(colour = "black"),
      strip.background = element_blank(),
      plot.background  = element_rect(fill = "white", colour = NA)
    ) +
    # Axis tick placement
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks)

  # Only crop if requested (no “east_africa” naming)
  if (crop) {
    p <- p + coord_sf(
      xlim = c(lims["xmin"], lims["xmax"]),
      ylim = c(lims["ymin"], lims["ymax"]),
      expand = FALSE
    )
  } else {
    # still use coord_sf for sf plots; no explicit xlim/ylim
    p <- p + coord_sf(expand = FALSE)
  }

  plot_theme_text_size(p)
}
