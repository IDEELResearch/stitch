#' Plot binned prevalence across Africa by year group
#'
#' Creates a faceted map of site-level prevalence binned into discrete
#' categories, with point size proportional to sample size. Optionally crops to
#' a user-supplied bounding box (e.g., East Africa) and controls axis tick breaks.
#'
#' @param prev_df A data frame of site-level observations containing at least
#'   `longitude`, `latitude`, `denominator`, `prevalence`, and `year_group`.
#'   `prevalence` should be a factor (or will be coerced to a factor) whose
#'   levels match `PREV_LEVELS(mut)`.
#'
#' @param mut Character string specifying the mutation group used to define
#'   prevalence bins and colors. Must be one of:
#'   \describe{
#'     \item{\code{"mdr1"}}{Pfmdr1 partner-drug resistance mutations.}
#'     \item{\code{"crt"}}{Pfcrt partner-drug resistance mutations.}
#'     \item{\code{"k13"}}{Pfkelch13 artemisinin-resistance mutations.}
#'   }
#'   This argument is passed to \code{PREV_LEVELS(mut)} and
#'   \code{prev_bin_colors(mut)} to ensure consistent bin ordering and coloring.
#'
#' @param africa_admin0 An `sf` object of Africa administrative boundaries
#'   (admin0) used as a background outline.
#'
#' @param shp_non_malaria An `sf` object of polygons to overlay as a mask or
#'   background layer (e.g., non-malaria areas). Plotted with
#'   `fill = "grey80"` and no outline.
#'
#' @param lims Optional named numeric vector or `sf::st_bbox` with elements
#'   `xmin`, `xmax`, `ymin`, `ymax`. If `NULL`, limits are derived from `prev_df`.
#'   If `crop = TRUE`, these limits are used for `coord_sf()` cropping.
#'
#' @param size_scale Numeric vector of length 2 giving the point size range
#'   passed to `ggplot2::scale_size_continuous(range = size_scale)`.
#'
#' @param legend Logical; if `TRUE` (default), show legends at the bottom.
#'   If `FALSE`, hide all legends.
#'
#' @param crop Logical; if `TRUE`, crop the map to `lims` via
#'   `ggplot2::coord_sf(xlim = ..., ylim = ...)`. If `FALSE`, no cropping
#'   is applied.
#'
#' @param x_axis_break Numeric; spacing (in degrees) for longitude tick marks.
#'
#' @param y_axis_break Numeric; spacing (in degrees) for latitude tick marks.
#'
#' @param padding_lon_lat Optional numeric; amount of padding (in degrees)
#'   added around the map extent when `crop = FALSE`.
#'
#' @param facet_n_row Integer; number of rows in the `year_group` facet layout.
#'
#' @details
#' The fill scale uses `prev_bin_colors(mut)` and the bin ordering from
#' `PREV_LEVELS(mut)`, ensuring mutation-specific prevalence categories and
#' consistent coloring across figures.
#'
#' Points are plotted as filled circles (shape 21) with `fill = prevalence`
#' and `size = denominator`. Size limits are derived from the observed range
#' of `denominator`, and size breaks are computed using
#' `pretty(..., n = 5)`.
#'
#' Axis breaks are computed from `lims`, rounded to the specified break spacing,
#' and applied via `scale_x_continuous()` and `scale_y_continuous()`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' p <- data_binned_year_plot(
#'   prev_df = k13_grouped,
#'   mut = "k13",
#'   africa_admin0 = africa_admin0,
#'   shp_non_malaria = shp_non_malaria,
#'   size_scale = c(1, 15),
#'   legend = TRUE
#' )
#' p
#'
#' ea_lims <- sf::st_bbox(
#'   c(xmin = 28.48, xmax = 44.5, ymin = -4.60, ymax = 16.00),
#'   crs = sf::st_crs(africa_admin0)
#' )
#'
#' p_ea <- data_binned_year_plot(
#'   prev_df = ea_k13_grouped,
#'   mut = "k13",
#'   africa_admin0 = africa_admin0,
#'   shp_non_malaria = shp_non_malaria,
#'   lims = ea_lims,
#'   size_scale = c(1, 15),
#'   legend = TRUE,
#'   crop = TRUE,
#'   x_axis_break = 2,
#'   y_axis_break = 2,
#'   facet_n_row = 2
#' )
#' p_ea
#' }
#'
#' @importFrom ggplot2 ggplot facet_wrap geom_sf geom_point aes
#' @importFrom ggplot2 scale_fill_manual scale_size_continuous theme_bw labs theme
#' @importFrom ggplot2 element_blank element_rect element_line guide_legend coord_sf
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous
#'
#' @export
data_binned_year_plot <- function(
    prev_df,
    mut,
    africa_admin0,
    shp_non_malaria,
    lims = NULL,
    size_scale,
    legend = TRUE,
    crop = FALSE,
    x_axis_break = 10,
    y_axis_break = 10,
    padding_lon_lat = NULL,
    facet_n_row = 1,
    sample_size_legend =  "right",
    prev_legend = "bottom"
    ){

  # Crop data if crop == TRUE
  if (crop) {
    prev_df <- prev_df %>%
      dplyr::filter(
        dplyr::between(longitude, lims["xmin"], lims["xmax"]),
        dplyr::between(latitude,  lims["ymin"], lims["ymax"])
      )
  }

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
    facet_wrap(~year_group, nrow = facet_n_row) +
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
      values = prev_bin_colors(mut),
      limits = PREV_LEVELS(mut),
      drop   = FALSE
    ) +
    scale_size_continuous(
      name   = "Sample Size (N)",
      range  = size_scale,
      limits = c(min(prev_df$denominator, na.rm = TRUE),
                 max(prev_df$denominator, na.rm = TRUE)),
      breaks = pretty(prev_df$denominator, n = 5),
      guide  = guide_legend(position = sample_size_legend)
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
    p <- p + theme(legend.position = prev_legend)
  } else {
    p <- p + theme(legend.position = "none")
  }

  # Spatial cropping (optional)
  if (crop) {
    p <- p + coord_sf(
      xlim = c(lims["xmin"], lims["xmax"]),
      ylim = c(lims["ymin"], lims["ymax"]),
      expand = FALSE
    )
  } else if (!is.null(padding_lon_lat)) {
    # add padding for Africa plots
    p <- p + coord_sf(
      xlim = c(lims["xmin"] - padding_lon_lat, lims["xmax"] + padding_lon_lat),
      ylim = c(lims["ymin"] - padding_lon_lat, lims["ymax"] + padding_lon_lat),
      expand = FALSE)
  }

  plot_theme_text_size(p)
}
