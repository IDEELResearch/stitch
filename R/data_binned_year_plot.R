#' Plot binned prevalence across Africa by year group
#'
#' Creates a faceted map of site-level prevalence using discrete prevalence bins,
#' with point size proportional to sample size. The map can optionally be cropped
#' to a user-supplied bounding box (for example, East Africa), and axis tick
#' spacing and tick extent can be controlled separately from the plotted map
#' extent.
#'
#' @param prev_df A data frame of site-level observations containing at least
#'   `longitude`, `latitude`, `denominator`, `prevalence`, and `year_group`.
#'   The `prevalence` column should be a discrete variable (typically a factor or
#'   ordered factor) whose levels match `PREV_LEVELS(mut)`.
#'
#' @param mut Character string specifying the mutation group used to define
#'   prevalence bins and colors. Must be one of:
#'   \describe{
#'     \item{\code{"mdr1"}}{Pfmdr1 partner-drug resistance mutations.}
#'     \item{\code{"crt"}}{Pfcrt partner-drug resistance mutations.}
#'     \item{\code{"k13"}}{Pfkelch13 artemisinin-resistance mutations.}
#'   }
#'   This argument is passed to `PREV_LEVELS(mut)` and
#'   `prev_bin_colors(mut)` to ensure consistent bin ordering and coloring.
#'
#' @param africa_admin0 An `sf` object of Africa administrative boundaries
#'   (admin0) used as a background outline.
#'
#' @param shp_non_malaria An `sf` object of polygons to overlay as a mask or
#'   background layer (for example, non-malaria areas). These polygons are
#'   plotted with `fill = "grey80"` and no outline.
#'
#' @param lims Optional named numeric vector or `sf::st_bbox` with elements
#'   `xmin`, `xmax`, `ymin`, `ymax`. If `NULL`, limits are derived from
#'   `prev_df`. If `crop = TRUE`, these values are used for `coord_sf()`
#'   cropping. If `crop = FALSE`, they are used only to derive default axis
#'   tick locations unless overridden by `x_break_lims` or `y_break_lims`.
#'
#' @param size_scale Numeric vector of length 2 giving the point size range
#'   passed to `ggplot2::scale_size_continuous(range = size_scale)`.
#'
#' @param legend Logical; if `TRUE` (default), legends are shown. If `FALSE`,
#'   all legends are hidden.
#'
#' @param crop Logical; if `TRUE`, crop the map to `lims` via
#'   `ggplot2::coord_sf(xlim = ..., ylim = ...)`. If `FALSE`, no spatial
#'   cropping is applied.
#'
#' @param x_axis_break Numeric; spacing (in degrees) for longitude tick marks.
#'
#' @param y_axis_break Numeric; spacing (in degrees) for latitude tick marks.
#'
#' @param x_break_lims Optional numeric vector of length 2 specifying the minimum
#'   and maximum longitude values used to generate x-axis tick locations.
#'   If `NULL`, x-axis ticks are derived from `lims`.
#'
#' @param y_break_lims Optional numeric vector of length 2 specifying the minimum
#'   and maximum latitude values used to generate y-axis tick locations.
#'   If `NULL`, y-axis ticks are derived from `lims`.
#'
#' @param padding_lon_lat Optional numeric; amount of padding (in degrees)
#'   added around the map extent when `crop = FALSE` and padding is desired
#'   for the `coord_sf()` limits.
#'
#' @param facet_n_row Integer; number of rows in the `year_group` facet layout.
#'
#' @param sample_size_legend Character string giving the legend position for
#'   the sample size legend, passed to `guide_legend(position = ...)`.
#'
#' @param prev_legend Character string giving the overall legend position
#'   applied through `theme(legend.position = ...)`.
#'
#' @details
#' The fill scale uses `prev_bin_colors(mut)` and the bin ordering from
#' `PREV_LEVELS(mut)`, ensuring mutation-specific prevalence categories and
#' consistent coloring across figures.
#'
#' Points are plotted as filled circles (`shape = 21`) with `fill = prevalence`
#' and `size = denominator`. Size limits are derived from the observed range
#' of `denominator`, and size breaks are computed using `pretty(..., n = 5)`.
#'
#' Axis tick locations are computed independently for the x- and y-axes. By
#' default, they are derived from `lims` and rounded to the specified break
#' spacing. Alternatively, users can provide `x_break_lims` and `y_break_lims`
#' to control the extent over which axis ticks are generated without changing
#' the plotted map extent.
#'
#' @return A `ggplot` object.
#'
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
    x_break_lims = NULL,
    y_break_lims = NULL,
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
  round_down <- function(x, by) floor(x / by) * by
  round_up   <- function(x, by) ceiling(x / by) * by

  # X-axis tick locations
  if (is.null(x_break_lims)) {
    x_min_break <- round_down(lims["xmin"], x_axis_break)
    x_max_break <- round_up(lims["xmax"], x_axis_break)
  } else {
    x_min_break <- x_break_lims[1]
    x_max_break <- x_break_lims[2]
  }
  x_breaks <- seq(x_min_break, x_max_break, by = x_axis_break)

  # Y-axis tick locations
  if (is.null(y_break_lims)) {
    y_min_break <- round_down(lims["ymin"], y_axis_break)
    y_max_break <- round_up(lims["ymax"], y_axis_break)
  } else {
    y_min_break <- y_break_lims[1]
    y_max_break <- y_break_lims[2]
  }
  y_breaks <- seq(y_min_break, y_max_break, by = y_axis_break)

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
