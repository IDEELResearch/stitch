#' Plot Spatiotemporal Maps from Raster Data for Specified Variables
#'
#' This function plots spatiotemporal maps from a raster dataframe for each variable
#' specified in the vector `variable_names`. It converts the raster data to a
#' data frame, filters out missing values, and creates a plot using **ggplot2**
#' for each variable. If `plot_by_year` is `TRUE`, the function will generate
#' faceted plots by year, requiring a valid `year_array`. Otherwise, the plots
#' are generated without faceting. The resulting plots are saved as PNG files
#' in the 'analysis/plots' directory with filenames corresponding to each variable.
#'
#' @param raster_df A data frame containing the raster data to be plotted. The data frame should contain longitude (`long`), latitude (`lat`), time, and variables for plotting.
#' @param variable_names A vector of variable names corresponding to columns in the `raster_df` to plot. The function loops over these names to generate plots.
#' @param plot_by_year A logical flag indicating whether to create faceted plots by year (default is `TRUE`). If `TRUE`, the `raster_df` must contain a `time` variable for faceting.
#' @return The function does not return a value but saves the plots as PNG files in the 'analysis/plots' directory.
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c coord_fixed theme_void theme element_rect ggsave facet_wrap
#' @importFrom scales percent_format
#' @importFrom dplyr filter
#' @importFrom rlang sym .data
#' @importFrom stringr str_glue
#' @importFrom here here
#' @examples
#' # Example of how to use the function
#' # Assuming 'raster_df' is a data frame with columns 'long', 'lat', 'time', and variables 'prev_Q50', 'prev_Q2.5', etc.
#' # variable_names <- c("prev_Q50", "prev_Q2.5", "prev_Q97.5")
#' # plot_spatiotemporal_map_raster(raster_df, variable_names, plot_by_year = TRUE)
#'
plot_spatiotemporal_map_raster <- function(raster_df, variable_names, plot_by_year = TRUE) {

  if (plot_by_year) {
    for (v in variable_names) {
      p <- raster_df %>%
        filter(!is.na(.data[[v]])) %>%
        ggplot(aes(x = .data$long, y = .data$lat)) +
        geom_tile(aes(fill = !!sym(v)), color = NA) +
        scale_fill_viridis_c(option = "viridis", labels = scales::percent_format(accuracy = 1)) +
        coord_fixed() +
        facet_wrap(~time) +
        theme_void(base_size = 14) +
        theme(plot.background = element_rect(fill = "white", color = "white"))

      filename <- str_glue("analysis/plots/map_slice_raster_grff_{v}.png")
      ggsave(filename = here(filename), plot = p, width = 8, height = 6, dpi = 300)
    }
  } else {
    for (v in variable_names) {
      p <- raster_df %>%
        filter(!is.na(.data[[v]])) %>%
        ggplot(aes(x = .data$long, y = .data$lat)) +
        geom_tile(aes(fill = !!sym(v)), color = NA) +
        scale_fill_viridis_c(option = "viridis", labels = scales::percent_format(accuracy = 1)) +
        coord_fixed() +
        theme_void(base_size = 14) +
        theme(plot.background = element_rect(fill = "white", color = "white"))

      filename <- str_glue("analysis/plots/map_raster_grff_{v}.png")
      ggsave(filename = here(filename), plot = p, width = 8, height = 6, dpi = 300)
    }
  }
}
