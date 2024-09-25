#' Plot spatiotemporal maps from raster data for specified variables
#'
#' This function plots spatiotemporal maps from a raster object for each variable specified in the vector `variable_names`.
#' It converts the raster data to a data frame, filters out missing values, and creates a plot using **ggplot2** for each variable.
#' The plots are saved as PNG files in the 'analysis/plots' directory with filenames corresponding to each variable.
#' Note that the raster object should contain layers corresponding to the variables specified in `variable_names`.
#'
#' @param raster A raster object (e.g., a SpatRaster from the **terra** package) containing the data to be plotted.
#' @param variable_names A vector of variable names corresponding to the layers in the raster object to plot. The function will loop over these names.
#' @return The function does not return a value; it saves plots as PNG files in the 'analysis/plots' directory.
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c coord_fixed theme_void theme element_rect ggsave
#' @importFrom scales percent_format
#' @importFrom dplyr filter
#' @importFrom rlang sym .data
#' @importFrom stringr str_glue
#' @examples
#' # Example of how to use the function
#' # library(terra)
#' # Load your raster data (assuming 'stacked_rasters' is a SpatRaster with
#' # layers 'prev_Q50', 'prev_Q2.5', 'prev_Q97.5')
#' # grff_summary <- readRDS("data_derived/GRFF_summary_example.rds")
#' # sf_grff_summary <- st_as_sf(grff_summary, coords = c("lon", "lat"), crs=4326)
#' # variable_names <- c("prev_Q50", "prev_Q2.5", "prev_Q97.5")
#' # plot_spatiotemporal_map_raster(stacked_rasters, variable_names)

plot_spatiotemporal_map_raster <- function(raster, variable_names){
  raster_df <- as.data.frame(raster, xy = TRUE, na.rm = TRUE)
  for (v in variable_names) {
    p <- raster_df %>%
      filter(!is.na(.data[[v]])) %>%
      ggplot(aes(x = .data$x, y = .data$y)) +
      geom_tile(aes(fill = !!sym(v)), color = NA) +
      scale_fill_viridis_c(option = "viridis", labels = scales::percent_format(accuracy = 1)) +
      coord_fixed() +
      #facet_wrap(~year) +
      theme_void(base_size = 14) +
      theme(plot.background = element_rect(fill = "white", color = "white"))

    filename <- str_glue("analysis/plots/map_raster_grff_{v}.png")
    ggsave(filename, p, width = 8, height = 6, dpi = 300)
  }
}
