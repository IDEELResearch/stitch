#' Title: Plotting spatiotemporal maps (time facets) using long sf objects
#'
#' Plot spatiotemporal maps with time facets based on a vector of variable names to loop over.
#' This expects a long format sf object (eg covariate data in long format returned from `get_map_covar_data()`)
#' and a border shp sf object (this could be the same as the long sf if you want the same borders), alternatively,
#' you could supply eg admin0 sf to plot only the country borders. Because rendering takes a lot of time, the function
#' outputs to 'analysis/plots' directory. Will likely edit this to be more felxible (eg user-supplied titles, filenames, color palettes)
#'
#' @param long_sf A long-format sf object
#' @param border_sf Shape files for the borders (eg adm0)
#' @param variable_names A vector of variable names that you wish to plot, it will loop over the names if >1
#' @return Description of the value returned by the function.
#' @export
#'
#' @examples
#' # Example of how to use the function
#' covars <- readRDS("analysis/data_raw/final_covariates.rds")
#' border_sf <- readRDS("analysis/data_derived/sf_admin0_africa.rds")
#' map_data <- get_map_covar_data(covars)
#' variable_names <- c("pfpr210_mean", "ft", "AL")
#' plot_spatiotemporal_map(map_data, border_sf, variable_names)

plot_spatiotemporal_map <- function(long_sf, border_sf, variable_names){

  for (v in variable_names) {
    p <- long_sf %>%
      filter(!is.na(.data[[v]])) %>%
      ggplot() +
      geom_sf(aes(fill = !!sym(v)), color = NA) +
      geom_sf(data = border_sf, color = "black", fill = NA) +
      scale_fill_viridis_c(option = "viridis", labels = scales::percent_format(accuracy = 1)) +
      coord_sf() +
      facet_wrap(~year) +
      theme_void(base_size = 14) +
      theme(plot.background = element_rect(fill = "white", color = "white"))

    filename <- str_glue("analysis/plots/map_{v}.png")
    ggsave(filename, p, width = 8, height = 6, dpi = 300)
  }
}
