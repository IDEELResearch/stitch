#' Create a Raster Object from a Spatial Object
#'
#' This function converts a spatial object into a raster object by rasterizing it based on a specified attribute field.
#'
#' @param shape_obj An `sf` or `SpatVector` object representing the spatial data to be rasterized.
#' @param raster_field A character string specifying the name of the attribute field in `shape_obj` to use for raster cell values.
#' @param res Numeric value specifying the resolution of the raster grid (default is `0.05`).
#' @return A `SpatRaster` object created by rasterizing the input spatial data. Each cell in the raster corresponds to an area in the spatial object, with cell values based on the specified attribute field.
#' @importFrom terra rast rasterize ext
#' @examples
#' \dontrun{
#' # Load required libraries
#' library(terra)
#' library(sf)
#'
#' # Example of usage:
#' # Read in Africa shapefile
#' africa_shp <- st_read("path_to_africa_shapefile.shp")
#'
#' # Create a raster for Uganda
#' uganda_shp <- africa_shp[africa_shp$iso == "UGA", ]
#' uganda_raster <- create_raster_object(shape_obj = uganda_shp, raster_field = "iso")
#'
#' # Plot Uganda raster with Ugandan border
#' plot(uganda_raster)
#' plot(st_geometry(uganda_shp), add = TRUE, border = "black")
#'
#' # Save the raster to a file
#' writeRaster(
#'   uganda_raster,
#'   filename = "uganda_raster_admin0.tif",
#'   filetype = "GTiff",
#'   overwrite = TRUE
#' )
#'
#' # Plot raster via ggplot2
#' library(ggplot2)
#' uganda_raster_df <- as.data.frame(uganda_raster, xy = TRUE, na.rm = TRUE)
#' ggplot() +
#'   geom_raster(data = uganda_raster_df, aes(x = x, y = y, fill = iso)) +
#'   coord_equal() +
#'   theme_minimal() +
#'   labs(
#'     title = "Uganda Raster",
#'     x = "Longitude",
#'     y = "Latitude"
#'   )
#'
#' # Create African raster
#' africa_raster <- create_raster_object(shape_obj = africa_shp, raster_field = "iso")
#' plot(africa_raster)
#' }

create_raster_object <- function(shape_obj, raster_field, res = 0.05) {
  # Create an empty raster grid covering the extent of the shape object with the specified resolution
  raster_grid <- rast(ext = ext(shape_obj), resolution = res)

  # Rasterize the shape object using the 'iso' column (country codes) as values for the raster cells
  rasterized <- rasterize(shape_obj, raster_grid, field = raster_field, background = NA)

  #change name of raster of the raster layer
  names(rasterized) <- raster_field

  # Return the rasterized object
  return(rasterized)
}
