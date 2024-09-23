library(stringr)
library(terra)

# Function to plot African countries with random colors using rasterization
#
# Args:
#   amdin_level:  A R object that represents the shapefile of a specific region of interest.
#                 This R object should be a 'sf' or 'sp' classes.
#
# Returns:
#   A raster object for a specific region
############ Example usage ############
#rds_file = "analysis/data_derived/sf_admin0_africa.rds"
#africa_shp <- readRDS(file = rds_file)
#africa_raster <- create_raster_object(africa_shp)

create_raster_object <- function(shape_obj) {
  # Create an empty raster for Africa with the specified resolution
  # The raster will cover the extent of the Africa shapefile, and each cell will have the given resolution
  raster_obj <- terra::rast(ext = ext(shape_obj), resolution = 0.1)  # 'res' controls the level of detail

  # Return the ggplot object
  return(raster_obj)
}
