library(stringr)
library(terra)
library(dplyr)
library(sf)
library(raster)
library(ggplot2)

# Function to convert a shapefile object to a raster object
#
# Args:
#   shape_obj: An sf or SpatVector object that represents the geographic shapefile data
#              (e.g., African countries or Uganda).
#   res: Numeric value that represents the resolution of the raster. A smaller number gives higher
#        resolution and more detail, but increases computational load (default is 0.05).
#
# Returns:
#   A raster object created by rasterizing the input shapefile. Each cell in the raster corresponds
#   to an area in the shapefile, and the values in the raster are based on the attribute 'iso'.
#
# Example usage:
# # Read in Africa shape file
# rds_file = "analysis/data_derived/sf_admin0_africa.rds"
# africa_shp <- readRDS(file = rds_file)
#
# # Create a raster for Uganda
# uganda_shp <- africa_shp %>% dplyr::filter(iso=="UGA")
# uganda_raster <- create_raster_object(shape_obj=uganda_shp)
# # Plot Uganda raster with Ugandan boarder
# plot(uganda_raster)
# plot(st_geometry(uganda_shp), add=TRUE, border="black")
#
# # Save the raster to a file
# writeRaster(uganda_raster,
#             filename="analysis/data_derived/uganda_raster_admin0.tif",
#             filetype="GTiff",
#             overwrite=TRUE)
#
# # Plot raster via ggplot
# uganda_raster_df <- as.data.frame(uganda_raster, xy=TRUE, na.rm=TRUE)
# ggplot() +
#   geom_raster(data = uganda_raster_df, aes(x = x, y = y)) +
#   #geom_sf(data=uganda_shp, color = "black", size = 0.5) +
#   scale_fill_viridis_c() +  # Optional: Nice color scale
#   coord_equal() +
#   theme_minimal() +
#   labs(
#     title = "Africa Admin 0 Raster",
#     x = "Longitude",
#     y = "Latitude"
#   )
#
# # Create African raster
# africa_raster <- create_raster_object(shape_obj=africa_shp)
# plot(africa_raster)

create_raster_object <- function(shape_obj, res = 0.05) {
  # Create an empty raster grid covering the extent of the shape object with the specified resolution
  raster_grid <- rast(ext = ext(shape_obj), resolution = res)

  # Rasterize the shape object using the 'iso' column (country codes) as values for the raster cells
  rasterized <- rasterize(shape_obj, raster_grid, field = "iso", background = NA)

  # Return the rasterized object
  return(rasterized)
}
