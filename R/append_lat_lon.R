#' Get 'centroid' coordinates in lat and lon
#'
#' This function extracts the centroid coordinates from the geometries of a shape file object and appends them as columns
#'
#' @param shp The sf object
#' @return A shape file with lat lon 'centroid' coordinates appended as columns before the geometries
#' @importFrom sf sf_use_s2 st_geometry st_centroid st_coordinates
#' @importFrom dplyr rename relocate
#'
#' @examples
#' # shp_adm0 <- readRDS("analysis/data_derived/sf_admin0_africa.rds")
#' # shp_adm0 <- shp_adm0 %>% append_lat_lon()

append_lat_lon <- function(shp){

  # switching off spherical geometry
  sf_use_s2(F)

  shp <- shp %>%
    # get geometry col
    st_geometry() %>%
    # get centroid of polygon
    st_centroid() %>%
    # convert to lat/lon coords
    st_coordinates() %>%
    # bind with map_data
    cbind(shp) %>%
    rename("lon" = "X",
           "lat" = "Y") %>%
    relocate(c(lat:lon), .before = geometry)

  return(shp)
}
