library(sf)

# variables needed for dataframe: lat, lon, time, num, denom, covar1

# find lat lon from stave -> merge with shp -> convert to df (remove geometries)

covars <- readRDS(url("https://github.com/IDEELResearch/scrape/raw/refs/heads/main/analysis/data-derived/final_covariates.rds"))
stave <- readRDS(url("https://github.com/IDEELResearch/stitch/raw/refs/heads/main/analysis/data_derived/mock_stave_prevalence.rds"))

map_data <- get_map_covar_data(covars)

stave %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%  # EPSG:4326 for WGS84
  ggplot() +
    geom_sf() +
    coord_sf()

get_grff_input <- function(stave_obj, covar_obj){

  covar_processed <- get_map_covar_data(covar_obj)
  covar_names <- covar_processed %>% select(-c(iso3c:year)) %>% st_drop_geometry() %>% names()

  sf_use_s2(FALSE)

  stave_obj %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) %>% # EPSG:4326 for WGS84 same as MAP shp files
    st_join(covar_processed, left = TRUE) %>%
    as.data.frame() %>%
    select(lat, lon, collection_day, numerator, denominator, year, all_of(covar_names)) %>%
    mutate(collection_year = year(as.Date(collection_day))) %>%
    filter(collection_year == year) %>%
    relocate(collection_year, .after = collection_day) %>%
    select(-year)

}

input_df <- get_grff_input(stave, covars)

saveRDS(input_df, "analysis/data_derived/input_for_grff.rds")
