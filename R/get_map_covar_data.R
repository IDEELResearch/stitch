#' Title: Get map and covariate data in long-format
#'
#' This unlists the covariate data frames and returns a sf object with the covariate data in long format.
#' It expects an object with a list of `covars` covariate data frames (eg by year)
#' and the `map` shp file from `scrape`. Bespoke function to wrangle `final_covariates.rds` from `scrape`.
#'
#' @param covars_obj Description of the parameter 'x'.
#' @return A sf object with covariate data in long format from all covar list
#' @importFrom purrr imap_dfr
#'
#' @examples
#' # Example of how to use the function
#' covars <- readRDS("analysis/data_raw/final_covariates.rds")
#'
#' map_data <- get_map_covar_data(covars)
#'
#' map_data %>%
#' ggplot() +
#'   geom_sf(aes(fill = ft), color = NA) +
#'   geom_sf(data = sf_adm0, color = "black", fill = NA) +
#'   coord_sf()

get_map_covar_data <- function(covars_obj) {
  # Unlist and combine ------------------------------------------------------
  map <- covars_obj$map
  covars <- covars_obj$covars

  covars_df <- imap_dfr(covars, ~ {
    year <- as.integer(sub("Y", "", .y))
    .x %>% mutate(year = year)
  })

  # Join map geometries with covar data -------------------------------------
  map_data <- map %>% left_join(covars_df)

  return(map_data)
}
