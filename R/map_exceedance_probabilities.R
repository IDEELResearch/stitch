here::i_am("R/map_exceedance_probabilities.R")

if (!require("pacman")) {
  install.packages("pacman", repos = "http://cran.us.r-project.org")
  library(pacman)
}

if (!require("INLA")) {
  install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
  library(INLA)
}

pacman::p_load(here,INLA,purrr,tidyverse,ggplot2,spdep)

# Load in lat,lon,posterior draws -----------------------------------------
data <- readRDS(here("data/GRFF_example1.rds"))
#sub <- data[[1]]$post_draws
#draws <- data[[1]]$post_draws[,-c(1:2)]
#locations <- data[[1]]$post_draws[,c(1:2)]

# Then, with those draws, calculate the exceedance probabilities and binary split based on threshold -------------------------
ex_prob_threshold <- 0.1
# todo: is_
class_threshold <- 0.9
calculate_ex_prob <- function(GRFF_obj,
                              ex_prob_threshold,
                              class_threshold) {

  # function to calculate prob over threshold on each row of dataframe
  calc_by_row <- function(draws, ex_prob_threshold){
    # For each item of the list, extract the "post_draws"
    obj <- draws[,-which(
      colnames(draws) %in% c("lat","lon"))]
    prob <- apply(obj, 1, function(row) mean(row > ex_prob_threshold))
    return(prob)
  }

  # Access posterior draws from each timepoint
  post_draws <- GRFF_obj$post_draws

  # Calculate the mean of post_draws greater than the threshold
  ex_prob <- calc_by_row(post_draws,ex_prob_threshold)

  # Append the mean to the list of dataframes for the timepoint
  GRFF_obj$ex_prob <- data.frame(lat=post_draws$lat,lon=post_draws$lon,ex_prob = ex_prob) %>%
    mutate(class=ifelse(ex_prob > class_threshold,TRUE,FALSE))

  return(GRFF_obj)
}

GRFF_obj_prob <- purrr::map(data, ~ calculate_ex_prob(.x, ex_prob_threshold,class_threshold))

#small_test <- draws[1,]
#apply(small_test,1)

# calculate_prob <- function(x,thr){
#   prob <- mean(x[,-c(1:2)] > thr)
#   return(prob)
# }
# test <- calculate_prob(small_test,0.9)
# calculate_ex_prob <- function(GRFF_obj){
#   subset <- GRFF_obj[,-c(1:2)]
#   ex_prob <- subset

# Plot these probs on map -------------------------------------------------
ggplot(GRFF_obj_prob[[6]]$ex_prob, aes(x = lon, y = lat,color=ex_prob)) +
  geom_point() +
  labs(title = "Exceedance Probabilities", x = "Longitude", y = "Latitude") +
  theme_minimal()

ggplot(GRFF_obj_prob[[6]]$ex_prob, aes(x = lon, y = lat,color=class)) +
  geom_point() +
  labs(title = "Exceedance Probabilities", x = "Longitude", y = "Latitude") +
  theme_minimal()

#p <- 222
# calculate area to plot over time ----------------------------------------
exceedance_summary <- do.call(rbind, lapply(seq_along(GRFF_obj_prob), function(p) {
  post_i <- GRFF_obj_prob[[p]]

  sf_data <- as.data.frame(post_i$ex_prob) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

  time <- post_i$time
  sf_data$time <- time

  # combine vs. union: https://geobgu.xyz/r/geometric-operations-with-vector-layers.html
  grouped_sf <- sf_data %>%
    group_by(class) %>%
    summarise(geometry = st_combine(geometry))

  # The convex hull is the smallest convex geometry that encloses all geometries in the input.
  # The st_convex_hull function is used to calculate the convex hull of the given geometry or geometries.
  polygons_sf <- grouped_sf %>%
    # draws a convex area around the area
    mutate(geometry = st_convex_hull(geometry))

  # Check if all points are either TRUE or FALSE
  if (all(sf_data$class == TRUE)) {
    area <- as.numeric(st_area(polygons_sf))/1000

    } else if (all(sf_data$class == FALSE)) {
      area <- 0

      } else {

        sf_data_true <- polygons_sf %>%
          filter(class == TRUE)

        sf_data_false <- polygons_sf %>%
          filter(class == FALSE)

        diff_poly <- st_difference(sf_data_true$geometry,sf_data_false$geometry)

        if (any(st_is(diff_poly, "MULTILINESTRING")==TRUE |
                st_is(diff_poly, "LINESTRING")==TRUE)) {
          diff_poly <- diff_poly %>%
            # NOTE: sometimes st_difference returns a multilinestring if there is a
            # very small region, I am filtering that out currently
            st_collection_extract("POLYGON")
          }

        #km^2
        area <- as.numeric(st_area(diff_poly))/1000}

  final_data <- data.frame(time = time,exceedance_area = area)
  #print(area)
  return(final_data)
}))


# Plot area over time -----------------------------------------------------
ggplot() +
  geom_point(data = exceedance_summary,aes(x = time,y = exceedance_area)) +
  labs(x = "Time", y = "Exceedance Area")

#ggplot() +
  #geom_sf(data = diff_poly,fill="orange", alpha = 0.5) +
  #geom_sf(data = diff_f,fill="lightblue", alpha = 0.5) +
  #geom_sf(data = sf_data, aes(color = class), size = 2) +
  #scale_fill_manual(values = c("lightblue", "orange")) +
  #theme_minimal()

# look at methods to look at spread over space/time

