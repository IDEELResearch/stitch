# author: CMS and NWY
# description: This file converts the stave object into prevalence data for selected mutations

library(sf)       # for shapefile handling
library(ggplot2)  # for plotting
library(dplyr)    # for data manipulation
library(tidyr)    # for mild clean of stave data
library(usethis)  # req'd for devtools
library(here)     # req'd for devtools
library(devtools) # for github code check
library(countrycode) # for coordinating STAVE country with iso3c code

# disables the use of the S2 geometry engine for spatial operations
sf_use_s2(FALSE)

# --- Read in Data -------------------------------------------------------------

# TO-DO: change back to proper filename
# stave <- readRDS("analysis/data_raw/stave_final_data.rds")
stave <- readRDS("analysis/data_raw/stave_data_2026.03.13.rds")

# read shape files
africa_shp_admin0 <- readRDS("analysis/data_derived/sf_admin0_africa.rds")
africa_shp_admin1 <- readRDS("analysis/data_derived/sf_admin1_africa.rds")
africa_shp_admin0 <- sf::st_transform(africa_shp_admin0, crs = 4326)
africa_shp_admin1 <- sf::st_transform(africa_shp_admin0, crs = 4326)

# Get surveys from STAVE object
survey <- stave$get_surveys()

# To-do: left join admin0 shape file based on lat/lon
# # Convert into coordinates into sf class
# survey_sf <- survey %>%
#   sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
# # Join survey with admin1 shape files
# survey_joined <- sf::st_join(
#   survey_sf,
#   africa_shp_admin0,
#   join = st_within,   # point-in-polygon
#   left = TRUE         # keep all survey rows
# )

# --- Extract all candidate and validated mutation prevalences -----------------
all_who_mutations <- c("k13:446:I", "k13:458:Y", "k13:469:Y", "k13:476:I", "k13:493:H", "k13:539:T",
                       "k13:543:T",  "k13:553:L", "k13:561:H", "k13:574:L", "k13:580:Y", "k13:622:I","k13:675:V",
                       "k13:441:L", "k13:449:A", "k13:469:F", "k13:481:V",
                       "k13:515:K", "k13:527:H", "k13:537:I", "k13:537:D", "k13:538:V",  "k13:568:G")

partner_drug_mutations <- c("crt:76:T","mdr1:86:Y", "mdr1:86:N")

all_mutations <- c(all_who_mutations, partner_drug_mutations)

# Initalize dataframe that will hold all prevalences
all_mut_prev_data  <- data.frame()

#Loop over STAVE object to pull k13 mutations
start_time <- Sys.time()
count = 0
for (mut in all_mutations) {
  count = count + 1
  mutation_time <- Sys.time()
  print(paste0("Processing: ", mut))

  prevalence_data <- stave$get_prevalence(mut) %>%
    mutate(year = lubridate::year(collection_day))
  prevalence_data$mutation <- mut
  all_mut_prev_data <- bind_rows(all_mut_prev_data, prevalence_data)

  # Mid-loop time logging
  current_time <- Sys.time()
  elapsed <- round(difftime(current_time, mutation_time, units = "secs"), 2)
  print(paste("Elapsed time:", elapsed, "seconds (", count, "of", length(all_who_mutations), ")"))
}

end_time <- Sys.time()
print(paste("Total time taken:", round(difftime(end_time, start_time, units = "secs"), 2), "seconds"))

write.csv(all_mut_prev_data, "analysis/data_derived/all_mutations_get_prevalence.csv", row.names = FALSE)
