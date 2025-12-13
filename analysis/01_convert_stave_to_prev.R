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
stave <- readRDS("analysis/data_raw/STAVE_combined_prelim.rds")

# read shape files
africa_shp_admin0 <- readRDS("analysis/data_derived/sf_admin0_africa.rds")
africa_shp_admin1 <- readRDS("analysis/data_derived/sf_admin1_africa.rds")
africa_shp_admin0 <- sf::st_transform(africa_shp_admin0, crs = 4326)
africa_shp_admin1 <- sf::st_transform(africa_shp_admin0, crs = 4326)

# Get surveys from STAVE object
survey <- stave$get_surveys()

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

# Initalize dataframe that will hold all prevalences
all_who_prev_data  <- data.frame()

#Loop over STAVE object to pull k13 mutations
start_time <- Sys.time()
count = 0
for (mut in all_who_mutations) {
  count = count + 1
  mutation_time <- Sys.time()
  print(paste0("Processing: ", mut))

  prevalence_data <- stave$get_prevalence(mut) %>%
    mutate(year = lubridate::year(collection_day))
  prevalence_data$mutation <- mut
  all_who_prev_data <- bind_rows(all_who_prev_data, prevalence_data)

  # Mid-loop time logging
  current_time <- Sys.time()
  elapsed <- round(difftime(current_time, mutation_time, units = "secs"), 2)
  print(paste("Elapsed time:", elapsed, "seconds (", count, "of", length(all_who_mutations), ")"))
}

end_time <- Sys.time()
print(paste("Total time taken:", round(difftime(end_time, start_time, units = "secs"), 2), "seconds"))

write.csv(all_who_prev_data, "analysis/data_derived/all_who_get_prevalence.csv", row.names = FALSE)

# --- Extract all partner drug mutation prevalences -----------------
pd_mutations <- c("crt:76:T","mdr1:86:Y", "mdr1:86:N")

pd_prev_data  <- data.frame()
#Loop over STAVE object to pull k13 mutations
start_time <- Sys.time()

for (i in seq_along(pd_mutations)) {
  mut <- pd_mutations[i]
  print(paste0("Processing: ", mut))

  prevalence_data <- stave$get_prevalence(mut) %>%
    mutate(year = substring(collection_day, 0, 4))
  prevalence_data$mutation <- mut
  pd_prev_data <- bind_rows(pd_prev_data, prevalence_data)

  # Mid-loop time logging
  current_time <- Sys.time()
  elapsed <- round(difftime(current_time, start_time, units = "secs"), 2)
  print(paste("Elapsed time:", elapsed, "seconds (", i, "of", length(pd_mutations), ")"))
}

end_time <- Sys.time()
print(paste("Total time taken:", round(difftime(end_time, start_time, units = "secs"), 2), "seconds"))

#add note at beginning if N86 pull so I do not confuse it with calculated

pd_prev_data <- pd_prev_data %>% mutate(mutation = if_else(
  mutation == "mdr1:86:N", "mdr1P:86:N", mutation
))

write.csv(pd_prev_data, "analysis/data-derived/pd_noedit_get_prevalence.csv",header = TRUE)

new_rows <- pd_prev_data %>%
  # Filter for the target mutation
  filter(mutation == "mdr1:86:Y") %>%
  mutate(
    # Calculate inverse numerator
    numerator = denominator - numerator,

    # Calculate inverse prevalence
    prevalence = 100 - prevalence,

    # SWAP and INVERT the Confidence Intervals
    # We use a temporary variable 'old_lower' to store the original lower bound
    # so we can use it to calculate the new upper bound.
    old_lower = prevalence_lower,

    prevalence_lower = 100 - prevalence_upper,
    prevalence_upper = 100 - old_lower,

    # Rename the mutation
    mutation = "mdr1:86:N"
  ) %>%
  # Remove the temporary column we created
  select(-old_lower)

# 3. Bind the new rows to the original dataframe and sort
final_pd_df <- bind_rows(pd_prev_data, new_rows) %>%
  arrange(study_id, mutation)

write.csv(final_pd_df, "analysis/data-derived/pd_get_prevalence.csv",header = TRUE)

################MDR RECALC###########################33
df <- final_pd_df

# 1. Create Base (Y rows)
base_df <- df %>%
  filter(mutation == "mdr1:86:Y")

# 2. Create N Lookup
n_lookup <- df %>%
  filter(mutation == "mdr1:86:N") %>%
  select(survey_id, numerator_N = numerator, prev_N = prevalence)


calc_df <- base_df %>%
  left_join(n_lookup, by = "survey_id") %>%
mutate(
  numerator_N = ifelse(is.na(numerator_N), 0, numerator_N),
  prev_N      = ifelse(is.na(prev_N), 0, prev_N),

  # --- THE FIX ---
  # 1. Run Bob's Formula: (Total - Y) + N
  raw_numerator = (denominator - numerator) + numerator_N,
  raw_prev      = (100 - prevalence) + prev_N,

  # 2. CLAMP the results
  # If raw_numerator > denominator, just take denominator
  numerator  = pmin(raw_numerator, denominator),

  # If raw_prev > 100, just take 100
  prevalence = pmin(raw_prev, 100),

  mutation = "mdr1C:86:N",
  prevalence_lower = NA,
  prevalence_upper = NA
) %>%
  select(-numerator_N, -prev_N, -raw_numerator, -raw_prev)

# 4. Combine
final_df <- bind_rows(df, calc_df) %>%
  arrange(survey_id, mutation) %>% drop_na(prevalence)

final_exp_df <- final_df %>% filter(mutation == "crt:76:T" | mutation == "mdr1C:86:N")

write.csv(final_exp_df,"analysis/data_derived/partner_drug_calc_get_prevalence.csv", row.names = FALSE)
