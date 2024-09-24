# title: "Generating base maps"
# author: "Shazia Ruybal-Pesántez"

# Load packages -----------------------------------------------------------
# devtools::install_github("malaria-atlas-project/malariaAtlas")

library(malariaAtlas)
library(dplyr)
library(sf)

# Get shape files ---------------------------------------------------------

# Use `malariaAtlas` R package to get shape files for Admin 0
adm0_shp_available <- malariaAtlas::listShp(admin_level = "admin0", printed = F) # 249 countries
shp_adm0 <- malariaAtlas::getShp(ISO = adm0_shp_available$iso, admin_level = "admin0")

adm1_shp_available <- malariaAtlas::listShp(admin_level = "admin1", printed = F) # 2022 adm1

shp_adm1 <- malariaAtlas::getShp(ISO = adm0_shp_available$iso, admin_level = "admin1")


# Convert to sf object ----------------------------------------------------

sf_adm0 <- shp_adm0 %>% sf::st_as_sf()
sf_adm1 <- shp_adm1 %>% sf::st_as_sf()

# Subset SSA --------------------------------------------------------------

africa_iso3 <- c(
  "DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD",
  "COM", "COD", "COG", "DJI", "EGY", "GNQ", "ERI", "SWZ", "ETH", "GAB",
  "GMB", "GHA", "GIN", "GNB", "CIV", "KEN", "LSO", "LBR", "LBY", "MDG",
  "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM", "NER", "NGA", "RWA",
  "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TGO", "TUN",
  "UGA", "TZA", "ZMB", "ZWE", "ESH"
)

sf_adm0_africa <- sf_adm0 %>% filter(iso %in% africa_iso3)
sf_adm1_africa <- sf_adm1 %>% filter(iso %in% africa_iso3)

# Save base maps ----------------------------------------------------------
saveRDS(sf_adm0, "analysis/data_derived/sf_admin0.rds")
saveRDS(sf_adm1, "analysis/data_derived/sf_admin1.rds")
saveRDS(sf_adm0_africa, "analysis/data_derived/sf_admin0_africa.rds")
saveRDS(sf_adm1_africa, "analysis/data_derived/sf_admin1_africa.rds")
