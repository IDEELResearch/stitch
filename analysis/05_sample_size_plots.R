


# Create sample size plots
# Add prevalence data to admin 1 shape file
africa_admin1_longlat_prev <- africa_admin1_longlat %>%
  left_join(all_prev_data, by = "name_1") %>%
  filter(!is.na(sample_size))

# Plot the Africa map coloring districts by sample_size
africa_map_sample_fill <- ggplot() +
  facet_wrap(~year) +
  geom_sf(data = africa_admin1_longlat_prev, aes(fill = sample_size), color = "darkgrey", lwd = 0.05) +
  geom_sf(data = africa_shp_admin0, fill = NA, color = "black", show.legend = FALSE, lwd = 0.05) +
  theme_void(base_size = 14) +
  labs(fill = "Sample Size (N)") +
  scale_fill_viridis_c() +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color="white"))

ggsave(filename="analysis/plots/africa_map_sample_fill.png", africa_map_sample_fill)


selected_mutation = "k13:561:H"

for (selected_mutation in all_who_mutations){
  #filter all_prev data to selected mutation
  prevalence_data <- all_prev_data %>% filter(mutation == selected_mutation)

  #associate prevalence data lat lons with clean admin_1 names
  prevalence_data <- left_join(prevalence_data, clean_admin1) %>% drop_na()

  # Add prevalence data to admin 1 shape file
  africa_admin1_longlat_prev <- africa_admin1_longlat %>%
    left_join(prevalence_data, by = "name_1") %>%
    filter(!is.na(denominator))

  # Plot the Africa map coloring districts by prevalence_size
  africa_map_sample_fill <- ggplot() +
    facet_wrap(~year) +
    geom_sf(data = africa_admin1_longlat_prev, aes(fill = prevalence), color = "darkgrey", lwd = 0.05) +
    geom_sf(data = africa_shp_admin0, fill = NA, color = "black", show.legend = FALSE, lwd = 0.05) +
    theme_void(base_size = 14) +
    labs(fill = "Prevalence (N)") +
    scale_fill_viridis_c() +
    theme(legend.position = "bottom",
          plot.background = element_rect(fill = "white", color="white"))

  ggsave(
    filename=paste0("analysis/plots/africa_map_", gsub(":","_", selected_mutation), "prev_facet.png"),
    plot = africa_map_sample_fill,
    width = 12, height = 10, units = "in", dpi = 300)

  # Plot the Africa map coloring districts by sample size
  africa_map_sample_fill <- ggplot() +
    facet_wrap(~year) +
    geom_sf(data = africa_admin1_longlat_prev, aes(fill = denominator), color = "darkgrey", lwd = 0.05) +
    geom_sf(data = africa_shp_admin0, fill = NA, color = "black", show.legend = FALSE, lwd = 0.05) +
    theme_void(base_size = 14) +
    labs(fill = "Sample Size (N)") +
    scale_fill_viridis_c() +
    theme(legend.position = "bottom",
          plot.background = element_rect(fill = "white", color="white"))

  ggsave(
    filename=paste0("analysis/plots/africa_map_", gsub(":","_", selected_mutation), "facet.png"),
    plot = africa_map_sample_fill,
    width = 12, height = 10, units = "in", dpi = 300)

  print(paste("Save plot for:", selected_mutation))
}

