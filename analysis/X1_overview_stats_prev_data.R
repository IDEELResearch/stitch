# author: CMS
# description: create overview numbers of prevalence dataframe

library(sf)

prev_df <- read.csv("analysis/data/validated_get_prevalence.csv")

# number of sites and countries per mutation, and first_year and last_year observed
summary_table <- prev_df %>% filter(mutation %in% validated_mutations) %>%
  filter(prevalence > 0) %>%
  group_by(mutation) %>%
  summarize (
    first_year = min(collection_start, na.rm = TRUE),
    last_year = max(collection_start, na.rm = TRUE),
    n_sites = n(),
    n_countries = n_distinct(country_name),
    sample_size = sum(denominator),
    .groups = "drop"
  ) %>%
  arrange(first_year)

write.csv(summary_table, "analysis/data_derived/summary_prev_data.csv")
