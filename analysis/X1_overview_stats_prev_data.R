# author: CMS
# description: create overview numbers of prevalence prev_dfframe

library(sf)

# load prevalence prev_df
prev_df <- read.csv("analysis/data_derived/validated_get_prevalence.csv")

# Define validated mutations
validated_mutations <- c("k13:446:I", "k13:458:Y", "k13:469:Y", "k13:476:I",   "k13:493:H",   "k13:539:T",
                         "k13:543:T",  "k13:553:L",   "k13:561:H",   "k13:574:L",  "k13:580:Y",  "k13:622:I","k13:675:V")

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

write.csv(summary_table, "analysis/data_derived/prev_summary_tables/summary_prev_prev_df.csv")

## Create summary table of sample size, study count and survey count per year and country
# Count unique study_IDs per year and country
study_counts_country_year <- prev_df %>%
  distinct(study_id, country_name, year) %>%
  count(year, country_name) %>%
  rename(num_study_ID = n)
# Count unique survey_IDs per year and country
survey_counts_country_year <- prev_df %>%
  distinct(survey_id, country_name, year) %>%
  count(year, country_name) %>%
  rename(num_survey_ID = n)
# Count sample size per year and country
sample_counts_country_year <- prev_df %>%
  group_by(year, country_name) %>%
  summarise(sample_size = n_distinct(survey_id), .groups = 'drop')
# Combine study_counts, survey_counts, and sample_counts
summary_table_per_year_country <- study_counts_country_year %>%
  full_join(survey_counts_country_year, by = c("year", "country_name")) %>%
  full_join(sample_counts_country_year, by = c("year", "country_name"))
# Save table
write.csv(summary_table_per_year_country, "analysis/data_derived/prev_summary_tables/summary_prev_per_country_year.csv")

## Create summary table of sample size, study count and survey count per country
# Count unique study_IDs per country
study_counts_country <- prev_df %>%
  distinct(study_id, country_name) %>%
  count(country_name) %>%
  rename(num_study_ID = n)
# Count unique survey_IDs per country
survey_counts_country <- prev_df %>%
  distinct(survey_id, country_name) %>%
  count(country_name) %>%
  rename(num_survey_ID = n)
# Count sample size per country
sample_counts_country <- prev_df %>%
  group_by(country_name) %>%
  summarise(sample_size = n_distinct(survey_id), .groups = 'drop')
# Combine study_counts, survey_counts, and sample_counts
summary_table_per_country <- study_counts_country %>%
  full_join(survey_counts_country, by = c("country_name")) %>%
  full_join(sample_counts_country, by = c("country_name"))
write.csv(summary_table_per_country, "analysis/data_derived/prev_summary_tables/summary_prev_per_country.csv")

## Create summary table of sample size, study count and survey count per year
# Count unique study_IDs per country
study_counts_year <- prev_df %>%
  distinct(study_id, year) %>%
  count(year ) %>%
  rename(num_study_ID = n)
# Count unique survey_IDs per country
survey_counts_year <- prev_df %>%
  distinct(survey_id, year ) %>%
  count(year ) %>%
  rename(num_survey_ID = n)
# Count sample size per country
sample_counts_year <- prev_df %>%
  group_by(year ) %>%
  summarise(sample_size = n_distinct(survey_id), .groups = 'drop')
# Combine study_counts, survey_counts, and sample_counts
summary_table_per_year <- study_counts_year %>%
  full_join(survey_counts_year, by = c("year")) %>%
  full_join(sample_counts_year, by = c("year"))
write.csv(summary_table_per_year, "analysis/data_derived/prev_summary_tables/summary_prev_per_year.csv")

# Plot Barplot of study IDs per year  -------------------------------------
study_count_barplot <- ggplot(study_counts_year, aes(x = factor(year), y = num_study_ID)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Number of Study IDs") +
  theme_minimal()
ggsave("analysis/plots/studyID_per_year_barplot.png", study_count_barplot)

# Plot Barplot of survey IDs per year  ------------------------------------
survey_count_barplot <- ggplot(survey_counts_year, aes(x = factor(year), y = num_survey_ID)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Number of Survey IDs") +
  theme_minimal()
ggsave("analysis/plots/survey_per_year_barplot.png", survey_count_barplot)

# Plot Barplot of sample size per year for each country -------------------
sample_size_barplot <- ggplot(sample_counts_country_year, aes(x = year, y = sample_size)) +
  geom_bar(stat = "identity") +
  facet_wrap(~country_name) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Sample Size"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
ggsave("analysis/plots/sampleSize_per_year_country_facet_barplot.png", sample_size_barplot)

