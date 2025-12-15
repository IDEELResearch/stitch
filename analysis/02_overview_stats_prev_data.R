# author: CMS
# description: Create overview tables/plots for K13 prevalence (prev_df)

# -- Packages ------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(stringr)
  library(purrr)
})

# --Define plot output path ----------------------------------------------------
plot_dir <- "plots"

# -- Load data -------------------------------------------------------------------
prev_df <- readr::read_csv("analysis/data_derived/all_who_get_prevalence.csv", show_col_types = FALSE)

# WHO validated + candidate K13 mutations
all_who_mutations <- c(
  "k13:446:I","k13:458:Y","k13:469:Y","k13:476:I","k13:493:H","k13:539:T",
  "k13:543:T","k13:553:L","k13:561:H","k13:574:L","k13:580:Y","k13:622:I","k13:675:V",
  "k13:441:L","k13:449:A","k13:469:F","k13:481:V",
  "k13:515:K","k13:527:H","k13:537:I","k13:537:D","k13:538:V","k13:568:G"
)

# -- Simple counts / overview ----------------------------------------------------
# Unique georeferenced sites (lat/long pairs)
n_sites_geo <- prev_df %>% distinct(latitude, longitude) %>% nrow()
n_countries <- prev_df %>% distinct(country_name) %>% arrange(country_name)
#Check manually for repeat names
write.csv(n_countries,file.path(out_dir,"total_countries.csv"))
n_studies <- prev_df %>% distinct(study_id)

# Per-survey maximum sample size (as in your original code)
num_samples <- prev_df %>%
  group_by(survey_id) %>%
  summarise(max_sample = max(denominator, na.rm = TRUE), .groups = "drop")
message("Total genotyped samples in study: ", sum(num_samples$max_sample))

# Positive-prevalence subset (any > 0)
prev_df_pos <- prev_df %>% filter(prevalence > 0)

# -- Summary overall (WHO list, positive prevalence only) -------------------
overall_summary_table <- prev_df %>%
  filter(mutation %in% all_who_mutations, prevalence > 0) %>%
  summarise(
    first_year   = min(collection_start, na.rm = TRUE),
    last_year    = max(collection_start, na.rm = TRUE),
    n_studies    = n_distinct(study_id),
    n_sites      = n(),                                # number of rows/sites reported
    n_countries  = n_distinct(country_name),
    sample_size  = sum(denominator, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(first_year)

# -- Summary per mutation (WHO list, positive prevalence only) -------------------
summary_table <- prev_df %>%
  filter(mutation %in% all_who_mutations, prevalence > 0) %>%
  group_by(mutation) %>%
  summarise(
    first_year   = min(collection_start, na.rm = TRUE),
    last_year    = max(collection_start, na.rm = TRUE),
    n_studies = n_distinct(study_id),
    n_sites      = n(),                                # number of rows/sites reported
    n_countries  = n_distinct(country_name),
    sample_size  = sum(denominator, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(first_year)

save_csv(summary_table, file.path(out_dir, "summary_prev_prev_df.csv"))

# -- Per-mutation first/last year stats (any vs positive) ------------------------
summary_stats_indv_mut <- prev_df %>%
  group_by(mutation) %>%
  summarise(
    first_pos_year = suppressWarnings(min(year[prevalence > 0], na.rm = TRUE)),
    first_any_year = min(year, na.rm = TRUE),
    last_pos_year  = suppressWarnings(max(year[prevalence > 0], na.rm = TRUE)),
    last_any_year  = max(year, na.rm = TRUE),

    avg_prev_first_pos_year = mean(prevalence[year == first_pos_year & prevalence > 0], na.rm = TRUE),
    avg_prev_first_any_year = mean(prevalence[year == first_any_year], na.rm = TRUE),
    avg_prev_last_pos_year  = mean(prevalence[year == last_pos_year & prevalence > 0], na.rm = TRUE),
    avg_prev_last_any_year  = mean(prevalence[year == last_any_year], na.rm = TRUE),
    .groups = "drop"
  )

save_csv(summary_stats_indv_mut, file.path(out_dir, "summary_prev_per_mutation_years.csv"))

# -- Year × Country summaries ----------------------------------------------------
# Note: In your original code, "sample_size" was n_distinct(survey_id).
# Many readers expect "sample_size" to mean a count of participants.
# To preserve your output and add clarity, we include BOTH:
#   - n_surveys (distinct survey_id)
#   - total_denominator (sum of denominator)

study_counts_country_year <- prev_df %>%
  distinct(study_id, country_name, year) %>%
  count(year, country_name, name = "num_study_ID")

survey_counts_country_year <- prev_df %>%
  distinct(survey_id, country_name, year) %>%
  count(year, country_name, name = "num_survey_ID")

sample_counts_country_year <- prev_df %>%
  group_by(year, country_name) %>%
  summarise(
    n_surveys         = n_distinct(survey_id),
    total_denominator = sum(denominator, na.rm = TRUE),
    .groups = "drop"
  )

summary_table_per_year_country <- study_counts_country_year %>%
  full_join(survey_counts_country_year, by = c("year","country_name")) %>%
  full_join(sample_counts_country_year, by = c("year","country_name")) %>%
  # keep your original column name for backwards compatibility
  mutate(sample_size = n_surveys) %>%
  select(year, country_name, num_study_ID, num_survey_ID, sample_size, total_denominator)

save_csv(summary_table_per_year_country,
         file.path(out_dir, "summary_prev_per_country_year.csv"))

# -- Country-only summaries ------------------------------------------------------
study_counts_country <- prev_df %>%
  distinct(study_id, country_name) %>%
  count(country_name, name = "num_study_ID")

survey_counts_country <- prev_df %>%
  distinct(survey_id, country_name) %>%
  count(country_name, name = "num_survey_ID")

sample_counts_country <- prev_df %>%
  group_by(country_name) %>%
  summarise(
    n_surveys         = n_distinct(survey_id),
    total_denominator = sum(denominator, na.rm = TRUE),
    .groups = "drop"
  )

summary_table_per_country <- study_counts_country %>%
  full_join(survey_counts_country, by = "country_name") %>%
  full_join(sample_counts_country, by = "country_name") %>%
  mutate(sample_size = n_surveys) %>%
  select(country_name, num_study_ID, num_survey_ID, sample_size, total_denominator)

save_csv(summary_table_per_country,
         file.path(out_dir, "summary_prev_per_country.csv"))

# -- Year-only summaries ---------------------------------------------------------
study_counts_year <- prev_df %>%
  distinct(study_id, year) %>%
  count(year, name = "num_study_ID")

survey_counts_year <- prev_df %>%
  distinct(survey_id, year) %>%
  count(year, name = "num_survey_ID")

sample_counts_year <- prev_df %>%
  group_by(year) %>%
  summarise(
    n_surveys         = n_distinct(survey_id),
    total_denominator = sum(denominator, na.rm = TRUE),
    .groups = "drop"
  )

summary_table_per_year <- study_counts_year %>%
  full_join(survey_counts_year, by = "year") %>%
  full_join(sample_counts_year, by = "year") %>%
  mutate(sample_size = n_surveys) %>%
  select(year, num_study_ID, num_survey_ID, sample_size, total_denominator)

save_csv(summary_table_per_year,
         file.path(out_dir, "summary_prev_per_year.csv"))

# -- Plots -----------------------------------------------------------------------
# Study IDs per year
p_studies_year <- ggplot(study_counts_year, aes(x = factor(year), y = num_study_ID)) +
  geom_col() +
  labs(x = "Year", y = "Number of Study IDs") +
  theme_minimal(base_size = 12)
ggsave(file.path(plot_dir, "studyID_per_year_barplot.png"), p_studies_year, width = 9, height = 5, dpi = 300)

# Survey IDs per year
p_surveys_year <- ggplot(survey_counts_year, aes(x = factor(year), y = num_survey_ID)) +
  geom_col() +
  labs(x = "Year", y = "Number of Survey IDs") +
  theme_minimal(base_size = 12)
ggsave(file.path(plot_dir, "survey_per_year_barplot.png"), p_surveys_year, width = 9, height = 5, dpi = 300)

# Sample size per year × country (facet)
p_sample_country_year <- ggplot(sample_counts_country_year, aes(x = factor(year), y = n_surveys)) +
  geom_col() +
  facet_wrap(~ country_name, scales = "free_y") +
  labs(x = "Year", y = "Sample Size (n_surveys)") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(plot_dir, "sampleSize_per_year_country_facet_barplot.png"),
       p_sample_country_year, width = 14, height = 9, dpi = 300)

# -- Console summary (optional) --------------------------------------------------
cat("\nOverview:\n",
    "- Unique geo sites (lat/long pairs): ", n_sites_geo, "\n",
    "- Countries: ", n_countries, "\n",
    "- Surveys (distinct): ", prev_df %>% distinct(survey_id) %>% nrow(), "\n",
    sep = "")


#----summary k13 in year range----------------------------------------------------

k13_any_site_year <- prev_df %>%
  # keep only K13 mutations; adjust this filter to your naming convention
  filter(grepl("^k13", mutation, ignore.case = TRUE)) %>%
  group_by(year, longitude, latitude) %>%
  summarise(
    numerator = sum(numerator, na.rm = TRUE),
    denominator = max(denominator, na.rm = TRUE),
    prevalence = numerator / denominator *100,
    .groups = "drop"
  ) %>%
  transmute(
    year,
    longitude = longitude,
    latitude  = latitude,
    mutation  = "k13:comb",
    numerator = numerator,
    denominator = denominator,
    prevalence = prevalence
  )

avg_12_13 <- filter(k13_any_site_year, year > 2011, year <2014, prevalence >0) %>% drop_na(prevalence) %>%
  summarize(avg_prevalence = mean(prevalence))

avg_21_23 <- filter(k13_any_site_year, year > 2021, year <2023, prevalence >0) %>% drop_na(prevalence) %>%
  summarize(avg_prevalence = mean(prevalence))




dat_with_k13 <- prev_df %>%
  bind_rows(k13_any_site_year) %>%
  arrange(year, longitude, latitude, mutation)

##############Partner Drug######################################################

# -- I/O paths -------------------------------------------------------------------
in_csv   <- "analysis/data_derived/partner_drug_calc_get_prevalence.csv"
out_dir  <- "analysis/data_derived/prev_summary_information"
plot_dir <- "analysis/plots/prev_summary_information"

# -- Constants -------------------------------------------------------------------
# WHO validated + candidate K13 mutations
all_who_mutations <- c(
  "crt:76:T", "mdr1C:86:N"
)

# -- Load data -------------------------------------------------------------------
prev_df <- readr::read_csv(in_csv, show_col_types = FALSE)

# Quick sanity check for required columns
required_cols <- c(
  "latitude","longitude","country_name","survey_id","study_id",
  "denominator","prevalence","mutation","collection_start","year"
)
missing_cols <- setdiff(required_cols, names(prev_df))
if (length(missing_cols)) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}


# -- Simple counts / overview ----------------------------------------------------
# Unique georeferenced sites (lat/long pairs)
n_sites_geo <- prev_df %>% distinct(latitude, longitude) %>% nrow()
n_countries <- prev_df %>% distinct(country_name) %>% arrange(country_name)
#Check manually for repeat names
write.csv(n_countries,file.path(out_dir,"total_countries.csv"))
n_studies <- prev_df %>% distinct(study_id)

# Per-survey maximum sample size (as in your original code)
num_samples <- prev_df %>%
  group_by(survey_id) %>%
  summarise(max_sample = max(denominator, na.rm = TRUE), .groups = "drop")
message("Total genotyped samples in study: ", sum(num_samples$max_sample))

# Positive-prevalence subset (any > 0)
prev_df_pos <- prev_df %>% filter(prevalence > 0)

