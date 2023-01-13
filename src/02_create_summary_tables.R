# Load the required R libraries -------------------------------------------

# Package names
packages <- c("dplyr", "data.table", "tidyr", "stringr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Disable scientific notation of numbers
options(scipen = 999)

# Specify directory
merged_dir <- "output"
merged_df <- "merged_dataset.rds"
summary_dir <- "summary_tables"

# Specify currency rate to CZK (7th December 2022) for conversion of ad amounts
usd_rate <- 23.105
eur_rate <- 24.325
pln_rate <- 5.176
vnd_rate <- 0.000951

# # Specify the end date for the data collection.
ads_create_end_date <- as.Date("2023-01-27")
ads_display_end_date <- as.Date("2023-01-27")

# Read in the dataset with Ads information created previously -------------
full_ads_table <- readRDS(file.path(merged_dir, merged_df)) %>%
  filter(ad_creation_time <= ads_create_end_date & ad_delivery_start_time <= ads_display_end_date) %>%
  mutate(page_name = replace_na(page_name, "unknown_page"))

# Check if output directory exists. If it does not, create it.
if (!dir.exists(file.path(merged_dir, summary_dir))) {
  dir.create(file.path(merged_dir, summary_dir))
} else {
  print("Output directory already exists")
}

# Create summary tables and save them -------------------------------------
# Creating a summary table focused on the detailed aspects of the advertising
# Percentage figures rounded
ad_summary <- full_ads_table %>%
  arrange(ad_creation_time) %>%
  # Select columns of interest
  transmute(
    id,
    ad_creation_time,
    ad_creative_bodies,
    ad_creative_link_captions,
    ad_creative_link_descriptions,
    ad_creative_link_titles,
    ad_delivery_start_time,
    ad_delivery_stop_time,
    ad_snapshot_url,
    bylines,
    currency,
    languages,
    page_id,
    page_name,
    publisher_platforms,
    estimated_audience_size_lower,
    estimated_audience_size_upper,
    # A small minority of ads is not in Czech currency and requires conversion
    # Also, a fraction of ads tend to have missing information regarding the
    # upper boundary of spending and impressions. We replace NAs with lower
    # boundary estimate for calculation purposes.
    impressions_lower,
    impressions_upper = case_when(
      is.na(impressions_upper) ~ impressions_lower, # If NA, use lower boundary
      TRUE ~ as.numeric(impressions_upper) # Ignore the rest. Specify type explicitly.
    ),
    spend_lower = round(
      case_when(
        currency == "CZK" ~ spend_lower,
        currency == "USD" ~ spend_lower * usd_rate,
        currency == "EUR" ~ spend_lower * eur_rate,
        currency == "PLN" ~ spend_lower * pln_rate,
        currency == "VND" ~ spend_lower * vnd_rate
      )),
    spend_upper = round(
      case_when(
        is.na(spend_upper) ~ as.numeric(spend_lower),
        currency == "CZK" ~ spend_upper,
        currency == "USD" ~ spend_upper * usd_rate,
        currency == "EUR" ~ spend_upper * eur_rate,
        currency == "PLN" ~ spend_upper * pln_rate,
        currency == "VND" ~ spend_upper * vnd_rate
      ))) %>%
  # Calculate group statistics for each spender
  group_by(page_id) %>%
  summarise(
    page_name = last(page_name), # Page name of spender - select last in case the same page_id had different page names over time
    total_ads = n(), # Total ads of given spender
    unique_ads = n_distinct(ad_creative_bodies, na.rm = TRUE), # The number of non-duplicate texts of ads
    percent_unique = round(unique_ads / total_ads, digits = 3), # Proportion unique
    avg_words = round(mean(str_count(ad_creative_bodies, "\\w+"), na.rm = TRUE)),
    avg_spend = round((sum(spend_lower, na.rm = TRUE) + sum(spend_upper, na.rm = TRUE)) / 2),
    per_ad_avg_spend = round(avg_spend / total_ads),
    # Calculate the average total impressions as a mean between lower and upper estimate
    total_avg_impressions = round((sum(impressions_lower, na.rm = TRUE) + sum(impressions_upper, na.rm = TRUE)) / 2),
    per_ad_avg_impression = round(total_avg_impressions / total_ads),
    # Calculate the hypothetical audience size, which comply with targeting criteria
    # This gives the information about how widely/narrowly are the ads targeted
    # We are only using the lower boundary, as there tends to be significant missingness
    # with the upper boundary information
    avg_ad_min_audience = round(mean(estimated_audience_size_lower, na.rm = TRUE)),
    # How long did the ad run
    avg_ad_runtime = round(mean(ad_delivery_stop_time - ad_delivery_start_time, na.rm = TRUE), digits = 1)
  ) %>%
  ungroup()

# Creating a summary table focused on the demographic aspects
# Percentage figures rounded to 3 decimal places
demographic_summary <- full_ads_table %>%
  arrange(ad_creation_time) %>%
  # Select and rename columns of interest
  transmute(
    page_name,
    page_id,
    female_13_17 = `female_13-17`,
    female_18_24 = `female_18-24`,
    female_25_34 = `female_25-34`,
    female_35_44 = `female_35-44`,
    female_45_54 = `female_45-54`,
    female_55_64 = `female_55-64`,
    female_65_plus = `female_65+`,
    male_13_17 = `male_13-17`,
    male_18_24 = `male_18-24`,
    male_25_34 = `male_25-34`,
    male_35_44 = `male_35-44`,
    male_45_54 = `male_45-54`,
    male_55_64 = `male_55-64`,
    male_65_plus = `male_65+`
  ) %>%
  # Replace NAs with 0s, sum gender stats for each ad.
  mutate(across(where(is.numeric), ~replace_na(.x, 0)),
         total_male = rowSums(across(starts_with("male"))),
         total_female = rowSums(across(starts_with("female"))),
         total_13_17 = rowSums(across(contains("13_17"))),
         total_18_24 = rowSums(across(contains("18_24"))),
         total_25_34 = rowSums(across(contains("25_34"))),
         total_35_44 = rowSums(across(contains("35_44"))),
         total_45_54 = rowSums(across(contains("45_54"))),
         total_55_64 = rowSums(across(contains("55_64"))),
         total_65_plus = rowSums(across(contains("65_plus")))
  ) %>%
  # Eliminate ads which do not have demographic information
  filter(total_male > 0 | total_female > 0) %>%
  group_by(page_id) %>%
  summarise(
    page_name = last(page_name),
    # total_ads = n(),
    avg_female_13_17 = mean(female_13_17),
    avg_female_18_24 = mean(female_18_24),
    avg_female_25_34 = mean(female_25_34),
    avg_female_35_44 = mean(female_35_44),
    avg_female_45_54 = mean(female_45_54),
    avg_female_55_64 = mean(female_55_64),
    avg_female_65_plus = mean(female_65_plus),
    avg_male_13_17 = mean(male_13_17),
    avg_male_18_24 = mean(male_18_24),
    avg_male_25_34 = mean(male_25_34),
    avg_male_35_44 = mean(male_35_44),
    avg_male_45_54 = mean(male_45_54),
    avg_male_55_64 = mean(male_55_64),
    avg_male_65_plus = mean(male_65_plus),
    avg_13_17 = mean(total_13_17),
    avg_18_24 = mean(total_18_24),
    avg_25_34 = mean(total_25_34),
    avg_35_44 = mean(total_35_44),
    avg_45_54 = mean(total_45_54),
    avg_55_64 = mean(total_55_64),
    avg_65_plus = mean(total_65_plus),
    avg_female = mean(total_female),
    avg_male = mean(total_male)
  ) %>%
  mutate(across(where(is.double), round, digits = 3)) %>%
  ungroup()

# Creating a summary table focused on the regional aspects
# We rename the Czech regions using Czech Statistical Office abbreviations
# Percentage figures rounded to 3 decimal places
region_summary <- full_ads_table %>%
  arrange(ad_creation_time) %>%
  transmute(
    page_name,
    page_id,
    reg_pha = `Prague`,
    reg_stc = `Central Bohemian Region`,
    reg_jhc = `South Bohemian Region`,
    reg_plk = `Plzeň Region`,
    reg_kvk = `Karlovy Vary Region`,
    reg_ulk = `Ústí nad Labem Region`,
    reg_lbk = `Liberec Region`,
    reg_hkk = `Hradec Králové Region`,
    reg_pak = `Pardubice Region`,
    reg_vys = `Vysočina Region`,
    reg_jhm = `South Moravian Region`,
    reg_olk = `Olomouc Region`,
    reg_msk = `Moravian-Silesian Region`,
    reg_zlk = `Zlín Region`) %>%
  # Replace NAs with 0s, sum region stats for each ad.
  mutate(across(where(is.numeric), ~replace_na(.x, 0)),
         total_regions = rowSums(across(starts_with("reg")))) %>%
  # Eliminate ads which do not have region information
  filter(total_regions > 0) %>%
  group_by(page_id) %>%
  summarise(
    page_name = last(page_name),
    # total_ads = n(),
    avg_pha = mean(reg_pha),
    avg_stc = mean(reg_stc),
    avg_jhc = mean(reg_jhc),
    avg_plk = mean(reg_plk),
    avg_kvk = mean(reg_kvk),
    avg_ulk = mean(reg_ulk),
    avg_lbk = mean(reg_lbk),
    avg_hkk = mean(reg_hkk),
    avg_pak = mean(reg_pak),
    avg_vys = mean(reg_vys),
    avg_jhm = mean(reg_jhm),
    avg_olk = mean(reg_olk),
    avg_msk = mean(reg_msk),
    avg_zlk = mean(reg_zlk)
  ) %>%
  mutate(across(where(is.double), round, digits = 3)) %>%
  ungroup()

# Creating a summary table with cumulative spending over time --------
time_summary <- full_ads_table %>%
  arrange(ad_creation_time) %>%
  transmute(ad_creation_time,
            page_name,
            page_id,
            avg_spend = (spend_lower + spend_upper) / 2) %>%
  group_by(page_id) %>%
  mutate(page_name = last(page_name),
         cumulative_spend = cumsum(avg_spend)) %>%
  ungroup()

# Combine ad, demographic and region table to one - "merged_summary". -------
merged_summary <- ad_summary %>%
  inner_join(demographic_summary, by = c("page_name", "page_id")) %>%
  inner_join(region_summary, by = c("page_name", "page_id"))

# Calculate top spenders for each of the regions --------------------------
regional_spenders <- merged_summary %>%
  select(
      page_name,
      page_id,
      avg_spend,
      avg_pha,
      avg_stc,
      avg_jhc,
      avg_plk,
      avg_kvk,
      avg_ulk,
      avg_lbk,
      avg_hkk,
      avg_pak,
      avg_vys,
      avg_jhm,
      avg_olk,
      avg_msk,
      avg_zlk
  ) %>%
  pivot_longer(-c("page_name", "page_id", "avg_spend"), names_to = "region", values_to = "spend_proportion") %>%
  transmute(page_name,
            page_id,
            region,
            regional_spend = round(avg_spend * spend_proportion))

# Save summarized datasets ------------------------------------------------
saveRDS(time_summary, file.path(merged_dir, summary_dir, "time_summary.rds"))
fwrite(merged_summary, file.path(merged_dir, summary_dir, "merged_summary.csv"))
saveRDS(merged_summary, file.path(merged_dir, summary_dir, "merged_summary.rds"))
fwrite(regional_spenders, file.path(merged_dir, summary_dir, "regional_spenders.csv"))
saveRDS(regional_spenders, file.path(merged_dir, summary_dir, "regional_spenders.rds"))

