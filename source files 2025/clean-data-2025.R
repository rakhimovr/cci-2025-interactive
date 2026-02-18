# Lucid data cleaning

lucid_2025 <- read_csv("C:/Users/RakhimRakhimov/Documents/GitHub/cci-2025/data/us_lucid.csv")

# Clean
lucid_2025 %<>%
  filter(PID != "test",
         SupplierName != "Z - Test Supplier") %>%
  rename(RID = `Response ID`) %>% 
  mutate(RID = tolower(RID),
         HISPANIC = case_when(HISPANIC_PRECODE == 1 ~ "Non-Hispanic",
                              TRUE ~ "Hispanic")) %>% 
  select(RID, SupplierName, ResponseStatus, AGE_TEXT, GENDER_TEXT, HISPANIC, ETHNICITY_TEXT, REGION_TEXT)

lucid_2025 <- lucid_2025 %>%
  mutate(ETHNICITY = case_when(
    ETHNICITY_TEXT == "White" ~ "White",
    ETHNICITY_TEXT == "Black, or African American" ~ "Black",
    ETHNICITY_TEXT == "American Indian or Alaska Native" ~ "American_Indian",
    grepl("^Asian", ETHNICITY_TEXT) ~ "Asian",
    grepl("^Pacific Islander", ETHNICITY_TEXT) ~ "Native_Hawaiian",
    ETHNICITY_TEXT == "Some other race" ~ "Other",
    ETHNICITY_TEXT == "Prefer not to answer" ~ "Prefers_not_to_say",
    TRUE ~ NA_character_
  )) %>% select(-ETHNICITY_TEXT)

# Merge Lucid and survey data
data_2025 <- merge(x = data_2025, y = lucid_2025, by = "RID", all =  TRUE)

# Demographic variable cleaning

# Clean demo variables
data_2025 %<>%
  rename(race = ETHNICITY) %>% 
  mutate(political_viewpoint_wvs = as.numeric(political_viewpoint_wvs),
         
         state = as_factor(state),
         state = as.character(state),
         state = case_when(state == "Washington, D C" ~ "District of Columbia",
                           TRUE ~ state),
         state = as_factor(state),
         
         income = as_factor(income),
         income = as.character(income),
         
         income = case_when(
           income %in% c("$150,000 to $199,999", "$200,000 to $249,999", "$250,000 and over") ~ "$150,000 and over",
           TRUE ~ as.character(income)),
         
         education = as_factor(education),
         education = as.character(education),
         education = case_when(
           education %in% c("No formal education", "1st, 2nd, 3rd, or 4th grade", "5th or 6th grade", "7th or 8th grade", "9th grade", "10th grade", "11th grade", "12th grade, no diploma") ~ "Less than high school",
           education %in% c("High school graduate – high school diploma or the equivalent (GED)") ~ "High school graduate",
           TRUE ~ as.character(education)))

# Clean and QC 2025 CCI Data

cat("Initial sample size:", nrow(data_2025), "\n")

# =========================================================================
# 1. COMPLETION STATUS
# =========================================================================

# Filter for complete responses only
data_2025 <- data_2025 %>% 
  filter(Vstatus == "Complete")

cat("After completion filter:", nrow(data_2025), "\n")

# =========================================================================
# 2. ATTENTION CHECKS - UPDATE AFTER VERIFICATION
# =========================================================================

cat("Applying attention check filters...\n")

att_0_correct <- 12772
att_2_correct <- 3  
att_3_correct <- 80

data_2025 <- data_2025 %>% 
  filter(att_check_0 == att_0_correct,
         att_check_2 == att_2_correct,
         att_check_3 == att_3_correct)

cat("After attention check filters:", nrow(data_2025), "\n")


# =========================================================================
# 3. SURVEY TIMING / SPEEDING CHECK
# =========================================================================

# Survey time is in SECONDS (confirmed from verification)
# Flag speeders based on median survey time (1/3 of median threshold)

if("survey_time" %in% names(data_2025)) {
  # Convert survey_time to numeric (it's currently stored as character)
  data_2025$survey_time_numeric <- as.numeric(data_2025$survey_time)
  
  # Check conversion success
  converted_count <- sum(!is.na(data_2025$survey_time_numeric))
  cat("Survey time conversion: converted", converted_count, "of", nrow(data_2025), "values to numeric\n")
  
  # Calculate key statistics
  median_time_seconds <- median(data_2025$survey_time_numeric, na.rm = TRUE)
  median_time_minutes <- median_time_seconds / 60
  
  # Calculate percentiles for survey completion times
  percentiles <- c(5, 10, 25, 33.33, 50, 66.67, 75, 90, 95, 99)
  time_percentiles <- quantile(data_2025$survey_time_numeric, probs = percentiles/100, na.rm = TRUE)
  
  # Set thresholds
  speeding_threshold <- median_time_seconds / 3
  speeding_threshold_minutes <- speeding_threshold / 60
  half_median_threshold <- median_time_seconds / 2
  half_median_threshold_minutes <- half_median_threshold / 60
  
  cat("Survey time analysis:\n")
  cat("  Median survey time:", round(median_time_minutes, 1), "minutes (", round(median_time_seconds, 0), "seconds)\n")
  cat("  Speeding threshold (1/3 of median):", round(speeding_threshold_minutes, 1), "minutes (", round(speeding_threshold, 0), "seconds)\n")
  cat("  Half median threshold (1/2 of median):", round(half_median_threshold_minutes, 1), "minutes (", round(half_median_threshold, 0), "seconds)\n")
  
  cat("\nKey percentiles of survey completion times:\n")
  for(i in 1:length(percentiles)) {
    minutes <- time_percentiles[i] / 60
    cat(sprintf("  %2.1f%%: %5.1f minutes (%4.0f seconds)\n", 
                percentiles[i], minutes, time_percentiles[i]))
  }
  
  # Calculate counts and percentages within key percentile ranges
  total_n <- sum(!is.na(data_2025$survey_time_numeric))
  
  cat("\nDistribution of respondents within key percentile ranges:\n")
  
  # Calculate range counts
  very_fast <- sum(data_2025$survey_time_numeric <= time_percentiles[1], na.rm = TRUE)  # <=5th percentile
  fast <- sum(data_2025$survey_time_numeric > time_percentiles[1] & 
                data_2025$survey_time_numeric <= time_percentiles[3], na.rm = TRUE)  # 5th-25th
  normal_low <- sum(data_2025$survey_time_numeric > time_percentiles[3] & 
                      data_2025$survey_time_numeric <= time_percentiles[5], na.rm = TRUE)  # 25th-50th
  normal_high <- sum(data_2025$survey_time_numeric > time_percentiles[5] & 
                       data_2025$survey_time_numeric <= time_percentiles[7], na.rm = TRUE)  # 50th-75th
  slow <- sum(data_2025$survey_time_numeric > time_percentiles[7] & 
                data_2025$survey_time_numeric <= time_percentiles[9], na.rm = TRUE)  # 75th-95th
  very_slow <- sum(data_2025$survey_time_numeric > time_percentiles[9], na.rm = TRUE)  # >95th percentile
  
  cat(sprintf("  Very fast (???5th percentile): %4d respondents (%4.1f%%)\n", very_fast, 100*very_fast/total_n))
  cat(sprintf("  Fast (5th-25th percentile): %4d respondents (%4.1f%%)\n", fast, 100*fast/total_n))
  cat(sprintf("  Normal-low (25th-50th percentile): %4d respondents (%4.1f%%)\n", normal_low, 100*normal_low/total_n))
  cat(sprintf("  Normal-high (50th-75th percentile): %4d respondents (%4.1f%%)\n", normal_high, 100*normal_high/total_n))
  cat(sprintf("  Slow (75th-95th percentile): %4d respondents (%4.1f%%)\n", slow, 100*slow/total_n))
  cat(sprintf("  Very slow (>95th percentile): %4d respondents (%4.1f%%)\n", very_slow, 100*very_slow/total_n))
  
  # Flag speeders (less than 1/3 of median time) - DO NOT REMOVE
  data_2025$speeding_flag <- ifelse(data_2025$survey_time_numeric < speeding_threshold, 1, 0)
  data_2025$speeding_flag[is.na(data_2025$survey_time_numeric)] <- NA  # Handle missing values
  
  speeders_third <- sum(data_2025$speeding_flag, na.rm = TRUE)
  speeders_third_rate <- round(100 * speeders_third / nrow(data_2025), 1)
  
  # Count respondents under 1/2 median (for curiosity - no flag created)
  speeders_half <- sum(data_2025$survey_time_numeric < half_median_threshold, na.rm = TRUE)
  speeders_half_rate <- round(100 * speeders_half / nrow(data_2025), 1)
  
  cat("\nSpeeding analysis:\n")
  cat("  Respondents under 1/3 median (flagged):", speeders_third, "out of", nrow(data_2025), "(", speeders_third_rate, "%)\n")
  cat("  Respondents under 1/2 median (curiosity):", speeders_half, "out of", nrow(data_2025), "(", speeders_half_rate, "%)\n")
  
  # Create enhanced histogram of survey response times
  library(ggplot2)
  
  # Prepare data for plotting (convert to minutes)
  survey_time_minutes <- data_2025$survey_time_numeric / 60
  plot_data <- data.frame(time_minutes = survey_time_minutes)
  plot_data <- plot_data[!is.na(plot_data$time_minutes), , drop = FALSE]
  
  # Calculate counts under each threshold for annotations
  n_total <- nrow(plot_data)
  n_under_third <- sum(plot_data$time_minutes < speeding_threshold_minutes)
  n_under_half <- sum(plot_data$time_minutes < half_median_threshold_minutes)
  n_under_median <- sum(plot_data$time_minutes < median_time_minutes)
  
  
  # Create main histogram (capped at 90th percentile to handle long tail)
  plot_cap_90 <- time_percentiles[8] / 60  # 90th percentile
  
  # Calculate histogram data manually for proper y-axis scaling
  filtered_data <- plot_data$time_minutes[plot_data$time_minutes <= plot_cap_90 & !is.na(plot_data$time_minutes)]
  hist_data <- hist(filtered_data, breaks = 40, plot = FALSE)
  max_count <- max(hist_data$counts)
  
  p <- ggplot(plot_data, aes(x = time_minutes)) +
    geom_histogram(bins = 40, fill = "lightblue", alpha = 0.7, color = "black", size = 0.3) +
    
    # Add vertical lines for key thresholds
    geom_vline(aes(xintercept = median_time_minutes), 
               color = "red", linetype = "solid", size = 1.2) +
    geom_vline(aes(xintercept = speeding_threshold_minutes), 
               color = "orange", linetype = "dashed", size = 1.1) +
    geom_vline(aes(xintercept = half_median_threshold_minutes), 
               color = "blue", linetype = "dotted", size = 1.1) +
    
    # Add count annotations near each line (using fixed y positions)
    annotate("text", x = speeding_threshold_minutes, y = 135,
             label = paste0("n=", n_under_third, "\n(", round(100*n_under_third/n_total, 1), "%)"),
             color = "orange", size = 3.5, hjust = 1.1, fontface = "bold") +
    
    annotate("text", x = half_median_threshold_minutes, y = 125,
             label = paste0("n=", n_under_half, "\n(", round(100*n_under_half/n_total, 1), "%)"),
             color = "blue", size = 3.5, hjust = 1.1, fontface = "bold") +
    
    annotate("text", x = median_time_minutes, y = 115,
             label = paste0("n=", n_under_median, "\n(", round(100*n_under_median/n_total, 1), "%)"),
             color = "red", size = 3.5, hjust = -0.1, fontface = "bold") +
    
    # Add labels and formatting
    labs(title = "Distribution of Survey Completion Times",
         subtitle = paste("Showing data up to 90th percentile |",
                          "Orange:", round(speeding_threshold_minutes, 1), "min |",
                          "Blue:", round(half_median_threshold_minutes, 1), "min |",
                          "Red:", round(median_time_minutes, 1), "min"),
         x = "Survey Time (minutes)",
         y = "Number of Respondents",
         caption = paste("Red = Median | Orange = 1/3 Median (speeding threshold) | Blue = 1/2 Median\n",
                         "Total respondents:", n_total)) +
    
    # Set x-axis limits and breaks
    scale_x_continuous(limits = c(0, plot_cap_90), 
                       breaks = seq(0, ceiling(plot_cap_90), by = 2.5),
                       minor_breaks = seq(0, ceiling(plot_cap_90), by = 1.25)) +
    
    # Set appropriate y-axis breaks - capped at 150
    scale_y_continuous(breaks = seq(0, 150, by = 25),
                       limits = c(0, 150)) +
    
    # Theme
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 11),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 11),
          panel.grid.minor = element_line(color = "gray95", size = 0.5))
  
  # Display the plot
  print(p)
  
  # Save the plot
  ggsave("survey_time_distribution.png", p, width = 12, height = 7, dpi = 300)
  
  cat("\nHistogram saved as 'survey_time_distribution.png'\n")
  
} else {
  cat("survey_time variable not found - skipping speeding check\n")
}

# =========================================================================
# 5. AGE FILTER AND DEMOGRAPHIC PREPARATION
# =========================================================================

# Create age variable and filter to adults
data_2025$age <- 2025 - data_2025$birth_year
data_2025 <- data_2025 %>% filter(age >= 18, age <= 100)

cat("After age filter (18-100):", nrow(data_2025), "\n")

# Create age groups for weighting (matching census categories)
age_breaks <- c(18, 25, 35, 45, 55, 100)
age_labels <- c("18-24", "25-34", "35-44", "45-54", "55+")
data_2025$age_group <- cut(data_2025$age, breaks = age_breaks, right = FALSE, labels = age_labels)

# Clean gender variable
data_2025 <- data_2025 %>% rename(sex = GENDER_TEXT)


# =========================================================================
# QUALITY SUMMARY
# =========================================================================

cat("\n=== DATA CLEANING SUMMARY ===\n")
cat("Final sample size:", nrow(data_2025), "\n")
cat("Variables available:", ncol(data_2025), "\n")
cat("Age range:", min(data_2025$age, na.rm=TRUE), "-", max(data_2025$age, na.rm=TRUE), "\n")

cat("\nFinal sample characteristics:\n")
cat("Gender distribution:\n")
print(table(data_2025$sex, useNA = "always"))
cat("Race distribution:\n")
print(table(data_2025$race, useNA = "always"))
cat("Hispanic distribution:\n")
print(table(data_2025$HISPANIC, useNA = "always"))
cat("Age group distribution:\n")
print(table(data_2025$age_group, useNA = "always"))


data_2025 <- data_2025 %>% 
  filter(!is.na(HISPANIC))