# Load 2025 CCI Survey Data 
# Verified and corrected version

cat("Loading 2025 CCI data...\n")

# Load 2025 survey data
survey_2025_raw <- read_sav("data/spss.sav")

cat("Total observations loaded:", nrow(survey_2025_raw), "\n")
cat("Total variables:", ncol(survey_2025_raw), "\n")

# Check geographic distribution
cat("\nGeographic distribution:\n")
print(table(survey_2025_raw$Geo, useNA = "always"))

# Filter to US National data only (exclude Boston and Denver as requested)
data_2025 <- survey_2025_raw %>% 
  filter(Geo == "US")

cat("US National sample size:", nrow(data_2025), "\n")

# Store full survey data for reference (if needed later)
survey_2025_full <- survey_2025_raw

# Clean up memory
rm(survey_2025_raw)

cat("\nData loaded successfully!\n")