# Lucid data cleaning

data_2025 <- survey_2025_full %>% 
  filter(Geo == "Boston")

lucid_2025 <- read_csv("C:/Users/RakhimRakhimov/Documents/GitHub/cci-2025/data/boston_lucid.csv")

# Clean
lucid_2025 %<>%
  filter(PID != "test",
         SupplierName != "Z - Test Supplier") %>%
  rename(RID = `Response ID`) %>% 
  mutate(RID = tolower(RID),
         HISPANIC = case_when(HISPANIC_PRECODE == 1 ~ "Non-Hispanic",
                              TRUE ~ "Hispanic")) %>% 
  select(RID, SupplierName, ResponseStatus, AGE_TEXT, GENDER_TEXT, HISPANIC, ETHNICITY_TEXT)

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
           education %in% c("High school graduate ??? high school diploma or the equivalent (GED)") ~ "High school graduate",
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

data_2025 <- data_2025 %>% 
  filter(!is.na(race))