state_division <- tribble(
  ~state,              ~division,
  # New England
  "Connecticut",       "New England",
  "Maine",             "New England",
  "Massachusetts",     "New England",
  "New Hampshire",     "New England",
  "Rhode Island",      "New England",
  "Vermont",           "New England",
  # Middle Atlantic
  "New Jersey",        "Middle Atlantic",
  "New York",          "Middle Atlantic",
  "Pennsylvania",      "Middle Atlantic",
  # East North Central
  "Illinois",          "East North Central",
  "Indiana",           "East North Central",
  "Michigan",          "East North Central",
  "Ohio",              "East North Central",
  "Wisconsin",         "East North Central",
  # West North Central
  "Iowa",              "West North Central",
  "Kansas",            "West North Central",
  "Minnesota",         "West North Central",
  "Missouri",          "West North Central",
  "Nebraska",          "West North Central",
  "North Dakota",      "West North Central",
  "South Dakota",      "West North Central",
  # South Atlantic (includes DC)
  "Delaware",          "South Atlantic",
  "District of Columbia","South Atlantic",
  "Florida",           "South Atlantic",
  "Georgia",           "South Atlantic",
  "Maryland",          "South Atlantic",
  "North Carolina",    "South Atlantic",
  "South Carolina",    "South Atlantic",
  "Virginia",          "South Atlantic",
  "West Virginia",     "South Atlantic",
  # East South Central
  "Alabama",           "East South Central",
  "Kentucky",          "East South Central",
  "Mississippi",       "East South Central",
  "Tennessee",         "East South Central",
  # West South Central
  "Arkansas",          "West South Central",
  "Louisiana",         "West South Central",
  "Oklahoma",          "West South Central",
  "Texas",             "West South Central",
  # Mountain
  "Arizona",           "Mountain",
  "Colorado",          "Mountain",
  "Idaho",             "Mountain",
  "Montana",           "Mountain",
  "Nevada",            "Mountain",
  "New Mexico",        "Mountain",
  "Utah",              "Mountain",
  "Wyoming",           "Mountain",
  # Pacific
  "Alaska",            "Pacific",
  "California",        "Pacific",
  "Hawaii",            "Pacific",
  "Oregon",            "Pacific",
  "Washington",        "Pacific"
)

# Sanity check: make sure we have exactly 51 entries (50 states + DC)
nrow(state_division) # should be 51

# Hispanic / Non-Hispanic
hispanic <- read_csv("weights/US/hispanic.csv")

# Household income
income <- read_csv("weights/US/income.csv")

# Highest level of educational attainment
education <- read_csv("weights/US/education.csv")

# Yale SASSY
sassy <- read_csv("weights/US/us_sassy.csv")

# State
state <- read_csv("weights/US/state.csv")

# Division
division <- read_csv("weights/US/division.csv")

# Join to assign `division`
data_2025 <- data_2025 %>%
  left_join(state_division, by = "state")

division %<>% mutate(Freq = nrow(data_2025) * prop)
division %<>% select(division, Freq)

# Political viewpoint (WVS)
political_view <- read.csv("weights/US/us_wvs.csv")

# Hispanic
hispanic %<>% mutate(Freq = nrow(data_2025) * prop)
hispanic %<>% select(hispanic, Freq) %>% rename(HISPANIC = hispanic)

# SASSY 
sassy %<>% mutate(Freq = nrow(data_2025) * prop)
sassy %<>% select(segment, Freq)

# Income
income %<>% mutate(Freq = nrow(data_2025) * prop) %>% select(income, Freq)

# Region
# region %<>% mutate(Freq = nrow(data_2025) * prop) %>% select(region, Freq)

# State 
state %<>% mutate(Freq = nrow(data_2025) * prop) %>% select(state, Freq)

# Remove rows with non-response in the sample
state <- state %>%
  filter(state %in% data_2025$state)

#data_2025$state <- droplevels(data_2025$state)
data_2025$state <- as.character(data_2025$state)

# Education
education_clean <- education %>%  
  mutate(seg_norm = str_squish(str_to_lower(segment))) %>%
  mutate(education = case_when(
    str_detect(seg_norm, "no diploma") ~ "Less than high school",
    str_detect(seg_norm, "^less than|1st,?\\s*2nd|5th or 6th|7th or 8th|\\b9th grade\\b|\\b10th grade\\b|\\b11th grade\\b|\\b12th grade\\b") ~ "Less than high school",
    str_detect(seg_norm, "high school grad") ~ "High school graduate",
    str_detect(seg_norm, "some college") ~ "Some college, no degree",
    str_detect(seg_norm, "^associate|associate deg") ~ "Associate’s degree",
    str_detect(seg_norm, "^bachelor|bachelor's") ~ "Bachelor’s degree",
    str_detect(seg_norm, "^master|master's") ~ "Master’s degree",
    str_detect(seg_norm, "professional school|doctorate|phd|edd|md|dds|dvm") ~ "Professional or doctorate degree",
    TRUE ~ NA_character_
  )) %>%
  group_by(education) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    education = factor(
      education,
      levels = c(
        "Less than high school",
        "High school graduate",
        "Some college, no degree",
        "Associate’s degree",
        "Bachelor’s degree",
        "Master’s degree",
        "Professional or doctorate degree"
      )
    )
  ) %>%
  arrange(education)

education = education_clean

education %<>%
  mutate(
    prop = count / sum(count),
    Freq = nrow(data_2025) * prop
  ) %>%
  select(education, Freq)

# Political view
political_view %<>% mutate(Freq = nrow(data_2025) * prop) %>% select(segment, Freq) %>% 
  rename(political_viewpoint_wvs = segment)

# Demo
age_sex_race <- read_csv("weights/US/age-gender-race.csv")

age_sex_race %<>% mutate(Freq = nrow(data_2025) * prop) %>% select(age_group, sex, race, Freq)

# Create a demo variable
age_sex_race %<>% mutate(demo = paste(race, "_", sex, "_", age_group)) %>% 
  select(demo, Freq)

age_sex_race %<>% mutate(Freq = pmax(Freq, 1))

# Remove rows with non-response in the sample
data_2025 %<>% mutate(demo = paste(race, "_", sex, "_", age_group))

age_sex_race <- age_sex_race %>%
  filter(demo %in% data_2025$demo)

rm(education_clean)

# Stepwise Raking Approach

# Start with unweighted design
svy.current <- svydesign(ids = ~ 1, data = data_2025)

# Step 1: Start with the most important/stable demographics
print("Step 1: Age/Sex/Race demographics...")
svy.step1 <- rake(design = svy.current,
                  sample.margins = list(~demo),
                  population.margins = list(age_sex_race),
                  control = list(maxit = 100, epsilon = 1e-7, verbose = TRUE))

# Step 2: Add geographic variables
print("Step 2: Adding geographic variables...")
svy.step2 <- rake(design = svy.step1,
                  sample.margins = list(~division, ~state),
                  population.margins = list(division, state),
                  control = list(maxit = 100, epsilon = 1e-7, verbose = TRUE))

# Step 3: Add socioeconomic variables
print("Step 3: Adding socioeconomic variables...")
svy.step3 <- rake(design = svy.step2,
                  sample.margins = list(~income, ~education),
                  population.margins = list(income, education),
                  control = list(maxit = 100, epsilon = 1e-7, verbose = TRUE))

# Step 4: Add ethnicity
print("Step 4: Adding Hispanic variable...")
svy.step4 <- rake(design = svy.step3,
                  sample.margins = list(~HISPANIC),
                  population.margins = list(hispanic),
                  control = list(maxit = 100, epsilon = 1e-7, verbose = TRUE))

# Step 5: Add political and psychographic variables
print("Step 5: Adding political and SASSY segments...")
svy.rake <- rake(design = svy.step4,
                 sample.margins = list(~political_viewpoint_wvs, ~segment),
                 population.margins = list(political_view, sassy),
                 control = list(maxit = 100, epsilon = 1e-7, verbose = TRUE))

# Check for extreme weights
extreme_weights <- weights(svy.rake)[weights(svy.rake) > 5 | weights(svy.rake) < 0.2]
if(length(extreme_weights) > 0) {
  print(paste("Warning:", length(extreme_weights), "extreme weights detected"))
}

# Check current weights
print("=== BEFORE TRIMMING ===")
summary(weights(svy.rake))

# Use the built-in trimWeights() function
# Syntax: trimWeights(design, lower=, upper=, strict=FALSE)
svy.final <- trimWeights(svy.rake, lower = 0.2, upper = 5.0)

print("=== AFTER TRIMMING ===")
summary(weights(svy.final))

print("=== COMPARISON ===")
print("Original extreme weights:")
print(sum(weights(svy.rake) < 0.1 | weights(svy.rake) > 10))

print("Trimmed extreme weights:")
print(sum(weights(svy.final) < 0.1 | weights(svy.final) > 10))

# Extract weight values
wt <- data.frame(wt = weights(svy.final))

# Bind weights with the df, re-order variable
data_2025 <- cbind(data_2025, wt)