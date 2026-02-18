# Hispanic / Non-Hispanic
hispanic <- read_csv("~/GitHub/cci-2025/Weights/US/hispanic.csv")

# Household income
income <- read_csv("~/GitHub/cci-2025/Weights/US/income.csv")

# Highest level of educational attainment
education <- read_csv("~/GitHub/cci-2025/Weights/US/education.csv")

# Yale SASSY
sassy <- read_csv("~/GitHub/cci-2025/Weights/US/us_sassy.csv")

# State
state <- read_csv("~/GitHub/cci-2025/Weights/US/state.csv")

# Division
division <- read_csv("~/GitHub/cci-2025/Weights/US/division.csv")

# Join to assign `division`
data_2025 <- data_2025 %>%
  left_join(state_division, by = "state")

division %<>% mutate(freq = nrow(data_2025) * prop)
division %<>% select(division, freq)

# Political viewpoint (WVS)
political_view <- read.csv("~/GitHub/cci-2025/Weights/US/us_wvs.csv")

# Hispanic
hispanic %<>% mutate(freq = nrow(data_2025) * prop)
hispanic %<>% select(hispanic, freq) %>% rename(HISPANIC = hispanic)

# SASSY 
sassy %<>% mutate(freq = nrow(data_2025) * prop)
sassy %<>% select(segment, freq)

# Income
income %<>% mutate(freq = nrow(data_2025) * prop) %>% select(income, freq)

# State 
state %<>% mutate(freq = nrow(data_2025) * prop) %>% select(state, freq)

# Remove rows with non-response in the sample
state <- state %>%
  filter(state %in% data_2025$state)

data_2025$state <- droplevels(data_2025$state)
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
    freq = nrow(data_2025) * prop
  ) %>%
  select(education, freq)

# Political view
political_view %<>% mutate(freq = nrow(data_2025) * prop) %>% select(segment, freq) %>% 
  rename(political_viewpoint_wvs = segment)

# Demo
age_sex_race <- read_csv("~/GitHub/cci-2025/Weights/US/age-gender-race.csv")

age_sex_race %<>% mutate(freq = nrow(data_2025) * prop) %>% select(age_group, sex, race, freq)

# Create a demo variable
age_sex_race %<>% mutate(demo = paste(race, "_", sex, "_", age_group)) %>% 
  select(demo, freq)

age_sex_race %<>% mutate(freq = pmax(freq, 1))

# Remove rows with non-response in the sample
data_2025 %<>% mutate(demo = paste(race, "_", sex, "_", age_group))

age_sex_race <- age_sex_race %>%
  filter(demo %in% data_2025$demo)

# Create un-weighted survey object
svy.unweighted <- svydesign(ids = ~ 1, data = data_2025)

# Create survey design object with weights
svy.rake <- rake(design = svy.unweighted,
                 sample.margins = list(~segment,
                                       ~HISPANIC,
                                       ~division,
                                       ~state,
                                       ~income,
                                       ~education,
                                       ~political_viewpoint_wvs),
                 population.margins = list(sassy,
                                           hispanic,
                                           division,
                                           state,
                                           income,
                                           education,
                                           political_view
                                           ))



summary(weights(svy.rake))

# Extract weight values
wt <- data.frame(wt = weights(svy.rake))

# Bind weights with the df, re-order variable
data_2025 <- cbind(data_2025, wt)
