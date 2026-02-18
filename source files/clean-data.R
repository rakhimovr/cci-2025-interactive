# Merge and clean Lucid data

# List of data frames
list_of_dfs <- list(lucid_2024_1, lucid_2024_2, lucid_2024_boston_1, lucid_2024_boston_2, lucid_2024_denver_1, lucid_2024_denver_2)

# Convert all columns in all data frames to character
list_of_dfs <- map(list_of_dfs, ~ .x %>% mutate(across(everything(), as.character)))

# Combine data frames
lucid_2024 <- bind_rows(list_of_dfs)

# Clean
lucid_2024 %<>%
  filter(PID != "test",
         SupplierName != "Z - Test Supplier") %>%
  rename(RID = `Response ID`) %>% 
  mutate(RID = tolower(RID),
         REGION = case_when(REGION == 1 ~ "Northeast",
                            REGION == 2 ~ "Midwest",
                            REGION == 3 ~ "South",
                            REGION == 4 ~ "West",
                            TRUE ~ REGION),
         ETHNICITY = case_when(ETHNICITY == 1 ~ "White",
                               ETHNICITY == 2 ~ "Black",
                               ETHNICITY == 3 ~ "American_Indian",
                               ETHNICITY == 4 ~ "Asian",
                               ETHNICITY == 5 ~ "Asian",
                               ETHNICITY == 6 ~ "Asian",
                               ETHNICITY == 7 ~ "Asian",
                               ETHNICITY == 8 ~ "Asian",
                               ETHNICITY == 9 ~ "Asian",
                               ETHNICITY == 10 ~ "Asian",
                               ETHNICITY == 11 ~ "Native_Hawaiian",
                               ETHNICITY == 12 ~ "Native_Hawaiian",
                               ETHNICITY == 13 ~ "Native_Hawaiian",
                               ETHNICITY == 14 ~ "Native_Hawaiian",
                               ETHNICITY == 15 ~ "Other",
                               ETHNICITY == 16 ~ "Prefers_not_to_say",
                               TRUE ~ ETHNICITY),
         
         HISPANIC = case_when(HISPANIC == 1 ~ "Non-Hispanic",
                              TRUE ~ "Hispanic")) %>% 
  select(RID, SurveyName, SupplierName, ResponseStatus, REGION:ETHNICITY)

  # Merge Lucid and survey data
data_2024 <- merge(x = survey_2024, y = lucid_2024, by = "RID", all =  TRUE)

  # Clean combined data
data_2024 %<>% mutate_at(vars(starts_with("att_")), as.character) %>%
  filter(att_check_2 == 3 & att_check_3 == "80") %>% # Passed all last 2 attention checks
  filter(Vstatus == "Complete") %>% # Registered as complete on Alchemer
  drop_na(RID) %>% 
  filter(RID != "") # Require a unique ID
  
  # Remove nonsensical age
data_2024 %<>% 
  mutate(age = as.integer(format(Sys.Date(), "%Y")) - birth_year) %>% 
  filter(age >= 18, age < 100)

# Create age brackets based on Lucid quotas
age_breaks <- c(18, 25, 35, 45, 55, 100)

age_labels <- c("18-24", "25-34", "35-44", "45-54", "55+")

# Add age brackets to the main df
setDT(data_2024)[ , age_group := cut(age, 
                                breaks = age_breaks, 
                                right = FALSE, 
                                labels = age_labels)]

  # Clean variables
data_2024 %<>%
  rename(race_pacific = race_pacifc) %>% 
  mutate(sex = case_when(
    sex == 10117 ~ "Male",
    sex == 10118 ~ "Female",
    TRUE ~ as.character(sex)),
         
         political_viewpoint_wvs = as.numeric(political_viewpoint_wvs),
         
         ETHNICITY = case_when(
           race_white == 1 ~ "White",
           race_black == 1 ~ "Black",
           race_asian == 1 ~ "Asian",
           race_other == 1 ~ "Other",
           race_pacific == 1 ~ "Native_Hawaiian",
           race_american_indian == 1 ~ "American_Indian",
           TRUE ~ ETHNICITY),
         
         HISPANIC = case_when(hispanic == 1 ~ "Hispanic",
                              TRUE ~ "Non-Hispanic"),
    
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

  # Create a cross section of age x sex x race in sample data
data_2024$demo <- paste(data_2024$ETHNICITY, "_", data_2024$sex, "_", data_2024$age_group)

  # Clean behavioral measures
data_2024 %<>% mutate(hp_adoption = as.numeric(hp_adoption),
                      hp_adoption = case_when(hp_adoption == 1 ~ 1,
                                              TRUE ~ 0),
                      
                      community.solar_adoption = as.numeric(community.solar_adoption),
                      community.solar_adoption = case_when(community.solar_adoption == 1 ~ 1,
                                                           TRUE ~ 0)
                      ) %>% 
  rename(fw_adoption = fw_attempt,
         beef_adoption = beef_attempt)
