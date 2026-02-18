# ==============================================================================
# LOAD DATA
# ==============================================================================
# Survey data
survey_2024 <- read_sav("~/GitHub/cci-2024/Survey Exports/2024/spss_5_7_2024.sav")
survey_2023 <- read_csv("~/GitHub/cci-2024/Survey Exports/national_2023.csv")
survey_2021 <- read.csv("~/GitHub/cci-2024/Survey Exports/national_2021.csv")

# 2024 Lucid data
# US
lucid_2024_1 <- read_csv("~/GitHub/cci-2024/Lucid Exports/05-07/US/RespondentAnswerReport_48714562_CC_-_Index_2024_-_National_2024-05-07_0847.csv")
lucid_2024_2 <- read_csv("~/GitHub/cci-2024/Lucid Exports/05-07/US/RespondentAnswerReport_50912475_CC_-_Index_2024_-_National_-_V2_2024-05-07_0845.csv")

# Denver
lucid_2024_denver_1 <- read_csv("~/GitHub/cci-2024/Lucid Exports/05-07/Denver/RespondentAnswerReport_48757136_CC_-_Index_2024_-_Denver_2024-05-07_0846.csv")
lucid_2024_denver_2 <- read_csv("~/GitHub/cci-2024/Lucid Exports/05-07/Denver/RespondentAnswerReport_50913982_CC_-_Index_2024_-_Denver_-_V2_2024-05-07_0844.csv")

# ==============================================================================
# CLEAN DATA
# ==============================================================================
# Merge and clean Lucid data

# List of data frames
list_of_dfs <- list(lucid_2024_1, lucid_2024_2, lucid_2024_denver_1, lucid_2024_denver_2)

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

# ==============================================================================
# CREATE GEOGRAPHIC DATAFRAMES
# ==============================================================================
# Geographic dfs
data_us_2024 <- data_2024 %>% filter(Geo == "US")

data_denver_2024 <- subset(data_2024, Geo == "Denver")

################################################
# Move to geographic .Rmd to construct weights #
################################################

# ==============================================================================
# TABLE COLOR FUNCTION
# ==============================================================================
# Function by Greg Lin
# Notice bias here = a positive number. 
# Higher values give more widely spaced colors at the high end
make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}
good_color <- make_color_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)
# Generate a vector of example numbers between 0 and 1
seq(0.1, 0.9, length.out = 12)
# [1] 0.1000000 0.1727273 0.2454545 0.3181818 0.3909091 0.4636364
# [7] 0.5363636 0.6090909 0.6818182 0.7545455 0.8272727 0.9000000
# create matching colors
good_color(seq(0.1, 0.9, length.out = 12))
# [1] "#E9F8CB" "#D9F2C0" "#C9ECB4" "#BDE6B2" "#B0E0AF" "#A4DAAD"
# [7] "#97D5AB" "#88CFAB" "#79C9AB" "#69C3AB" "#5ABDAB" "#4AB8AB"
# display the colors
seq(0.1, 0.9, length.out = 12) %>% 
  good_color() %>% 
  scales::show_col()

# ==============================================================================
# DENVER WEIGHTS
# ==============================================================================
#Adding weights for Denver

# Hispanic / Non-Hispanic
hispanic_denver <- read_csv("~/GitHub/cci-2024/Weights/Denver/hispanic-denver.csv")

# Age-sex-race
age_sex_race_denver <- read_csv("~/GitHub/cci-2024/Weights/Denver/age-sex-race-denver.csv")


###

# Hispanic
hispanic_denver %<>% mutate(freq = nrow(data_denver_2024) * prop)
hispanic_denver %<>% select(demo, freq) %>% rename(HISPANIC = demo)

age_sex_race_denver %<>% select(age_group: ethnicity, prop) %>% 
  mutate(freq = nrow(data_denver_2024) * prop) %>% rename(race= ethnicity)

# Update the 'race' variable in the age_sex_race_denver dataset
age_sex_race_denver <- age_sex_race_denver %>%
  mutate(race = case_when(
    race == "AI" ~ "American_Indian",    # Change "AI" to "American_Indian"
    race == "NH" ~ "Native_Hawaiian",    # Change "NH" to "Native_Hawaiian"
    TRUE ~ race                          # Keep other values unchanged
  ))

# Create a demo variable
age_sex_race_denver %<>% mutate(demo = paste(race,"_", sex, "_", age_group )) %>% 
  select(demo, freq)

###Note: I manually updated an observation in the population csv file from White_female_55-65 to White_female_55+


age_sex_race_denver %<>% mutate(freq = replace(freq, freq == 0, 1))

#Creating the weight variable in Denver data

# Remove rows with non-response in the sample
age_sex_race_denver <- age_sex_race_denver %>%
  filter(demo %in% data_denver_2024$demo)

# Create un-weighted survey object
svy.unweighted <- svydesign(ids = ~ 1, data = data_denver_2024)

# Create survey design object with weights
svy.rake <- rake(design = svy.unweighted,
                 sample.margins = list(~demo,
                                       ~HISPANIC),
                 population.margins = list(age_sex_race_denver,
                                           hispanic_denver))


summary(weights(svy.rake))

# Extract weight values
wt <- data.frame(wt = weights(svy.rake))

# Bind weights with the df, re-order variable
data_denver_2024 <- cbind(data_denver_2024, wt)

# ==============================================================================
# CORE TABLE DENVER
# ==============================================================================
# Main index table for Denver
index_denver_2024 <- data_denver_2024 %>% 
  select(
    wt,
    RID,
    
    # EVs
    ev_ne_denver,
    ev_adoption,
    ev_consider, 
    ev_ee_denver, 
    ev_pnb, 
    ev_intention, 
    ev_personal.benefit, 
    ev_self.efficacy, 
    ev_difficulty, 
    ev_outcome.efficacy,
    ev_policy.support,
    ev_policy.ee,
    ev_policy.info,
    ev_policy.contact,
    
    # Install solar panels
    solar_adoption,
    solar_ne_denver,
    solar_pnb,
    solar_ee_denver,
    solar_consider, 
    solar_ee, 
    solar_intention, 
    solar_personal.benefit, 
    solar_self.efficacy, 
    solar_difficulty, 
    solar_outcome.efficacy,
    solar_policy.support,
    solar_policy.ee,
    solar_policy.info,
    solar_policy.contact,
    
    # Community solar
    community.solar_adoption,
    community.solar_consider, 
    community.solar_ee_denver, 
    community.solar_pnb, 
    community.solar_intention, 
    community.solar_ne_denver, 
    community.solar_personal.benefit, 
    community.solar_self.efficacy, 
    community.solar_difficulty, 
    community.solar_outcome.efficacy,
    community.solar_policy.support,
    community.solar_policy.ee,
    community.solar_policy.info,
    community.solar_policy.contact,
    
    # Offsets
    offset_adoption,
    offset_consider,
    offset_ee_denver,
    offset_ne_denver,
    offset_intention,
    offset_pnb, 
    offset_personal.benefit, 
    offset_self.efficacy, 
    offset_difficulty, 
    offset_outcome.efficacy,
    offset_policy.support,
    offset_policy.ee,
    offset_policy.info,
    offset_policy.contact,
    
    # Heat pump AC
    hp_ne_denver,
    hp_adoption,
    hp_consider, 
    hp_ee_denver, 
    hp_pnb, 
    hp_intention, 
    hp_personal.benefit, 
    hp_self.efficacy, 
    hp_difficulty, 
    hp_outcome.efficacy,
    hp_policy.support,
    hp_policy.ee,
    hp_policy.info,
    hp_policy.contact,
    
    # Beef
    beef_adoption, 
    beef_consider, 
    beef_difficulty, 
    beef_ee_denver, 
    beef_pnb, 
    beef_ne_denver, 
    beef_intention,
    #beef_last_three_meals,
    beef_personal.benefit, 
    beef_self.efficacy, 
    beef_outcome.efficacy,
    beef_policy.support,
    beef_policy.ee,
    beef_policy.info,
    beef_policy.contact,
    
    # Food waste
    fw_adoption,
    fw_consider,
    fw_pnb,
    fw_ee_denver,
    fw_ne_denver,
    fw_intention,
    fw_personal.benefit,
    fw_self.efficacy,
    fw_difficulty,
    fw_outcome.efficacy,
    fw_policy.support,
    fw_policy.ee,
    fw_policy.info,
    fw_policy.contact)

# Clean behavioral variables
index_denver_2024 %<>% mutate(ev_adoption = case_when(ev_adoption == 0 ~ 0,
                                                      ev_adoption == 1 ~ 1,
                                                      ev_adoption == 2 ~ 1,
                                                      ev_adoption == 5 ~ 1,
                                                      ev_adoption == 10 ~ 1,
                                                      TRUE ~ 0),
                              
                              ev_policy.support = case_when(
                                ev_policy.support %in% c(3, 4) ~ 1,
                                ev_policy.support %in% c(1, 2) ~ 0,
                              ),
                              solar_policy.support = case_when(
                                solar_policy.support %in% c(3, 4) ~ 1,
                                solar_policy.support %in% c(1, 2) ~ 0,
                              ),
                              community.solar_policy.support = case_when(
                                community.solar_policy.support %in% c(3, 4) ~ 1,
                                community.solar_policy.support %in% c(1, 2) ~ 0,
                              ),
                              offset_policy.support = case_when(
                                offset_policy.support %in% c(3, 4) ~ 1,
                                offset_policy.support %in% c(1, 2) ~ 0,
                              ),
                              hp_policy.support = case_when(
                                hp_policy.support %in% c(3, 4) ~ 1,
                                hp_policy.support %in% c(1, 2) ~ 0,
                              ),
                              beef_policy.support = case_when(
                                beef_policy.support %in% c(3, 4) ~ 1,
                                beef_policy.support %in% c(1, 2) ~ 0,
                              ),
                              fw_policy.support = case_when(
                                fw_policy.support %in% c(3, 4) ~ 1,
                                fw_policy.support %in% c(1, 2) ~ 0,
                              ))

survey_denver_2024 <- index_denver_2024

index_denver_2024 <- index_denver_2024 %>%
  select(RID, wt,  ev_ne_denver:fw_policy.contact)

index_denver_2024 <- mutate_all(index_denver_2024, function(x) as.numeric(as.character(x)))

index_denver_2024 <- index_denver_2024 %>%
  pivot_longer(
    cols = contains(c("intention",  "ee", "pnb", "ne", "adoption", "consider", "personal.benefit", "self.efficacy", "outcome.efficacy", "difficulty", 
                      "policy.support", "policy.ee", "policy.info", "policy.contact")),
    names_to = "indicator",
    values_to = "value",
    values_drop_na = TRUE)

index_denver_2024 <- index_denver_2024 %>%
  mutate(behavior = indicator)

fin_table <- index_denver_2024 %>% 
  mutate(behavior = sapply(strsplit(index_denver_2024$indicator, split = '_', fixed = TRUE),function(x) (x[1])))

fin_table <- fin_table %>% 
  mutate(ind = sapply(strsplit(fin_table$indicator, split = '_', fixed = TRUE),function(x) (x[2])))

fin_table %<>% dplyr::select(-RID, -indicator)

t1 <- group_by(fin_table, behavior, ind)

t1 <- summarise(t1, w.mean = weighted.mean(value, wt, na.rm = TRUE))

t1 %<>% 
  group_by(behavior) %>% 
  pivot_wider(names_from = ind, values_from = w.mean)

t1 <- t1 %>% 
  mutate(ee = ee/10,
         intention = intention/100,
         ne = ne/10,
         personal.benefit = (personal.benefit - 1)/4,
         outcome.efficacy = (outcome.efficacy - 1)/4,
         self.efficacy = (self.efficacy - 1)/4,
         difficulty = (100 - difficulty)/100,
         policy.ee = policy.ee/10)

t1 <- t1 %>% 
  rename('Behavior' = behavior,
         'Consideration' = consider,
         'Adoption' = adoption,
         'Intention' = intention,
         'Belief that others are engaging in the behavior' = ee,
         'Belief that others should engage in the behavior' = pnb,
         'Belief that others think people should engage in the behavior' = ne,
         'Self-efficacy' = self.efficacy,
         'Outcome efficacy' = outcome.efficacy,
         'Perceived personal benefit' = personal.benefit,
         'Perceived ease of adoption' = difficulty,
         'Policy support' = policy.support,
         'Perceived policy support' = policy.ee,
         'Openness to learn about the policy' = policy.info,
         'Openness to contact a representative' = policy.contact)

# Rename rows
t1$Behavior[t1$Behavior == "ev"] <- "Purchase an EV"
t1$Behavior[t1$Behavior == "offset"] <- "Purchase carbon offsets"
t1$Behavior[t1$Behavior == "solar"] <- "Install solar panels"
t1$Behavior[t1$Behavior == "beef"] <- "Eat less beef"
t1$Behavior[t1$Behavior == "community.solar"] <- "Subscribe to community solar"
t1$Behavior[t1$Behavior == "fw"] <- "Reduce food waste"
t1$Behavior[t1$Behavior == "hp"] <- "Install a heat pump"

# Re-order variables
t1 <- t1 %>% select(Behavior, Consideration, Adoption, Intention, 
                    `Belief that others are engaging in the behavior`, 
                    `Belief that others should engage in the behavior`, 
                    `Belief that others think people should engage in the behavior`, 
                    `Self-efficacy`, `Outcome efficacy`,
                    `Perceived personal benefit`, `Perceived ease of adoption`,
                    `Policy support`, `Perceived policy support`, `Openness to learn about the policy`, `Openness to contact a representative`)

# ==============================================================================
# TRENDS ANALYSIS
# ==============================================================================
# 2024 data
survey_denver_2024 <- survey_denver_2024 %>% 
  select(wt,
         RID,
    
    # Beef
    beef_adoption, 
    beef_consider, 
    beef_ee_denver, 
    beef_intention,
    beef_ne_denver, 
    beef_personal.benefit,
    beef_pnb, 
    beef_self.efficacy,
    
    # Food waste
    fw_adoption,
    fw_consider,
    fw_pnb,
    fw_ee_denver,
    fw_ne_denver,
    fw_intention,
    fw_personal.benefit,
    fw_self.efficacy,
    
    # Community solar
    community.solar_adoption,
    community.solar_consider, 
    community.solar_ee_denver, 
    community.solar_intention, 
    community.solar_ne_denver, 
    community.solar_personal.benefit, 
    community.solar_pnb, 
    community.solar_self.efficacy, 
    
    # EV
    ev_adoption,
    ev_consider, 
    ev_ee_denver, 
    ev_intention, 
    ev_ne_denver,
    ev_personal.benefit, 
    ev_pnb, 
    ev_self.efficacy,
    
    # Heat pump AC
    hp_adoption,
    hp_consider, 
    hp_ee_denver, 
    hp_intention, 
    hp_ne_denver,
    hp_personal.benefit, 
    hp_pnb, 
    hp_self.efficacy, 
    
    # Offsets
    offset_adoption,
    offset_consider,
    offset_ee_denver,
    offset_intention,
    offset_ne_denver,
    offset_personal.benefit, 
    offset_pnb, 
    offset_self.efficacy, 
    
    # Install solar panels
    solar_adoption,
    solar_consider, 
    solar_ee_denver,
    solar_intention, 
    solar_ne_denver,
    solar_personal.benefit, 
    solar_pnb,
    solar_self.efficacy
  )

#personal.benefit = (personal.benefit - 1)/4*100,
#outcome.efficacy = (outcome.efficacy - 1)/4*100,
#self.efficacy = (self.efficacy - 1)/4*100,

survey_denver_2024 %<>% rename_with(~ gsub("_denver", "", .x), everything()) %>% # Remove suffixes "_denver"
  mutate(wave = "2024") 

# Clean behavioral variables
survey_denver_2024 %<>% mutate(ev_adoption = case_when(ev_adoption == 0 ~ 0,
                                                      ev_adoption == 1 ~ 1,
                                                      ev_adoption == 2 ~ 1,
                                                      ev_adoption == 5 ~ 1,
                                                      ev_adoption == 10 ~ 1,
                                                      TRUE ~ 0))


survey_denver_2023 <- read_sav("~/GitHub/cc-index-2023/data_denver_2023.sav")

survey_denver_2023 <- survey_denver_2023 %>% select(wt,
                             RID,
                             -contains("retrofit"),
                             contains("solar"),
                             contains("offset"),
                             contains("beef"),
                             contains("hp"),
                             contains("ev"),
                             contains("community.solar"),
                             
                             -contains("_tx"), # Remove other geos
                             -contains("_ca"),
                             -contains("boston"),
                             -contains("national"),
                             -contains("heard"),
                             -contains("diff"),
                             -contains("outcome")) %>% 
  rename_with(~ gsub("_denver", "", .x), everything()) %>% # Remove suffixes "_denver"
  mutate(wave = "2023") 


# List of data frames
list_of_dfs <- list(survey_denver_2023, survey_denver_2024)

# Convert all columns in all data frames to character
list_of_dfs <- map(list_of_dfs, ~ .x %>% mutate(across(everything(), as.character)))

# Combine data frames
trends_2024 <- bind_rows(list_of_dfs)

trends_2024 <- mutate_all(trends_2024, function(x) as.numeric(as.character(x)))


################
# 2025 data
################

index_2025 <- data_2025 %>% 
  select(
    wt,
    RID,
    
    # EVs
    ev_ne,
    ev_adoption,
    ev_consider, 
    ev_ee, 
    ev_pnb, 
    ev_intention, 
    ev_personal.benefit, 
    ev_self.efficacy, 
    
    # Install solar panels
    solar_adoption,
    solar_ne,
    solar_pnb,
    solar_ee,
    solar_consider, 
    solar_ee, 
    solar_pnb, 
    solar_intention, 
    solar_personal.benefit, 
    solar_self.efficacy, 

    # Community solar
    community.solar_adoption,
    community.solar_consider, 
    community.solar_ee, 
    community.solar_pnb, 
    community.solar_intention, 
    community.solar_ne, 
    community.solar_personal.benefit, 
    community.solar_self.efficacy, 
    
    # Offsets
    offset_adoption,
    offset_consider,
    offset_ee,
    offset_ne,
    offset_intention,
    offset_pnb, 
    offset_intention, 
    offset_personal.benefit, 
    offset_self.efficacy,
    
    # Heat pump AC
    hp_ne,
    hp_adoption,
    hp_consider, 
    hp_ee, 
    hp_pnb, 
    hp_intention, 
    hp_personal.benefit, 
    hp_self.efficacy, 
    
    # Beef
    beef_adoption, 
    beef_consider,
    beef_ee, 
    beef_pnb, 
    beef_ne, 
    beef_intention,
    #beef_last_three_meals,
    beef_personal.benefit, 
    beef_self.efficacy, 
    
    # Food waste
    fw_adoption,
    fw_consider,
    fw_pnb,
    fw_ee,
    fw_ne,
    fw_intention,
    fw_personal.benefit,
    fw_self.efficacy)

# Clean behavioral variables
index_2025 %<>% mutate(ev_adoption = case_when(ev_adoption == 0 ~ 0,
                                               ev_adoption == 1 ~ 1,
                                               ev_adoption == 2 ~ 1,
                                               ev_adoption == 3 ~ 1,
                                               ev_adoption == 4 ~ 1,
                                               ev_adoption == 10 ~ 1,
                                               TRUE ~ 0))

index_2025 <- mutate_all(index_2025, function(x) as.numeric(as.character(x)))

index_2025$wave <- 2025

#############
# Rbind

trends_2025 <- rbind(trends_2024, index_2025)

trends_2025 <- mutate_all(trends_2025, function(x) as.numeric(as.character(x)))

#############

# Alternative data processing for detailed plots
behavior_mapping <- c(
  "ev" = "Drive an EV", "solar" = "Install solar panels", "offset" = "Buy carbon offsets",
  "community.solar" = "Sign up for community solar", "hp" = "Install a heat pump",
  "beef" = "Eat less beef", "fw" = "Reduce food waste"
)

name_mapping <- c(
  "adoption" = "Reported adoption", "consider" = "Consideration", "intention" = "Reported intention",
  "ee" = "Empirical expectations", "pnb" = "Personal normative beliefs",
  "ne" = "Normative expectations", "self.efficacy" = "Self-efficacy",
  "personal.benefit" = "Personal benefit", "outcome.efficacy" = "Outcome efficacy",
  "difficulty" = "Perceived difficulty",
  "policy.support" = "Policy support", "policy.ee" = "Perceived policy support",
  "policy.info" = "Openness to learn about policy", "policy.contact" = "Willingness to contact representative"
)

# Long format with readable labels
long <- trends_2025 %>%
  pivot_longer(cols = -c(wave, wt),
               names_to = c("behavior", "measure"), 
               names_pattern = "^(.+?)_(.+)$",  # Split on first underscore
               values_to = "value") %>%
  # Apply the mappings to create readable labels
  mutate(
    behavior_label = behavior_mapping[behavior],
    measure_label = name_mapping[measure]
  )

df_summary <- long %>%
  filter(!is.na(value)) %>%
  group_by(wave, behavior, measure, behavior_label, measure_label) %>%
  summarise(
    n = n(),
    mean = weighted.mean(value, w = wt, na.rm = TRUE),
    weighted_var = sum(wt * (value - weighted.mean(value, w = wt, na.rm = TRUE))^2) / sum(wt),
    weighted_sd = sqrt(weighted_var),
    se = weighted_sd / sqrt(n()),
    conf.low = mean - qt(0.975, df = n() - 1) * se,
    conf.high = mean + qt(0.975, df = n() - 1) * se,
    .groups = "drop"
  )

# Rescale measures to appropriate scales
df_summary <- df_summary %>%
  mutate(
    # Convert to percentages (0-1 to 0-100)
    mean = ifelse(measure_label %in% c("Reported adoption", "Consideration", "Personal normative beliefs"), 
                  mean * 100, mean),
    conf.low = ifelse(measure_label %in% c("Reported adoption", "Consideration", "Personal normative beliefs"), 
                      conf.low * 100, conf.low),
    conf.high = ifelse(measure_label %in% c("Reported adoption", "Consideration", "Personal normative beliefs"), 
                       conf.high * 100, conf.high),
    
    # Convert 0-10 scale to 0-100 (multiply by 10)
    mean = ifelse(measure_label %in% c("Empirical expectations", "Normative expectations"), 
                  mean * 10, mean),
    conf.low = ifelse(measure_label %in% c("Empirical expectations", "Normative expectations"), 
                      conf.low * 10, conf.low),
    conf.high = ifelse(measure_label %in% c("Empirical expectations", "Normative expectations"), 
                       conf.high * 10, conf.high)
  )


# List of measures to apply the scaling to
measures_to_scale <- c("Self-efficacy", "Outcome efficacy", "Personal benefit")

df_summary <- df_summary %>%
  mutate(across(c(mean, conf.low, conf.high),
                ~ if_else(measure_label %in% measures_to_scale, (. - 1) / 4 * 100, .)))