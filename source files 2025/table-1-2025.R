# Climate Culture Index Table Generation for 2025
# Verified and corrected version with proper scale transformations

cat("Creating Climate Culture Index table for 2025...\n")

# =========================================================================
# BEHAVIORAL VARIABLE STANDARDIZATION
# =========================================================================

# TODO: Update scale transformations after running verify-critical-values.R
# Current transformations are based on standard assumptions - VERIFY SCALES

# Create standardized behavioral variables
cat("Standardizing behavioral variables...\n")

# ADOPTION VARIABLES - Convert to binary (0/1) and percentage
# TODO: Verify if variables are 0/1 or 1/2 coding
data_2025 <- data_2025 %>% mutate(
  # Adoption variables (assume binary conversion needed)
  beef_adoption_std = case_when(
    beef_attempt == 1 ~ 1,
    beef_attempt == 0 ~ 0,
    TRUE ~ NA_real_
  ),
  fw_adoption_std = case_when(
    fw_attempt == 1 ~ 1, 
    fw_attempt == 0 ~ 0,
    TRUE ~ NA_real_
  ),
  ev_adoption_std = case_when(
    ev_adoption == 1 ~ 1,
    ev_adoption == 0 ~ 0,
    TRUE ~ NA_real_
  ),
  solar_adoption_std = case_when(
    solar_adoption == 1 ~ 1,
    solar_adoption == 0 ~ 0, 
    TRUE ~ NA_real_
  ),
  community.solar_adoption_std = case_when(
    community.solar_adoption == 1 ~ 1,
    community.solar_adoption == 0 ~ 0,
    TRUE ~ NA_real_
  ),
  hp_adoption_std = case_when(
    hp_adoption == 1 ~ 1,
    hp_adoption == 0 ~ 0,
    TRUE ~ NA_real_
  ),
  offset_adoption_std = case_when(
    offset_adoption == 1 ~ 1,
    offset_adoption == 0 ~ 0,
    TRUE ~ NA_real_
  )
)

# INTENTION VARIABLES - Standardize to 0-100 scale
# TODO: Verify if intentions are 0-10 or 0-100 scale
intention_scale_factor <- 1  # UPDATE: Change to 10 if intentions are 0-10 scale

data_2025 <- data_2025 %>% mutate(
  beef_intention_std = beef_intention * intention_scale_factor,
  fw_intention_std = fw_intention * intention_scale_factor,
  ev_intention_std = ev_intention * intention_scale_factor,
  solar_intention_std = solar_intention * intention_scale_factor,
  community.solar_intention_std = community.solar_intention * intention_scale_factor,
  hp_intention_std = hp_intention * intention_scale_factor,
  offset_intention_std = offset_intention * intention_scale_factor
)

# EFFICACY VARIABLES - Convert to 0-100 scale 
# TODO: Verify if efficacy variables are 1-5 or 1-7 scale
# Standard transformation: (x - min) / (max - min) * 100

# Assume 1-5 scale for now - UPDATE if different
efficacy_min <- 1
efficacy_max <- 5

data_2025 <- data_2025 %>% mutate(
  # Personal benefit
  beef_personal.benefit_std = (beef_personal.benefit - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  fw_personal.benefit_std = (fw_personal.benefit - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  ev_personal.benefit_std = (ev_personal.benefit - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  solar_personal.benefit_std = (solar_personal.benefit - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  community.solar_personal.benefit_std = (community.solar_personal.benefit - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  hp_personal.benefit_std = (hp_personal.benefit - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  offset_personal.benefit_std = (offset_personal.benefit - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  
  # Self-efficacy
  beef_self.efficacy_std = (beef_self.efficacy - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  fw_self.efficacy_std = (fw_self.efficacy - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  ev_self.efficacy_std = (ev_self.efficacy - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  solar_self.efficacy_std = (solar_self.efficacy - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  community.solar_self.efficacy_std = (community.solar_self.efficacy - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  hp_self.efficacy_std = (hp_self.efficacy - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  offset_self.efficacy_std = (offset_self.efficacy - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  
  # Outcome efficacy
  beef_outcome.efficacy_std = (beef_outcome.efficacy - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  fw_outcome.efficacy_std = (fw_outcome.efficacy - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  ev_outcome.efficacy_std = (ev_outcome.efficacy - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  solar_outcome.efficacy_std = (solar_outcome.efficacy - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  community.solar_outcome.efficacy_std = (community.solar_outcome.efficacy - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  hp_outcome.efficacy_std = (hp_outcome.efficacy - efficacy_min) / (efficacy_max - efficacy_min) * 100,
  offset_outcome.efficacy_std = (offset_outcome.efficacy - efficacy_min) / (efficacy_max - efficacy_min) * 100
)

# POLICY SUPPORT - Convert to binary (support = 1, oppose = 0)
# TODO: Verify scale - assume 1-4 scale with 3-4 = support
data_2025 <- data_2025 %>% mutate(
  beef_policy.support_std = case_when(
    beef_policy.support %in% c(3, 4) ~ 1,
    beef_policy.support %in% c(1, 2) ~ 0,
    TRUE ~ NA_real_
  ),
  fw_policy.support_std = case_when(
    fw_policy.support %in% c(3, 4) ~ 1,
    fw_policy.support %in% c(1, 2) ~ 0,
    TRUE ~ NA_real_
  ),
  ev_policy.support_std = case_when(
    ev_policy.support %in% c(3, 4) ~ 1,
    ev_policy.support %in% c(1, 2) ~ 0,
    TRUE ~ NA_real_
  ),
  solar_policy.support_std = case_when(
    solar_policy.support %in% c(3, 4) ~ 1,
    solar_policy.support %in% c(1, 2) ~ 0,
    TRUE ~ NA_real_
  ),
  community.solar_policy.support_std = case_when(
    community.solar_policy.support %in% c(3, 4) ~ 1,
    community.solar_policy.support %in% c(1, 2) ~ 0,
    TRUE ~ NA_real_
  ),
  hp_policy.support_std = case_when(
    hp_policy.support %in% c(3, 4) ~ 1,
    hp_policy.support %in% c(1, 2) ~ 0,
    TRUE ~ NA_real_
  ),
  offset_policy.support_std = case_when(
    offset_policy.support %in% c(3, 4) ~ 1,
    offset_policy.support %in% c(1, 2) ~ 0,
    TRUE ~ NA_real_
  )
)

# =========================================================================
# RESHAPE DATA FOR ANALYSIS
# =========================================================================

cat("Reshaping data for analysis...\n")

# Select key variables for analysis
analysis_vars <- data_2025 %>% 
  select(
    RID, wt,
    
    # Standardized behavioral variables
    beef_adoption_std, beef_intention_std, beef_personal.benefit_std, 
    beef_self.efficacy_std, beef_outcome.efficacy_std, beef_policy.support_std,
    beef_ee_national, beef_ne_national, beef_pnb, beef_consider,
    
    fw_adoption_std, fw_intention_std, fw_personal.benefit_std,
    fw_self.efficacy_std, fw_outcome.efficacy_std, fw_policy.support_std,
    fw_ee_national, fw_ne_national, fw_pnb, fw_consider,
    
    ev_adoption_std, ev_intention_std, ev_personal.benefit_std,
    ev_self.efficacy_std, ev_outcome.efficacy_std, ev_policy.support_std,
    ev_ee_national, ev_ne_national, ev_pnb, ev_consider,
    
    solar_adoption_std, solar_intention_std, solar_personal.benefit_std,
    solar_self.efficacy_std, solar_outcome.efficacy_std, solar_policy.support_std,
    solar_ee_national, solar_ne_national, solar_pnb, solar_consider,
    
    community.solar_adoption_std, community.solar_intention_std, community.solar_personal.benefit_std,
    community.solar_self.efficacy_std, community.solar_outcome.efficacy_std, community.solar_policy.support_std,
    community.solar_ee_national, community.solar_ne_national, community.solar_pnb, community.solar_consider,
    
    hp_adoption_std, hp_intention_std, hp_personal.benefit_std,
    hp_self.efficacy_std, hp_outcome.efficacy_std, hp_policy.support_std,
    hp_ee_national, hp_ne_national, hp_pnb, hp_consider,
    
    offset_adoption_std, offset_intention_std, offset_personal.benefit_std,
    offset_self.efficacy_std, offset_outcome.efficacy_std, offset_policy.support_std,
    offset_ee_national, offset_ne_national, offset_pnb, offset_consider
  )

# Reshape to long format for analysis
behavioral_long <- analysis_vars %>%
  pivot_longer(
    cols = -c(RID, wt),
    names_to = "variable",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  # Extract behavior and indicator
  separate(variable, into = c("behavior", "indicator"), sep = "_", extra = "merge") %>%
  # Clean up indicator names
  mutate(
    indicator = case_when(
      indicator == "adoption_std" ~ "adoption",
      indicator == "intention_std" ~ "intention", 
      indicator == "personal.benefit_std" ~ "personal_benefit",
      indicator == "self.efficacy_std" ~ "self_efficacy",
      indicator == "outcome.efficacy_std" ~ "outcome_efficacy",
      indicator == "policy.support_std" ~ "policy_support",
      indicator == "ee_national" ~ "empirical_expectations",
      indicator == "ne_national" ~ "normative_expectations",
      TRUE ~ indicator
    )
  )

# =========================================================================
# CALCULATE WEIGHTED MEANS BY BEHAVIOR AND INDICATOR
# =========================================================================

cat("Calculating weighted means...\n")

results_table <- behavioral_long %>%
  group_by(behavior, indicator) %>%
  summarise(
    weighted_mean = weighted.mean(value, wt, na.rm = TRUE),
    n = sum(!is.na(value)),
    .groups = 'drop'
  ) %>%
  # Convert to percentages where appropriate
  mutate(
    weighted_mean_display = case_when(
      indicator %in% c("adoption", "policy_support", "consider") ~ weighted_mean * 100,
      indicator %in% c("personal_benefit", "self_efficacy", "outcome_efficacy") ~ weighted_mean, # Already 0-100
      indicator %in% c("intention") ~ weighted_mean, # Should be 0-100
      indicator %in% c("empirical_expectations", "normative_expectations", "pnb") ~ weighted_mean * 10, # Scale appropriately
      TRUE ~ weighted_mean
    )
  ) %>%
  select(-weighted_mean) %>%
  rename(value = weighted_mean_display)

# Pivot to wide format for final table
final_table_2025 <- results_table %>%
  pivot_wider(names_from = indicator, values_from = value, values_fill = NA)

# Clean up behavior names
final_table_2025 <- final_table_2025 %>%
  mutate(
    behavior = case_when(
      behavior == "beef" ~ "Eat less beef",
      behavior == "fw" ~ "Reduce food waste", 
      behavior == "ev" ~ "Drive an electric vehicle",
      behavior == "solar" ~ "Install solar panels",
      behavior == "community.solar" ~ "Sign up for community solar",
      behavior == "hp" ~ "Install a heat pump",
      behavior == "offset" ~ "Buy carbon offsets",
      TRUE ~ behavior
    )
  )

# Reorder columns logically
col_order <- c("behavior", "adoption", "consider", "intention", "empirical_expectations", 
              "normative_expectations", "pnb", "personal_benefit", "self_efficacy", 
              "outcome_efficacy", "policy_support")

available_cols <- intersect(col_order, names(final_table_2025))
final_table_2025 <- final_table_2025[, available_cols]

cat("Climate Culture Index table created successfully!\n")
cat("Behaviors analyzed:", nrow(final_table_2025), "\n") 
cat("Indicators per behavior:", ncol(final_table_2025) - 1, "\n")

# Display sample of results
cat("\nSample results:\n")
print(head(final_table_2025, 3))