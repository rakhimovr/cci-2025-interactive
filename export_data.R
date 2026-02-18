# =============================================================================
# CCI 2025 Data Export for Interactive Explorer
# Runs the existing pipeline, then exports pre-computed summary statistics
# to data/cci-data.json for the chatbot web application.
#
# Usage: Rscript export_data.R
#   (working directory must be the project root)
# =============================================================================

cat("=== CCI 2025 Data Export ===\n\n")

# ---- Load packages --------------------------------------------------------
library(haven)
library(tidyverse)
library(survey)
library(magrittr)
library(readr)
library(stringr)
library(jsonlite)

setwd("C:/Users/RakhimRakhimov/Documents/GitHub/cci-2025-interactive")

# ---- Run existing pipeline ------------------------------------------------
cat("Step 1/6: Loading data...\n")
source("source files 2025/load-data-2025.R")

cat("Step 2/6: Cleaning data...\n")
source("source files 2025/clean-data-2025.R")

cat("Step 3/6: Joining SASSY segments...\n")
sassy <- read_csv("sassy_2025 Segmentation.csv")
sassy <- sassy %>% select(RID, segment)
data_2025 <- data_2025 %>% left_join(sassy, by = "RID")

# Drop non-US columns
drop_cols <- grep("boston|denver", colnames(data_2025), value = TRUE)
data_2025 <- data_2025[, !(names(data_2025) %in% drop_cols)]

cat("Step 4/6: Computing survey weights...\n")
source("source files 2025/national-weights.R")

# ---- Prepare behavioral variables -----------------------------------------
cat("Step 5/6: Preparing variables...\n")

data_2025 %<>% mutate(
  hp_adoption = as.numeric(hp_adoption),
  hp_adoption = case_when(hp_adoption == 1 ~ 1, TRUE ~ 0),
  community.solar_adoption = as.numeric(community.solar_adoption),
  community.solar_adoption = case_when(community.solar_adoption == 1 ~ 1, TRUE ~ 0)
) %>%
  rename(fw_adoption = fw_attempt, beef_adoption = beef_attempt)

# Define behaviors and indicators
behavior_codes <- c("ev", "solar", "community.solar", "offset", "hp", "beef", "fw")
behavior_labels <- c(
  ev = "Drive an EV",
  solar = "Install solar panels",
  community.solar = "Sign up for community solar",
  offset = "Buy carbon offsets",
  hp = "Install a heat pump",
  beef = "Eat less beef",
  fw = "Reduce food waste"
)

indicator_labels <- c(
  adoption = "Reported adoption",
  consider = "Consideration",
  intention = "Intention",
  ee = "Belief that others are engaging in the behavior",
  pnb = "Belief that others should engage in the behavior",
  ne = "Belief that others think people should engage in the behavior",
  self.efficacy = "Self-efficacy",
  outcome.efficacy = "Outcome efficacy",
  personal.benefit = "Perceived personal benefit",
  difficulty = "Perceived ease of adoption",
  policy.support = "Policy support",
  policy.ee = "Perceived policy support",
  policy.info = "Openness to learn about the policy",
  policy.contact = "Openness to contact a representative"
)

# Recode EV adoption and policy support (match Rmd logic)
data_2025 %<>% mutate(
  ev_adoption = case_when(ev_adoption %in% 1:5 ~ 1, TRUE ~ 0),
  ev_policy.support = case_when(ev_policy.support %in% c(3, 4) ~ 1, ev_policy.support %in% c(1, 2) ~ 0),
  solar_policy.support = case_when(solar_policy.support %in% c(3, 4) ~ 1, solar_policy.support %in% c(1, 2) ~ 0),
  community.solar_policy.support = case_when(community.solar_policy.support %in% c(3, 4) ~ 1, community.solar_policy.support %in% c(1, 2) ~ 0),
  offset_policy.support = case_when(offset_policy.support %in% c(3, 4) ~ 1, offset_policy.support %in% c(1, 2) ~ 0),
  hp_policy.support = case_when(hp_policy.support %in% c(3, 4) ~ 1, hp_policy.support %in% c(1, 2) ~ 0),
  beef_policy.support = case_when(beef_policy.support %in% c(3, 4) ~ 1, beef_policy.support %in% c(1, 2) ~ 0),
  fw_policy.support = case_when(fw_policy.support %in% c(3, 4) ~ 1, fw_policy.support %in% c(1, 2) ~ 0)
)

# Map indicator variable names (some use _national suffix, some use _ee directly)
get_var_name <- function(beh, ind) {
  if (ind == "ee") {
    if (beh %in% c("ev", "solar")) return(paste0(beh, "_ee"))
    else return(paste0(beh, "_ee_national"))
  }
  if (ind == "ne") {
    if (beh %in% c("ev", "solar")) return(paste0(beh, "_ne_national"))
    else return(paste0(beh, "_ne_national"))
  }
  return(paste0(beh, "_", ind))
}

# Scale transformation function (match Rmd logic)
transform_value <- function(ind, val) {
  switch(ind,
    adoption = val * 100,
    consider = val * 100,
    ee = val * 10,
    ne = val * 10,
    pnb = val * 100,
    personal.benefit = (val - 1) / 4 * 100,
    outcome.efficacy = (val - 1) / 4 * 100,
    self.efficacy = (val - 1) / 4 * 100,
    policy.support = val * 100,
    policy.ee = val * 10,
    policy.info = val * 100,
    policy.contact = val * 100,
    difficulty = val,       # Keep raw 1-5 scale
    intention = val,        # Already 0-100
    val                     # Default: no transform
  )
}

# ---- Helper: compute weighted stats for a subgroup -----------------------
compute_stats <- function(df, weight_col = "wt") {
  results <- list()
  for (beh in behavior_codes) {
    beh_results <- list()
    for (ind in names(indicator_labels)) {
      var_name <- get_var_name(beh, ind)
      if (!var_name %in% names(df)) next
      vals <- as.numeric(df[[var_name]])
      wts <- as.numeric(df[[weight_col]])
      valid <- !is.na(vals) & !is.na(wts)
      if (sum(valid) < 10) next

      v <- vals[valid]
      w <- wts[valid]
      n <- length(v)
      wmean <- weighted.mean(v, w, na.rm = TRUE)
      # Weighted SE via linearization
      wsum <- sum(w)
      wvar <- sum(w * (v - wmean)^2) / (wsum - sum(w^2) / wsum)
      wse <- sqrt(wvar / n)
      ci_lower <- wmean - 1.96 * wse
      ci_upper <- wmean + 1.96 * wse

      # Apply scale transformation
      wmean_t <- transform_value(ind, wmean)
      ci_lower_t <- transform_value(ind, ci_lower)
      ci_upper_t <- transform_value(ind, ci_upper)

      beh_results[[ind]] <- list(
        mean = round(wmean_t, 1),
        ci_lower = round(ci_lower_t, 1),
        ci_upper = round(ci_upper_t, 1),
        n = n
      )
    }
    results[[beh]] <- beh_results
  }
  results
}

# ---- Compute overall stats -----------------------------------------------
cat("Step 6/6: Computing summary statistics...\n")

overall <- compute_stats(data_2025)

# ---- Compute demographic breakdowns --------------------------------------
compute_grouped <- function(df, group_var) {
  groups <- unique(df[[group_var]])
  groups <- groups[!is.na(groups)]
  result <- list()
  for (g in groups) {
    sub <- df[df[[group_var]] == as.character(g), ]
    if (nrow(sub) >= 30) {
      result[[as.character(g)]] <- compute_stats(sub)
    }
  }
  result
}

# Political party: decode numeric to label
data_2025 <- data_2025 %>% mutate(
  party_label = case_when(
    political_party %in% c(12503, 12504) ~ "Democrats",
    political_party %in% c(12505, 12506) ~ "Republicans",
    political_party == 12507 ~ "Independent/Other",
    political_party == 12618 ~ "No Party Affiliation",
    TRUE ~ NA_character_
  )
)

cat("  Computing age breakdowns...\n")
by_age <- compute_grouped(data_2025, "age_group")

cat("  Computing gender breakdowns...\n")
by_gender <- compute_grouped(data_2025, "sex")

cat("  Computing race breakdowns...\n")
by_race <- compute_grouped(data_2025, "race")

cat("  Computing political party breakdowns...\n")
by_party <- compute_grouped(data_2025, "party_label")

cat("  Computing SASSY segment breakdowns...\n")
by_segment <- compute_grouped(data_2025, "segment")

cat("  Computing education breakdowns...\n")
by_education <- compute_grouped(data_2025, "education")

cat("  Computing income breakdowns...\n")
# Collapse income into fewer groups for meaningful subgroup sizes
data_2025 <- data_2025 %>% mutate(
  income_group = case_when(
    income %in% c("Less than $5,000", "$5,000 to $7,499", "$7,500 to $9,999",
                   "$10,000 to $12,499", "$12,500 to $14,999", "$15,000 to $19,999",
                   "$20,000 to $24,999") ~ "Under $25,000",
    income %in% c("$25,000 to $29,999", "$30,000 to $34,999", "$35,000 to $39,999",
                   "$40,000 to $49,999") ~ "$25,000 - $49,999",
    income %in% c("$50,000 to $59,999", "$60,000 to $74,999") ~ "$50,000 - $74,999",
    income %in% c("$75,000 to $84,999", "$85,000 to $99,999") ~ "$75,000 - $99,999",
    income %in% c("$100,000 to $124,999", "$125,000 to $149,999") ~ "$100,000 - $149,999",
    income == "$150,000 and over" ~ "$150,000 and over",
    TRUE ~ NA_character_
  )
)
by_income <- compute_grouped(data_2025, "income_group")

# ---- Trend data (2024 vs 2025) -------------------------------------------
cat("  Computing trend data...\n")
trends_2024 <- tryCatch({
  read_csv("trends_2024.csv", show_col_types = FALSE)
}, error = function(e) {
  cat("  Warning: trends_2024.csv not found, skipping trends.\n")
  NULL
})

trends <- list()
if (!is.null(trends_2024)) {
  # Compute 2024 weighted means
  trends_2024_stats <- compute_stats(trends_2024)

  # Build comparison
  for (beh in behavior_codes) {
    beh_trends <- list()
    for (ind in names(indicator_labels)) {
      val_2025 <- overall[[beh]][[ind]]
      val_2024 <- trends_2024_stats[[beh]][[ind]]
      if (!is.null(val_2025) && !is.null(val_2024)) {
        diff <- round(val_2025$mean - val_2024$mean, 1)
        # Simple significance test: check if CIs overlap
        sig <- (val_2025$ci_lower > val_2024$ci_upper) |
               (val_2025$ci_upper < val_2024$ci_lower)
        beh_trends[[ind]] <- list(
          mean_2024 = val_2024$mean,
          mean_2025 = val_2025$mean,
          difference = diff,
          significant = sig
        )
      }
    }
    if (length(beh_trends) > 0) trends[[beh]] <- beh_trends
  }
}

# ---- Assemble JSON -------------------------------------------------------
cat("\nAssembling JSON...\n")

export <- list(
  metadata = list(
    title = "Climate Culture Index 2025",
    organization = "Rare",
    survey_date = "August 2025",
    sample_description = "US adults, quota-matched on age group, gender, and ethnicity",
    sample_size = nrow(data_2025),
    weighting_method = "5-step iterative raking (age x gender x race, geography, SES, ethnicity, politics + SASSY segments) with weight trimming (0.2-5.0)",
    years_available = c(2024, 2025),
    behaviors = as.list(behavior_labels),
    indicators = as.list(indicator_labels),
    indicator_scales = list(
      adoption = "Percentage (0-100%)",
      consider = "Percentage (0-100%)",
      intention = "Average intention (0-100)",
      ee = "Perceived percentage of others (0-100)",
      pnb = "Percentage (0-100%)",
      ne = "Perceived percentage (0-100)",
      self.efficacy = "0-100 scale",
      outcome.efficacy = "0-100 scale",
      personal.benefit = "0-100 scale",
      difficulty = "1-5 Likert (higher = more difficult)",
      policy.support = "Percentage supporting (0-100%)",
      policy.ee = "Perceived percentage supporting (0-100)",
      policy.info = "Percentage (0-100%)",
      policy.contact = "Percentage (0-100%)"
    ),
    policy_descriptions = list(
      ev = "State upfront cash rebate for EV purchase or lease",
      solar = "Utility buys excess electricity from solar panels (net metering)",
      community.solar = "Guarantee at least 5% annual savings on electricity bill",
      offset = "Federal requirement for travel providers to offer carbon offsets",
      hp = "State upfront cash rebate for heat pump installation",
      beef = "State increases plant-based meal options in state-run facilities",
      fw = "Federal uniform food date labeling system"
    ),
    demographic_dimensions = list(
      age_group = sort(unique(na.omit(as.character(data_2025$age_group)))),
      gender = sort(unique(na.omit(data_2025$sex))),
      race = sort(unique(na.omit(data_2025$race))),
      political_party = sort(unique(na.omit(data_2025$party_label))),
      sassy_segment = sort(unique(na.omit(data_2025$segment))),
      education = sort(unique(na.omit(data_2025$education))),
      income_group = sort(unique(na.omit(data_2025$income_group)))
    ),
    behavior_colors = list(
      "Drive an EV" = "#005BBB",
      "Install solar panels" = "#F58233",
      "Sign up for community solar" = "#008542",
      "Buy carbon offsets" = "#5E6A71",
      "Install a heat pump" = "#7f5d90",
      "Eat less beef" = "#AA1948",
      "Reduce food waste" = "#884600"
    )
  ),
  overall = overall,
  by_age = by_age,
  by_gender = by_gender,
  by_race = by_race,
  by_party = by_party,
  by_segment = by_segment,
  by_education = by_education,
  by_income = by_income,
  trends = trends
)

# Write JSON
json_out <- toJSON(export, auto_unbox = TRUE, pretty = TRUE, na = "null")
writeLines(json_out, "data/cci-data.json")

cat("\nExport complete! Written to data/cci-data.json\n")
cat("File size:", file.size("data/cci-data.json"), "bytes\n")
