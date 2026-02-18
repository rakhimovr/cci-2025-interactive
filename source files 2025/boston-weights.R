# Hispanic / Non-Hispanic
hispanic <- read_csv("weights/Boston/boston-hispanic.csv")

# Hispanic
hispanic %<>% mutate(Freq = nrow(data_2025) * prop)
hispanic %<>% select(hispanic, Freq) %>% rename(HISPANIC = hispanic)


# Demo
age_sex_race <- read_csv("~/GitHub/cci-2025/Weights/Boston/boston-age-gender-race.csv")

age_sex_race %<>% mutate(Freq = nrow(data_2025) * prop) %>% select(age_group, sex, race, Freq)

# Create a demo variable
age_sex_race %<>% mutate(demo = paste(race, "_", sex, "_", age_group)) %>% 
  select(demo, Freq)

age_sex_race %<>% mutate(Freq = pmax(Freq, 1))

# Remove rows with non-response in the sample
data_2025 %<>% mutate(demo = paste(race, "_", sex, "_", age_group))

age_sex_race <- age_sex_race %>%
  filter(demo %in% data_2025$demo)

# Create un-weighted survey object
svy.unweighted <- svydesign(ids = ~ 1, data = data_2025)

svy.final <- rake(design = svy.unweighted,
                  sample.margins = list(~demo,
                                        ~HISPANIC),
                  population.margins = list(age_sex_race,
                                            hispanic),
                  control = list(verbose = TRUE))

# Check current weights
print("=== BEFORE TRIMMING ===")
summary(weights(svy.final))

# Use the built-in trimWeights() function
# Syntax: trimWeights(design, lower=, upper=, strict=FALSE)
svy.final <- trimWeights(svy.final, lower = 0.2, upper = 5.0)

print("=== AFTER TRIMMING ===")
summary(weights(svy.final))

# Extract weight values
wt <- data.frame(wt = weights(svy.final))

# Bind weights with the df, re-order variable
data_2025 <- cbind(data_2025, wt)