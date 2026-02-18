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