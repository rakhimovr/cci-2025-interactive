# Plot Setup - Required libraries and configurations
# This file contains common elements needed by all plot scripts

# Load required libraries
library(ggrepel)

# Define custom colors for behaviors
custom_colors <- c("Drive an EV" = "#005BBB", 
                   "Buy carbon offsets" = "#5E6A71", 
                   "Install solar panels" = "#F58233",
                   "Sign up for community solar" = "#008542",  
                   "Install a heat pump" = "#7f5d90",
                   "Eat less beef" = "#AA1948",
                   "Reduce food waste" = "#884600") 

# Color function for table formatting (if needed)
good_color <- function(x) {
  # Function to create color gradients for tables
  # This would need to be defined based on your specific needs
  rgb(0.2, 0.6, 0.9, alpha = x)
}