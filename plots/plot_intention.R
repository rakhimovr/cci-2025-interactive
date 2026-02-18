# Plot: Reported Intention
# Lines 762-856 from cci-25.Rmd

create_intention_plot <- function(df_summary, custom_colors) {
  
  # Intention
  measures_ordered <- c("Reported intention")
  
  plot_intention <- lapply(1:length(measures_ordered), function(i) {
    m <- measures_ordered[i]
    
    # Filter using measure_label instead of measure
    df_measure <- df_summary %>% 
      filter(measure_label == m)
    
    # Specify the order of behaviors as they should appear in the legend
    behavior_order <- c("Reduce food waste", "Eat less beef", "Drive an EV", 
                        "Install a heat pump", "Install solar panels",
                        "Sign up for community solar", "Buy carbon offsets")
    
    custom_nudges <- data.frame(
      behavior_label = behavior_order,  # Changed from behavior to behavior_label
      nudge_x = c(0, 0, 0, 0, 0, 0.2, 0),
      nudge_y = c(5, 5, 5, -4.5, 5, -5, -5)
    )
    
    # Filter data frame for first observation of each behavior
    first_observation <- df_measure %>%
      group_by(behavior_label) %>%  # Changed from behavior to behavior_label
      filter(wave == min(wave)) %>%
      ungroup()
    
    # Main plot using behavior_label for grouping and coloring
    p <- ggplot(df_measure, aes(x = factor(wave), y = mean, 
                                 group = behavior_label, 
                                 color = behavior_label)) +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                    width = 0.175, alpha = 0.45, 
                    position = position_dodge(width = 0.08)) +
      geom_line(position = position_dodge(width = 0.08)) +
      geom_point(size = 1.5, position = position_dodge(width = 0.08), alpha = 0.7)
    
    # Remove the nudged labels - will use bottom legend instead
    
    # Determine x-axis labels based on available waves
    unique_waves <- sort(unique(df_measure$wave))
    x_labels <- as.character(unique_waves)
    
    p <- p + labs(y = "") +
      theme_minimal() +
      theme(
        text = element_text(family = "Arial"),
        axis.text.x = element_text(vjust = 0.5, hjust = 1, color = "grey60", size = 7.5),
        axis.text.y = element_text(color = "grey60", size = 7.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 6, face = "italic"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.line = element_line(colour = "grey70"),
        plot.title = element_text(size = 9.5, color = "black", face = "bold"),  
        plot.subtitle = element_text(size = 6.5, color = "grey60", face = "italic"),  
        plot.caption = element_text(hjust = 0, color = "black", size = 4.5),
        legend.box.margin = margin(t = 1),
        legend.spacing.x = unit(0, 'cm'),
        legend.box.spacing = unit(0, 'cm')
      ) +
      scale_x_discrete(labels = x_labels) +  # Use dynamic labels based on available waves
      scale_color_manual(values = custom_colors, breaks = behavior_order) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), 
                         limits = c(0, 80), 
                         breaks = seq(0, 100, by = 10),
                         expand = expansion(mult = c(0, 0.02))) +
      geom_hline(yintercept = c(10, 20, 30, 40, 50, 60, 70, 80, 90), 
                 linetype = "dashed", color = "grey40", alpha = 0.2) +
      ggtitle(paste("Average reported intention to adopt climate actions")) + 
      guides(color = guide_legend(nrow = 2, byrow = TRUE, 
                                  keywidth = unit(0.8, "lines"),
                                  keyheight = unit(0.8, "lines"))) + 
      labs(caption = paste0("Source: Climate Culture Index, Rare, ", max(unique_waves)))
    
    # Save the plot
    ggsave(paste0(m, ".jpg"), width = 5, height = 5, dpi = 600)
    
    p
  })
  
  # Return the plot
  plot_intention[[1]]
}