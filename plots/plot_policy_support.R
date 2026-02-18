# Plot: Policy Support
# Two-point trend analysis (2024-2025 only)

create_policy_support_plot <- function(df_summary, custom_colors) {
  
  # Policy Support
  measures_ordered <- c("Policy support")
  
  plot_policy_support <- lapply(1:length(measures_ordered), function(i) {
    m <- measures_ordered[i]
    
    # Filter using measure_label
    df_measure <- df_summary %>% 
      filter(measure_label == m)
    
    # Check if we have data for this measure
    if(nrow(df_measure) == 0) {
      cat("No data found for", m, "\n")
      return(NULL)
    }
    
    # Specify the order of behaviors as they should appear in the legend
    behavior_order <- c("Reduce food waste",
                        "Install solar panels",
                        "Sign up for community solar", 
                        "Install a heat pump",
                        "Drive an EV",
                        "Buy carbon offsets",
                        "Eat less beef")
    
    # Custom nudges for 2-point plots (adjusted for shorter x-axis)
    custom_nudges <- data.frame(
      behavior_label = behavior_order,
      nudge_x = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2),
      nudge_y = c(5, 4, 3, 2, 1, 0, -1)
    )
    
    # Filter data frame for first observation of each behavior (2024 in this case)
    first_observation <- df_measure %>%
      group_by(behavior_label) %>%
      filter(wave == min(wave)) %>%
      ungroup()
    
    p <- ggplot(df_measure, aes(x = factor(wave), y = mean, 
                                 group = behavior_label, 
                                 color = behavior_label)) +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                    width = 0.175, alpha = 0.45, 
                    position = position_dodge(width = 0.08)) +
      geom_line(position = position_dodge(width = 0.08), size = 1.2) +
      geom_point(size = 2.5, position = position_dodge(width = 0.08), alpha = 0.8)
    
    # Add individual geom_label_repel for each behavior with custom nudges
    for (j in seq_along(behavior_order)) {
      behavior_data <- first_observation %>% 
        filter(behavior_label == behavior_order[j])
      
      if(nrow(behavior_data) > 0) {
        p <- p + geom_label_repel(
          data = behavior_data,
          aes(label = behavior_label),
          nudge_x = custom_nudges$nudge_x[j],
          nudge_y = custom_nudges$nudge_y[j],
          box.padding = 0.35,
          point.padding = 0.5,
          fontface = "italic",
          size = 2.8,
          segment.alpha = 0.6
        )
      }
    }
    
    # Determine x-axis labels based on available waves
    unique_waves <- sort(unique(df_measure$wave))
    x_labels <- as.character(unique_waves)
    
    p <- p + labs(y = "") +
      theme_minimal() +
      theme(
        text = element_text(family = "Arial"),
        axis.text.x = element_text(vjust = 0.5, hjust = 1, color = "grey60", size = 8),
        axis.text.y = element_text(color = "grey60", size = 8),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.line = element_line(colour = "grey70"),
        plot.title = element_text(size = 10, color = "black", face = "bold"),  
        plot.subtitle = element_text(size = 7, color = "grey60", face = "italic"),  
        plot.caption = element_text(hjust = 0, color = "black", size = 5),
        legend.position = "none"
      ) +
      scale_x_discrete(labels = x_labels) +
      scale_color_manual(values = custom_colors, breaks = behavior_order) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), 
                         limits = c(0, NA), 
                         breaks = seq(0, 100, by = 20)) +
      geom_hline(yintercept = c(20, 40, 60, 80), 
                 linetype = "dashed", color = "grey40", alpha = 0.2) +
      ggtitle(paste("U.S. adult support for climate policies")) + 
      guides(color = guide_legend(nrow = 2)) + 
      labs(subtitle = "% of U.S. adults who support policies to promote climate behaviors",
           caption = paste0("Source: Climate Culture Index, Rare, ", paste(unique_waves, collapse = ", ")))
    
    ggsave(paste0(gsub(" ", "_", m), ".jpg"), width = 5, height = 5, dpi = 600)
    
    p
  })
  
  # Return the plot (handle case where no data)
  if(is.null(plot_policy_support[[1]])) {
    return(NULL)
  } else {
    return(plot_policy_support[[1]])
  }
}