# NextStepsQuant Data Analysis
# Script 3: Panel Line Plots by Participant
# Purpose: Create longitudinal trajectory visualizations

source("analysis/01_data_loading.R")

# Filter out participants with only 1 timepoint
df_filtered <- df %>%
  group_by(record_id) %>%
  mutate(n_timepoints = n()) %>%
  filter(n_timepoints >= 2) %>%
  ungroup()

# Categorize participants by number of timepoints
df_filtered <- df_filtered %>%
  mutate(timepoint_category = case_when(
    n_timepoints >= 3 ~ "3+ Timepoints",
    n_timepoints == 2 ~ "2 Timepoints",
    TRUE ~ "Other"
  ))

# Get columns to plot (all numeric columns after "Day of Assessment")
cols_to_plot <- c("bite_avg", "sip_sum", "mps_sum", "wccl_skills_mean", "WCCL_poor coping_Index")

# Function to create panel plots for a given variable
create_panel_plot <- function(data, variable_name, timepoint_cat) {
  # Filter for the specific timepoint category
  data_subset <- data %>%
    filter(timepoint_category == timepoint_cat) %>%
    arrange(record_id)
  
  # Get unique record_ids
  unique_records <- unique(data_subset$record_id)
  n_records <- length(unique_records)
  
  if (n_records == 0) {
    return(NULL)
  }
  
  # Create plot
  p <- ggplot(data_subset, aes(x = `Day of Assessment`, y = .data[[variable_name]], color = factor(record_id))) +
    geom_line(size = 0.8, alpha = 0.7) +
    geom_point(size = 2) +
    facet_wrap(~record_id, nrow = 1, scales = "free_y") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 10),
      strip.text = element_text(size = 9, face = "bold"),
      legend.position = "none"
    ) +
    labs(
      title = paste(variable_name, "-", timepoint_cat, "(n =", n_records, "participants)"),
      x = "Day of Assessment",
      y = variable_name
    )
  
  return(p)
}

# Create plots for each variable
for (var in cols_to_plot) {
  cat("\n=== Creating plots for:", var, "===\n")
  
  # Plot for 3+ timepoints
  p1 <- create_panel_plot(df_filtered, var, "3+ Timepoints")
  
  # Plot for 2 timepoints
  p2 <- create_panel_plot(df_filtered, var, "2 Timepoints")
  
  # Combine plots vertically
  if (!is.null(p1) && !is.null(p2)) {
    combined_plot <- p1 / p2 + 
      plot_layout(heights = c(1, 1)) +
      plot_annotation(
        title = paste("Longitudinal Trajectories:", var),
        theme = theme(plot.title = element_text(size = 14, face = "bold"))
      )
  } else if (!is.null(p1)) {
    combined_plot <- p1 +
      plot_annotation(
        title = paste("Longitudinal Trajectories:", var),
        theme = theme(plot.title = element_text(size = 14, face = "bold"))
      )
  } else if (!is.null(p2)) {
    combined_plot <- p2 +
      plot_annotation(
        title = paste("Longitudinal Trajectories:", var),
        theme = theme(plot.title = element_text(size = 14, face = "bold"))
      )
  } else {
    combined_plot <- NULL
  }
  
  # Save plot
  if (!is.null(combined_plot)) {
    filename <- file.path(project_dir, "output", paste0("03_trajectories_", gsub(" ", "_", var), ".png"))
    ggsave(filename, combined_plot, width = 14, height = 8, dpi = 300)
    cat("Saved:", filename, "\n")
    print(combined_plot)
  }
}

# Summary statistics
cat("\n=== Participation Summary ===\n")
summary_table <- df_filtered %>%
  group_by(record_id) %>%
  summarise(
    n_timepoints = n(),
    timepoint_category = first(timepoint_category),
    .groups = "drop"
  ) %>%
  arrange(desc(n_timepoints), record_id)

print(summary_table)
