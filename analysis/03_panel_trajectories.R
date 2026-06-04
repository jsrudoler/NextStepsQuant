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

# ============================================================================
# PERSON-CENTERED ANALYSIS
# ============================================================================

cat("\n=== GENERATING PERSON-CENTERED VISUALIZATIONS ===\n")

# Create person-centered versions of each variable
df_filtered <- df_filtered %>%
  group_by(record_id) %>%
  mutate(across(all_of(cols_to_plot), 
                list(person_centered = ~. - mean(., na.rm = TRUE)),
                .names = "{.col}_person_centered")) %>%
  ungroup()

# Create person-centered panel plots
for (var in cols_to_plot) {
  pc_var <- paste0(var, "_person_centered")
  cat("\n=== Creating person-centered panel plots for:", var, "===\n")
  
  # Plot for 3+ timepoints
  p1 <- create_panel_plot(df_filtered, pc_var, "3+ Timepoints")
  
  # Plot for 2 timepoints
  p2 <- create_panel_plot(df_filtered, pc_var, "2 Timepoints")
  
  # Combine plots vertically
  if (!is.null(p1) && !is.null(p2)) {
    combined_plot <- p1 / p2 + 
      plot_layout(heights = c(1, 1)) +
      plot_annotation(
        title = paste("Person-Centered Longitudinal Trajectories:", var),
        theme = theme(plot.title = element_text(size = 14, face = "bold"))
      )
  } else if (!is.null(p1)) {
    combined_plot <- p1 +
      plot_annotation(
        title = paste("Person-Centered Longitudinal Trajectories:", var),
        theme = theme(plot.title = element_text(size = 14, face = "bold"))
      )
  } else if (!is.null(p2)) {
    combined_plot <- p2 +
      plot_annotation(
        title = paste("Person-Centered Longitudinal Trajectories:", var),
        theme = theme(plot.title = element_text(size = 14, face = "bold"))
      )
  } else {
    combined_plot <- NULL
  }
  
  # Save plot
  if (!is.null(combined_plot)) {
    filename <- file.path(project_dir, "output", paste0("03_trajectories_person_centered_", gsub(" ", "_", var), ".png"))
    ggsave(filename, combined_plot, width = 14, height = 8, dpi = 300)
    cat("Saved:", filename, "\n")
    print(combined_plot)
  }
}

# Create average person-centered plots by timepoint
for (var in cols_to_plot) {
  pc_var <- paste0(var, "_person_centered")
  cat("\n=== Creating average person-centered plot for:", var, "===\n")
  
  # Calculate average person-centered score at each timepoint
  avg_pc_by_timepoint <- df_filtered %>%
    group_by(timepoint) %>%
    summarise(
      mean_pc_score = mean(.data[[pc_var]], na.rm = TRUE),
      se_pc_score = sd(.data[[pc_var]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[pc_var]]))),
      n_obs = sum(!is.na(.data[[pc_var]])),
      .groups = "drop"
    ) %>%
    arrange(timepoint)
  
  # Create average plot
  p_avg <- ggplot(avg_pc_by_timepoint, aes(x = timepoint, y = mean_pc_score)) +
    geom_line(size = 1, color = "steelblue") +
    geom_point(size = 3, color = "steelblue") +
    geom_errorbar(aes(ymin = mean_pc_score - se_pc_score, ymax = mean_pc_score + se_pc_score), 
                  width = 0.2, color = "steelblue", alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 13, face = "bold"),
      axis.title = element_text(size = 11),
      panel.grid.major = element_line(color = "gray90")
    ) +
    labs(
      title = paste("Average Person-Centered Score by Timepoint:", var),
      x = "Timepoint",
      y = paste("Mean Person-Centered", var),
      subtitle = "Error bars show ±1 SE"
    )
  
  # Save plot
  filename <- file.path(project_dir, "output", paste0("03_average_person_centered_", gsub(" ", "_", var), ".png"))
  ggsave(filename, p_avg, width = 8, height = 6, dpi = 300)
  cat("Saved:", filename, "\n")
  print(p_avg)
  
  # Print summary table
  cat("\nSummary statistics for", var, "(person-centered):\n")
  print(avg_pc_by_timepoint)
}

cat("\n=== PERSON-CENTERED ANALYSIS COMPLETE ===\n")
