# NextStepsQuant Data Analysis
# Script 3: Panel Line Plots by Participant
# Purpose: Create longitudinal trajectory visualizations

# Set working directory
setwd("/Users/jeremy/Library/CloudStorage/OneDrive-UW/Next Steps/Research/Quantitative/Data")

# Load required libraries
library(tidyverse)
library(ggplot2)
library(patchwork)

# Read in the data
data_path <- "data_DeID_clean_2026-04-06.csv"
df <- read_csv(data_path)

# Set output directory for saving visuals
output_dir <- "/Users/jeremy/Library/CloudStorage/OneDrive-UW/Next Steps/Research/Quantitative/Visuals"

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

# Function to create individual plots for a given variable and participant
create_individual_plot <- function(data, variable_name, participant_id) {
  # Filter for the specific participant
  data_subset <- data %>%
    filter(record_id == participant_id) %>%
    filter(!is.na(.data[[variable_name]])) %>%
    arrange(timepoint)
  
  if (nrow(data_subset) == 0) {
    return(NULL)
  }
  
  # Create plot
  p <- ggplot(data_subset, aes(x = timepoint, y = .data[[variable_name]])) +
    geom_line(aes(group = record_id), size = 0.8, alpha = 0.7, color = "steelblue") +
    geom_point(size = 2, color = "steelblue") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8)
    ) +
    labs(
      title = paste("Participant", participant_id),
      x = "Timepoint",
      y = variable_name
    )
  
  return(p)
}

# Function to create panel plots for a given variable
create_panel_plot <- function(data, variable_name, timepoint_cat) {
  # Filter for the specific timepoint category, remove NA values for the variable, and sort by timepoint
  data_subset <- data %>%
    filter(timepoint_category == timepoint_cat) %>%
    filter(!is.na(.data[[variable_name]])) %>%
    arrange(record_id, timepoint)
  
  # Get unique record_ids
  unique_records <- unique(data_subset$record_id)
  n_records <- length(unique_records)
  
  if (n_records == 0) {
    return(NULL)
  }
  
  # Create plot
  p <- ggplot(data_subset, aes(x = timepoint, y = .data[[variable_name]], color = factor(record_id))) +
    geom_line(aes(group = record_id), size = 0.8, alpha = 0.7) +
    geom_point(size = 2) +
    facet_wrap(~record_id, nrow = 1, scales = "free_y") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 10),
      strip.text = element_text(size = 9, face = "bold"),
      legend.position = "none"
    ) +
    labs(
      title = paste(variable_name, "-", timepoint_cat, "(n =", n_records, "participants)"),
      x = "Timepoint",
      y = variable_name
    )
  
  return(p)
}

# ============================================================================
# RAW DATA PLOTS - SEPARATE BY TIMEPOINT CATEGORY
# ============================================================================

cat("\n=== CREATING RAW DATA PLOTS ===\n")

# Create plots for each variable
for (var in cols_to_plot) {
  cat("\n=== Creating plots for:", var, "===\n")
  
  # Plot for 3+ timepoints
  p_3plus <- create_panel_plot(df_filtered, var, "3+ Timepoints")
  if (!is.null(p_3plus)) {
    filename <- file.path(output_dir, paste0("03_trajectories_", gsub(" ", "_", var), "_3plus_timepoints.png"))
    ggsave(filename, p_3plus, width = 14, height = 4, dpi = 300, bg = "white")
    cat("Saved:", filename, "\n")
    print(p_3plus)
  }
  
  # Plot for 2 timepoints
  p_2 <- create_panel_plot(df_filtered, var, "2 Timepoints")
  if (!is.null(p_2)) {
    filename <- file.path(output_dir, paste0("03_trajectories_", gsub(" ", "_", var), "_2_timepoints.png"))
    ggsave(filename, p_2, width = 14, height = 4, dpi = 300, bg = "white")
    cat("Saved:", filename, "\n")
    print(p_2)
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

# Create person-centered panel plots (3 plots wide layout)
for (var in cols_to_plot) {
  pc_var <- paste0(var, "_person_centered")
  cat("\n=== Creating person-centered panel plots for:", var, "===\n")
  
  # ============================================================================
  # Panel for 3+ Timepoints
  # ============================================================================
  
  data_3plus <- df_filtered %>%
    filter(timepoint_category == "3+ Timepoints") %>%
    filter(!is.na(.data[[pc_var]])) %>%
    arrange(record_id, timepoint)
  
  unique_records_3plus <- unique(data_3plus$record_id)
  n_records_3plus <- length(unique_records_3plus)
  
  if (n_records_3plus > 0) {
    # Create individual plots for each participant
    plot_list_3plus <- list()
    for (participant in unique_records_3plus) {
      p <- create_individual_plot(data_3plus, pc_var, participant)
      if (!is.null(p)) {
        plot_list_3plus[[as.character(participant)]] <- p
      }
    }
    
    # Determine number of rows needed for 3 plots per row
    n_rows <- ceiling(length(plot_list_3plus) / 3)
    
    # Combine plots with patchwork (3 plots per row)
    combined_3plus <- wrap_plots(plot_list_3plus, ncol = 3) +
      plot_annotation(
        title = paste("Person-Centered", var, "- 3+ Timepoints (n =", n_records_3plus, "participants)"),
        theme = theme(plot.title = element_text(size = 14, face = "bold"))
      )
    
    # Save plot
    filename <- file.path(output_dir, paste0("03_person_centered_", gsub(" ", "_", var), "_3plus_timepoints.png"))
    ggsave(filename, combined_3plus, width = 14, height = 4 * n_rows, dpi = 300, bg = "white")
    cat("Saved:", filename, "\n")
    print(combined_3plus)
  }
  
  # ============================================================================
  # Panel for 2 Timepoints
  # ============================================================================
  
  data_2 <- df_filtered %>%
    filter(timepoint_category == "2 Timepoints") %>%
    filter(!is.na(.data[[pc_var]])) %>%
    arrange(record_id, timepoint)
  
  unique_records_2 <- unique(data_2$record_id)
  n_records_2 <- length(unique_records_2)
  
  if (n_records_2 > 0) {
    # Create individual plots for each participant
    plot_list_2 <- list()
    for (participant in unique_records_2) {
      p <- create_individual_plot(data_2, pc_var, participant)
      if (!is.null(p)) {
        plot_list_2[[as.character(participant)]] <- p
      }
    }
    
    # Determine number of rows needed for 3 plots per row
    n_rows <- ceiling(length(plot_list_2) / 3)
    
    # Combine plots with patchwork (3 plots per row)
    combined_2 <- wrap_plots(plot_list_2, ncol = 3) +
      plot_annotation(
        title = paste("Person-Centered", var, "- 2 Timepoints (n =", n_records_2, "participants)"),
        theme = theme(plot.title = element_text(size = 14, face = "bold"))
      )
    
    # Save plot
    filename <- file.path(output_dir, paste0("03_person_centered_", gsub(" ", "_", var), "_2_timepoints.png"))
    ggsave(filename, combined_2, width = 14, height = 4 * n_rows, dpi = 300, bg = "white")
    cat("Saved:", filename, "\n")
    print(combined_2)
  }
  
  # ============================================================================
  # Average Person-Centered Plot (3+ Timepoints only)
  # ============================================================================
  
  cat("\n=== Creating average person-centered plot for:", var, "===\n")
  
  # Calculate average person-centered score at each timepoint (3+ timepoints only)
  avg_pc_by_timepoint <- df_filtered %>%
    filter(timepoint_category == "3+ Timepoints") %>%
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
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 13, face = "bold"),
      axis.title = element_text(size = 11),
      panel.grid.major = element_line(color = "gray90")
    ) +
    labs(
      title = paste("Average Person-Centered Score by Timepoint:", var, "(3+ Timepoints)"),
      x = "Timepoint",
      y = paste("Mean Person-Centered", var),
      subtitle = "Error bars show ±1 SE"
    )
  
  # Save plot
  filename <- file.path(output_dir, paste0("03_average_person_centered_", gsub(" ", "_", var), ".png"))
  ggsave(filename, p_avg, width = 8, height = 6, dpi = 300, bg = "white")
  cat("Saved:", filename, "\n")
  print(p_avg)
  
  # Print summary table
  cat("\nSummary statistics for", var, "(person-centered, 3+ timepoints):\n")
  print(avg_pc_by_timepoint)
}

cat("\n=== PERSON-CENTERED ANALYSIS COMPLETE ===\n")
