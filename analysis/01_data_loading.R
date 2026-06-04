# NextStepsQuant Data Analysis
# Script 1: Data Loading
# Purpose: Load and prepare data for analysis

# Set working directory
setwd("/Users/jeremy/Library/CloudStorage/OneDrive-UW/Next Steps/Research/Quantitative/Data")

# Load required libraries
library(tidyverse)
library(ggplot2)
library(patchwork)

# Read in the data
data_path <- "data_DeID_clean_2026-04-06.csv"
df <- read_csv(data_path)

# Set project directory for saving outputs
project_dir <- getwd()

# Display basic info about the data
cat("Data loaded successfully!\n")
cat("Dimensions:", nrow(df), "rows,", ncol(df), "columns\n")
cat("Column names:\n")
print(names(df))
cat("\nFirst few rows:\n")
print(head(df))

# Create output directory if it doesn't exist
output_dir <- file.path(project_dir, "output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  cat("\nCreated output directory:", output_dir, "\n")
}
