# ==============================================================================
# EIND 142 - Lab Session 1: Data Exploration & Pareto Analysis
# ==============================================================================
#
# Student Name: ____________________
# Date: ____________________
#
# INSTRUCTIONS:
# -------------
# 1. Read through each section carefully
# 2. Complete the TODO items (3) by filling in the missing code (replace ____)
# 3. Run each line/section with Ctrl+Enter (Positron/VS Code) or Cmd+Enter (Mac)
# 4. Answer the questions in a separate document (e.g., Word, text, or markdown file)
#
# LEARNING OBJECTIVES:
# - Explore and understand a real-world dataset structure
# - Construct Pareto charts for root cause analysis
# - Understand and apply the Pareto Principle (80/20 rule)
#
# DATA SOURCE: Kaggle F1 World Championship Dataset (1950-2024)
# https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020
# ==============================================================================


# ==============================================================================
# STEP 0: VERIFY R IS INSTALLED
# ==============================================================================
# Before continuing, let's make sure R is properly installed on your computer.
#
# Run the line below by placing your cursor on it and pressing:
#   - Ctrl+Enter (Windows/Linux)
#   - Cmd+Enter (Mac)
#
# If R is installed correctly, you'll see version information printed below.
# If you get an error or nothing happens, install R from: https://cran.r-project.org/
# ==============================================================================

R.version.string  # Run this line - you should see something like "R version 4.x.x"

# If you see output like "R version 4.4.1 (2024-06-14)" you're good to go!
# If not, stop here and install R before continuing.


# ==============================================================================
# PART 1: SETUP & DATA OVERVIEW
# ==============================================================================
#
# Before analyzing F1 data, let's set up our environment and understand
# what data we're working with. The dataset has been pre-filtered to the
# modern hybrid era (2014-2024) for meaningful engineering analysis.
# ==============================================================================

cat("======================================================================\n")
cat("PART 1: DATA EXPLORATION\n")
cat("======================================================================\n")

library(dplyr)

# ==============================================================================
# IMPORTANT: SET YOUR WORKING DIRECTORY
# ==============================================================================
# Before running this lab, make sure your working directory is set to the
# project root folder (the folder containing 'scripts/', 'data/', etc.)
#
# Option 1: Open this project folder in Positron (File > Open Folder)
#           The working directory is automatically set to the opened folder.
# Option 2: Use Command Palette (Ctrl+Shift+P): "Interpreter: Set as Working Directory"
# Option 3: Uncomment and modify the line below:
#
# setwd("/path_to_your/racing_lab")
#
# To check your current working directory, run: getwd()
# ==============================================================================

# Create output directory if it doesn't exist
if (!dir.exists("outputs")) dir.create("outputs")

# ==============================================================================
# OPTIONAL: DOWNLOAD FULL DATASET FROM KAGGLE
# ==============================================================================
# The data for this lab has been pre-filtered and included in the repository.
# If you want to explore the FULL F1 dataset (1950-2024), you can download it:
#
# 1. Create a free Kaggle account at https://www.kaggle.com
# 2. Download the dataset from:
#    https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020
# 3. Extract CSV files into: data/raw/
# 4. Uncomment the "FULL DATASET EXPLORATION" section below
# ==============================================================================

# --- FULL DATASET EXPLORATION (Instructor Demo / Optional) ---
# Uncomment this section if you downloaded the full Kaggle dataset to data/raw/
#
# races_raw <- read.csv("data/raw/races.csv", stringsAsFactors = FALSE)
# cat("\nRACES TABLE (Full Dataset):\n")
# cat("  Total races:", nrow(races_raw), "\n")
# cat("  Years covered:", min(races_raw$year), "-", max(races_raw$year), "\n")
#
# results_raw <- read.csv("data/raw/results.csv", stringsAsFactors = FALSE)
# cat("\nRESULTS TABLE (Full Dataset):\n")
# cat("  Total results:", nrow(results_raw), "\n")
#
# status_raw <- read.csv("data/raw/status.csv", stringsAsFactors = FALSE)
# cat("\nSTATUS TABLE:\n")
# print(head(status_raw, 10))
# --- END FULL DATASET EXPLORATION ---

# --- About the Data ---
#
# This lab uses F1 data from the "modern hybrid era" (2014-2024).
# We focus on this period because it represents a consistent technical
# regulation period with V6 turbo-hybrid engines, making comparisons meaningful.
# The full Kaggle dataset spans 1950-2024, but comparing modern data to
# 1950s data wouldn't be meaningful for engineering analysis.

cat("\n--- Data loaded (Hybrid Era 2014-2024) ---\n")


# ==============================================================================
# PART 2: PARETO ANALYSIS - ROOT CAUSE OF DNFs
# ==============================================================================
#
# A Pareto chart shows the 80/20 rule: roughly 80% of effects come from
# 20% of causes. We'll identify which failure modes cause the most DNFs.
# ==============================================================================

cat("\n======================================================================\n")
cat("PART 2: PARETO ANALYSIS - Root Cause of DNFs (Hybrid Era 2014-2024)\n")
cat("======================================================================\n")

# Load the backend library
source("scripts/f1_backend.R")

OUTPUT_DIR <- "outputs"

# Load DNF data
dnf_data <- get_dnf_data(start_year = 2014, end_year = 2024)

cat("\nTotal DNF incidents (2014-2024):", nrow(dnf_data), "\n")
cat("\nSample of DNF data:\n")
print(head(dnf_data, 10))

num_unique_reasons <- length(unique(dnf_data$reason))
cat("\nNumber of unique DNF reasons:", num_unique_reasons, "\n")

# TODO 1: Extract the list of DNF reasons from the data
# HINT: Use dnf_data$reason to get the column
dnf_reasons <- ____

# TODO 2: Calculate Pareto data using the calculate_pareto_data() function
# HINT: Pass the list of reasons to the function
pareto_data <- calculate_pareto_data(____)

cat("\n--------------------------------------------------\n")
cat("PARETO ANALYSIS RESULTS:\n")
cat("--------------------------------------------------\n")
print(pareto_data)

# Identify the "vital few" - categories that make up ~80% of issues
vital_few <- c()
for (i in 1:nrow(pareto_data)) {
  row <- pareto_data[i, ]
  if (row$cumulative_percentage <= 80) {
    vital_few <- c(vital_few, row$category)
  } else {
    vital_few <- c(vital_few, row$category)
    break
  }
}

cat("\n'Vital Few' failure modes (causing ~80% of DNFs):",
    paste(vital_few, collapse = ", "), "\n")
cat("Number of 'Vital Few' categories:", length(vital_few), "\n")

# TODO 3: Create the Pareto chart
# HINT: Use plot_pareto_chart(pareto_data, title, xlabel, ylabel)
fig_pareto <- plot_pareto_chart(
  ____,
  title = "Pareto Chart: F1 DNF Root Causes (Hybrid Era 2014-2024)",
  xlabel = "Failure Mode",
  ylabel = "Number of Incidents"
)
print(fig_pareto)
ggsave(file.path(OUTPUT_DIR, "pareto_dnf.png"), fig_pareto,
       width = 10, height = 6, dpi = 150)
cat("\nSaved: pareto_dnf.png\n")


# ==============================================================================
# QUESTION 1: PARETO ANALYSIS - ANSWER IN SECONDARY FILE (.docx/.txt/.md)
# ==============================================================================
#
# Q1.1: What are the "vital few" failure modes that F1 teams should **prioritize**?
#       Hint: Which failure modes can the team affect through engineering changes?
#       Answer: ________________________________________________________________
#
#       ________________________________________________________________________
#
# Q1.2: What percentage of DNFs does the top failure mode account for?
#
#       Answer: ________________________________________________________________
#
# Q1.3: How does the 80/20 rule apply to this F1 DNF data?
#
#       Answer: ________________________________________________________________
#
#       ________________________________________________________________________
#
# ==============================================================================


# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n======================================================================\n")
cat("SESSION 1 SUMMARY\n")
cat("======================================================================\n")

cat("
Summarize your findings:

1. Years in the full dataset: ____________________
2. Total DNFs in hybrid era: ____________________
3. Number of unique failure reasons: ____________________
4. Top failure mode: ____________________
5. Number of 'Vital Few' categories: ____________________
")

# Copy -> paste this summary into your secondary answer file.

cat("\n======================================================================\n")
cat("SESSION 1 COMPLETE!\n")
cat("======================================================================\n")
