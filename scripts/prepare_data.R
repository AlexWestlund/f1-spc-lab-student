# ==============================================================================
# F1 Data Preparation Script
# ==============================================================================
# This script filters the raw Kaggle F1 dataset to the hybrid era (2014-2024)
#
# BEFORE RUNNING THIS SCRIPT:
# 1. Download the F1 dataset from Kaggle:
#    https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020
# 2. Extract the ZIP file contents into the data/raw/ folder
# 3. Run this script from the repo root directory
#
# WHY 2014-2024?
# The "hybrid era" began in 2014 when F1 introduced V6 turbo-hybrid power units.
# This represents a consistent technical regulation period, making comparisons
# more meaningful. Before 2014, cars used V8 naturally aspirated engines with
# very different reliability characteristics.
# ==============================================================================

library(dplyr)

# Configuration
RAW_DIR <- "data/raw"
OUTPUT_DIR <- "data"
START_YEAR <- 2014
END_YEAR <- 2024

cat(strrep("=", 62), "\n")
cat("F1 Data Preparation - Filtering to Hybrid Era (2014-2024)\n")
cat(strrep("=", 62), "\n\n")

# Check that raw data exists
if (!file.exists(file.path(RAW_DIR, "races.csv"))) {
  stop(paste0(
    "Raw data not found in ", RAW_DIR, "/\n",
    "Please download the F1 dataset from Kaggle and extract it to ", RAW_DIR, "/"
  ))
}

cat("Loading raw data from:", RAW_DIR, "\n")
cat("Output will be saved to:", OUTPUT_DIR, "\n")
cat("Filtering to years:", START_YEAR, "-", END_YEAR, "\n\n")

# --- Step 1: Load and filter races ---
cat("Step 1: Filtering races...\n")
races <- read.csv(file.path(RAW_DIR, "races.csv"), stringsAsFactors = FALSE)
races_filtered <- races %>% filter(year >= START_YEAR, year <= END_YEAR)
race_ids <- races_filtered$raceId

cat(sprintf("  Races: %d -> %d (%.0f%% of original)\n",
            nrow(races), nrow(races_filtered),
            nrow(races_filtered) / nrow(races) * 100))
write.csv(races_filtered, file.path(OUTPUT_DIR, "races.csv"), row.names = FALSE)

# --- Step 2: Filter lap times (largest file) ---
cat("\nStep 2: Filtering lap times (this may take a moment)...\n")
lap_times <- read.csv(file.path(RAW_DIR, "lap_times.csv"), stringsAsFactors = FALSE)
lap_times_filtered <- lap_times %>% filter(raceId %in% race_ids)
cat(sprintf("  Lap times: %d -> %d (%.0f%% of original)\n",
            nrow(lap_times), nrow(lap_times_filtered),
            nrow(lap_times_filtered) / nrow(lap_times) * 100))
write.csv(lap_times_filtered, file.path(OUTPUT_DIR, "lap_times.csv"), row.names = FALSE)

# --- Step 3: Filter pit stops ---
cat("\nStep 3: Filtering pit stops...\n")
pit_stops <- read.csv(file.path(RAW_DIR, "pit_stops.csv"), stringsAsFactors = FALSE)
pit_stops_filtered <- pit_stops %>% filter(raceId %in% race_ids)
cat(sprintf("  Pit stops: %d -> %d (%.0f%% of original)\n",
            nrow(pit_stops), nrow(pit_stops_filtered),
            nrow(pit_stops_filtered) / nrow(pit_stops) * 100))
write.csv(pit_stops_filtered, file.path(OUTPUT_DIR, "pit_stops.csv"), row.names = FALSE)

# --- Step 4: Filter results ---
cat("\nStep 4: Filtering results...\n")
results <- read.csv(file.path(RAW_DIR, "results.csv"), stringsAsFactors = FALSE)
results_filtered <- results %>% filter(raceId %in% race_ids)
cat(sprintf("  Results: %d -> %d (%.0f%% of original)\n",
            nrow(results), nrow(results_filtered),
            nrow(results_filtered) / nrow(results) * 100))
write.csv(results_filtered, file.path(OUTPUT_DIR, "results.csv"), row.names = FALSE)

# --- Step 5: Filter qualifying ---
cat("\nStep 5: Filtering qualifying...\n")
qualifying <- read.csv(file.path(RAW_DIR, "qualifying.csv"), stringsAsFactors = FALSE)
qualifying_filtered <- qualifying %>% filter(raceId %in% race_ids)
cat(sprintf("  Qualifying: %d -> %d (%.0f%% of original)\n",
            nrow(qualifying), nrow(qualifying_filtered),
            nrow(qualifying_filtered) / nrow(qualifying) * 100))
write.csv(qualifying_filtered, file.path(OUTPUT_DIR, "qualifying.csv"), row.names = FALSE)

# --- Step 6: Filter sprint results ---
cat("\nStep 6: Filtering sprint results...\n")
sprint_results <- read.csv(file.path(RAW_DIR, "sprint_results.csv"), stringsAsFactors = FALSE)
sprint_results_filtered <- sprint_results %>% filter(raceId %in% race_ids)
cat(sprintf("  Sprint results: %d -> %d\n",
            nrow(sprint_results), nrow(sprint_results_filtered)))
write.csv(sprint_results_filtered, file.path(OUTPUT_DIR, "sprint_results.csv"), row.names = FALSE)

# --- Step 7: Copy/filter reference tables ---
cat("\nStep 7: Processing reference tables...\n")

# Circuits - copy all
file.copy(file.path(RAW_DIR, "circuits.csv"),
          file.path(OUTPUT_DIR, "circuits.csv"), overwrite = TRUE)
cat("  Circuits: copied (all circuits retained)\n")

# Drivers - filter to those who raced in hybrid era
driver_ids <- unique(results_filtered$driverId)
drivers <- read.csv(file.path(RAW_DIR, "drivers.csv"), stringsAsFactors = FALSE)
drivers_filtered <- drivers %>% filter(driverId %in% driver_ids)
cat(sprintf("  Drivers: %d -> %d\n", nrow(drivers), nrow(drivers_filtered)))
write.csv(drivers_filtered, file.path(OUTPUT_DIR, "drivers.csv"), row.names = FALSE)

# Constructors - filter to those who raced in hybrid era
constructor_ids <- unique(results_filtered$constructorId)
constructors <- read.csv(file.path(RAW_DIR, "constructors.csv"), stringsAsFactors = FALSE)
constructors_filtered <- constructors %>% filter(constructorId %in% constructor_ids)
cat(sprintf("  Constructors: %d -> %d\n", nrow(constructors), nrow(constructors_filtered)))
write.csv(constructors_filtered, file.path(OUTPUT_DIR, "constructors.csv"), row.names = FALSE)

# Status - copy all (reference table for DNF reasons)
file.copy(file.path(RAW_DIR, "status.csv"),
          file.path(OUTPUT_DIR, "status.csv"), overwrite = TRUE)
cat("  Status: copied (DNF reason codes)\n")

# --- Summary ---
cat("\n", strrep("=", 62), "\n")
cat("DATA PREPARATION COMPLETE!\n")
cat(strrep("=", 62), "\n\n")

cat("File size comparison (raw vs filtered):\n")
for (f in c("lap_times.csv", "pit_stops.csv", "results.csv", "races.csv")) {
  raw_path <- file.path(RAW_DIR, f)
  filtered_path <- file.path(OUTPUT_DIR, f)
  if (file.exists(raw_path) && file.exists(filtered_path)) {
    raw_size <- file.size(raw_path) / 1024 / 1024
    filtered_size <- file.size(filtered_path) / 1024 / 1024
    reduction <- (1 - filtered_size / raw_size) * 100
    cat(sprintf("  %s: %.1f MB -> %.1f MB (%.0f%% smaller)\n",
                f, raw_size, filtered_size, reduction))
  }
}

cat("\nThe filtered data is now ready for analysis!\n")
cat("You can proceed with the lab exercises.\n")
