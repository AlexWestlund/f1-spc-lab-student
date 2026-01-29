# ==============================================================================
# F1 Racing Lab - Backend Functions
# ==============================================================================
# This file contains helper functions for the F1 SPC Lab.
# Uses real data from the Kaggle F1 World Championship dataset.
# Students should NOT modify this file.
# ==============================================================================

# Load required packages (install if needed)
if (!require("ggplot2")) install.packages("ggplot2", repos = "https://cloud.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "https://cloud.r-project.org")
if (!require("tidyr")) install.packages("tidyr", repos = "https://cloud.r-project.org")

library(ggplot2)
library(dplyr)
library(tidyr)

# Create outputs directory if it doesn't exist
if (!dir.exists("outputs")) {
  dir.create("outputs")
}

# ==============================================================================
# DATA LOADING - Load pre-filtered CSV files (2014-2024 hybrid era)
# ==============================================================================

DATA_DIR <- "data"

cat("Loading F1 data (2014-2024 hybrid era)...\n")

# Load all data files
lap_times <- read.csv(file.path(DATA_DIR, "lap_times.csv"), stringsAsFactors = FALSE)
pit_stops <- read.csv(file.path(DATA_DIR, "pit_stops.csv"), stringsAsFactors = FALSE)
results <- read.csv(file.path(DATA_DIR, "results.csv"), stringsAsFactors = FALSE)
status <- read.csv(file.path(DATA_DIR, "status.csv"), stringsAsFactors = FALSE)
races <- read.csv(file.path(DATA_DIR, "races.csv"), stringsAsFactors = FALSE)
drivers <- read.csv(file.path(DATA_DIR, "drivers.csv"), stringsAsFactors = FALSE)
constructors <- read.csv(file.path(DATA_DIR, "constructors.csv"), stringsAsFactors = FALSE)

cat(sprintf("  Loaded %d lap time records\n", nrow(lap_times)))
cat(sprintf("  Loaded %d pit stop records\n", nrow(pit_stops)))
cat(sprintf("  Loaded %d race result records\n", nrow(results)))
cat(sprintf("  Loaded %d races\n", nrow(races)))
circuits <- read.csv(file.path(DATA_DIR, "circuits.csv"), stringsAsFactors = FALSE)
cat(sprintf("  Loaded %d circuits\n", nrow(circuits)))
cat("Data loaded successfully!\n\n")


# ==============================================================================
# DATA PREPARATION FUNCTIONS
# ==============================================================================

#' Get DNF (Did Not Finish) data for recent seasons
#' @param start_year First year to include (default 2020)
#' @param end_year Last year to include (default 2024)
#' @return Data frame with DNF incidents and their causes
get_dnf_data <- function(start_year = 2020, end_year = 2024) {
  # Status IDs that indicate the car finished (even if lapped)
  finished_statuses <- c(1, 11:19, 45, 50, 53, 55, 58, 88, 111:128)

  # Get races in the date range
  race_ids <- races %>%
    filter(year >= start_year, year <= end_year) %>%
    pull(raceId)

  # Get DNF results
  dnf_data <- results %>%
    filter(raceId %in% race_ids, !statusId %in% finished_statuses) %>%
    left_join(status, by = "statusId") %>%
    left_join(races %>% select(raceId, year, name), by = "raceId") %>%
    left_join(drivers %>% select(driverId, surname, code), by = "driverId") %>%
    select(year, race = name, driver = surname, code, reason = status, laps)

  dnf_data
}


#' Get pit stop data for a specific race or season
#' @param race_id Specific race ID (optional)
#' @param year Season year (optional, used if race_id is NULL)
#' @return Data frame with pit stop data
get_pit_stop_data <- function(race_id = NULL, year = NULL) {
  if (!is.null(race_id)) {
    pit_data <- pit_stops %>%
      filter(raceId == race_id)
  } else if (!is.null(year)) {
    race_ids <- races %>%
      filter(year == !!year) %>%
      pull(raceId)
    pit_data <- pit_stops %>%
      filter(raceId %in% race_ids)
  } else {
    # Default to 2023-2024
    race_ids <- races %>%
      filter(year >= 2023) %>%
      pull(raceId)
    pit_data <- pit_stops %>%
      filter(raceId %in% race_ids)
  }

  # Add race and driver names
  pit_data <- pit_data %>%
    left_join(races %>% select(raceId, race_name = name, year), by = "raceId") %>%
    left_join(drivers %>% select(driverId, driver = surname, code), by = "driverId")

  # Calculate duration in seconds and classify pit stops
  pit_data <- pit_data %>%
    mutate(
      duration_sec = milliseconds / 1000,
      # Classify pit stops by duration
      category = case_when(
        duration_sec < 25 ~ "Fast (<25s)",
        duration_sec < 28 ~ "Normal (25-28s)",
        duration_sec < 32 ~ "Slow (28-32s)",
        duration_sec < 40 ~ "Problem (32-40s)",
        TRUE ~ "Major Issue (>40s)"
      )
    )

  pit_data
}


#' Get track-normalized pit stop data
#' Calculates z-scores based on historical pit stop times at each circuit
#' @param year Year of interest
#' @param lookback_years Number of years to look back for baseline (default 5)
#' @return Data frame with pit stops and z-scores
get_normalized_pit_stops <- function(year = 2023, lookback_years = 5) {
  # Get races for the year of interest
  year_races <- races %>%
    filter(year == !!year) %>%
    select(raceId, circuitId, race_name = name, year)

  # Get historical races for each circuit (last N years including current)
  start_year <- year - lookback_years + 1
  historical_races <- races %>%
    filter(year >= start_year, year <= !!year) %>%
    select(raceId, circuitId, year)

  # Get all pit stops from historical races
  historical_pit_stops <- pit_stops %>%
    inner_join(historical_races, by = "raceId") %>%
    mutate(duration_sec = milliseconds / 1000) %>%
    # Filter out obvious outliers (pit lane issues, penalties, etc.)
    filter(duration_sec >= 15, duration_sec <= 60)

  # Calculate mean and SD for each circuit
  circuit_stats <- historical_pit_stops %>%
    group_by(circuitId) %>%
    summarize(
      circuit_mean = mean(duration_sec, na.rm = TRUE),
      circuit_sd = sd(duration_sec, na.rm = TRUE),
      circuit_n = n(),
      .groups = "drop"
    )

  # Get pit stops for the year of interest
  year_pit_stops <- pit_stops %>%
    inner_join(year_races, by = "raceId") %>%
    left_join(drivers %>% select(driverId, driver = surname, code), by = "driverId") %>%
    mutate(duration_sec = milliseconds / 1000) %>%
    filter(duration_sec >= 15, duration_sec <= 60)

  # Join with circuit stats and calculate z-scores
  normalized_data <- year_pit_stops %>%
    left_join(circuit_stats, by = "circuitId") %>%
    mutate(
      z_score = (duration_sec - circuit_mean) / circuit_sd,
      # Categorize based on z-score
      category = case_when(
        z_score < -1.5 ~ "Excellent (< -1.5 SD)",
        z_score < -0.5 ~ "Good (-1.5 to -0.5 SD)",
        z_score <= 0.5 ~ "Normal (-0.5 to +0.5 SD)",
        z_score <= 1.5 ~ "Slow (+0.5 to +1.5 SD)",
        TRUE ~ "Problem (> +1.5 SD)"
      )
    )

  normalized_data
}


#' Get pit stop statistics for a specific race
#' @param race_id Race ID
#' @return List with mean, sd, race name, and pit stop data
get_race_pit_stats <- function(race_id) {
  # Get race info
  race_info <- races %>%
    filter(raceId == race_id) %>%
    select(raceId, year, race_name = name)

  # Get pit stops for this race
  race_pit_stops <- pit_stops %>%
    filter(raceId == race_id) %>%
    left_join(drivers %>% select(driverId, driver = surname), by = "driverId") %>%
    mutate(duration_sec = milliseconds / 1000) %>%
    filter(duration_sec >= 15, duration_sec <= 60)  # Filter obvious outliers

  list(
    mean = mean(race_pit_stops$duration_sec, na.rm = TRUE),
    sd = sd(race_pit_stops$duration_sec, na.rm = TRUE),
    n = nrow(race_pit_stops),
    race_name = race_info$race_name,
    year = race_info$year,
    data = race_pit_stops
  )
}


#' Plot pit stop on normal distribution
#' Shows where a pit stop falls relative to the track's historical distribution
#' @param pit_stop_time The pit stop duration in seconds
#' @param circuit_mean Mean pit stop time at this circuit
#' @param circuit_sd Standard deviation at this circuit
#' @param title Plot title
#' @return ggplot object
plot_pit_stop_distribution <- function(pit_stop_time, circuit_mean, circuit_sd,
                                        title = "Pit Stop vs Track Average") {
  # Generate normal distribution curve
  x_range <- seq(circuit_mean - 4*circuit_sd, circuit_mean + 4*circuit_sd, length.out = 200)
  y_values <- dnorm(x_range, mean = circuit_mean, sd = circuit_sd)

  dist_df <- data.frame(x = x_range, y = y_values)

  # Calculate z-score for the pit stop
  z_score <- (pit_stop_time - circuit_mean) / circuit_sd
  y_at_pit <- dnorm(pit_stop_time, mean = circuit_mean, sd = circuit_sd)

  # Determine category and color
  if (z_score < -1.5) {
    category <- "Excellent"
    color <- "darkgreen"
  } else if (z_score < -0.5) {
    category <- "Good"
    color <- "green"
  } else if (z_score <= 0.5) {
    category <- "Normal"
    color <- "steelblue"
  } else if (z_score <= 1.5) {
    category <- "Slow"
    color <- "orange"
  } else {
    category <- "Problem"
    color <- "red"
  }

  ggplot(dist_df, aes(x = x, y = y)) +
    # Shade regions
    geom_area(data = subset(dist_df, x < circuit_mean - 1.5*circuit_sd),
              fill = "darkgreen", alpha = 0.3) +
    geom_area(data = subset(dist_df, x >= circuit_mean - 1.5*circuit_sd & x < circuit_mean - 0.5*circuit_sd),
              fill = "green", alpha = 0.3) +
    geom_area(data = subset(dist_df, x >= circuit_mean - 0.5*circuit_sd & x <= circuit_mean + 0.5*circuit_sd),
              fill = "steelblue", alpha = 0.3) +
    geom_area(data = subset(dist_df, x > circuit_mean + 0.5*circuit_sd & x <= circuit_mean + 1.5*circuit_sd),
              fill = "orange", alpha = 0.3) +
    geom_area(data = subset(dist_df, x > circuit_mean + 1.5*circuit_sd),
              fill = "red", alpha = 0.3) +
    # Distribution curve
    geom_line(linewidth = 1) +
    # Mean line
    geom_vline(xintercept = circuit_mean, linetype = "dashed", color = "black", linewidth = 0.8) +
    # Pit stop marker
    geom_segment(aes(x = pit_stop_time, xend = pit_stop_time, y = 0, yend = y_at_pit),
                 color = color, linewidth = 1.5) +
    geom_point(aes(x = pit_stop_time, y = y_at_pit), color = color, size = 4) +
    # Annotations
    annotate("text", x = circuit_mean, y = max(y_values) * 1.1,
             label = sprintf("Track Avg: %.1fs", circuit_mean), hjust = 0.5, size = 3.5) +
    annotate("text", x = pit_stop_time, y = y_at_pit + max(y_values) * 0.1,
             label = sprintf("%.1fs\n(z = %.2f)\n%s", pit_stop_time, z_score, category),
             hjust = 0.5, size = 3, color = color, fontface = "bold") +
    # Labels
    labs(
      title = title,
      subtitle = sprintf("Based on %d years of historical data at this circuit", 5),
      x = "Pit Stop Duration (seconds)",
      y = "Probability Density"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray50")
    )
}


#' Plot specific pit stops on normal distribution with labels
#' Shows where specific pit stops fall relative to the track's historical distribution
#' @param pit_stops Data frame with specific pit stops to highlight (must have driver, duration_sec columns)
#' @param circuit_mean Mean pit stop time at this circuit
#' @param circuit_sd Standard deviation at this circuit
#' @param title Plot title
#' @param circuit_name Name of circuit for subtitle
#' @return ggplot object
plot_labeled_pit_stops <- function(pit_stops, circuit_mean, circuit_sd,
                                    title = "Pit Stop Performance",
                                    circuit_name = "Circuit") {
  # Generate normal distribution curve
  x_range <- seq(circuit_mean - 4*circuit_sd, circuit_mean + 4*circuit_sd, length.out = 200)
  y_values <- dnorm(x_range, mean = circuit_mean, sd = circuit_sd)
  max_y <- max(y_values)

  dist_df <- data.frame(x = x_range, y = y_values)

  # Calculate z-scores and y positions for each pit stop
  pit_stops <- pit_stops %>%
    mutate(
      z_score = (duration_sec - circuit_mean) / circuit_sd,
      y_pos = dnorm(duration_sec, mean = circuit_mean, sd = circuit_sd),
      category = case_when(
        z_score < -1.5 ~ "Excellent",
        z_score < -0.5 ~ "Good",
        z_score <= 0.5 ~ "Normal",
        z_score <= 1.5 ~ "Slow",
        TRUE ~ "Problem"
      ),
      color = case_when(
        z_score < -1.5 ~ "darkgreen",
        z_score < -0.5 ~ "green3",
        z_score <= 0.5 ~ "steelblue",
        z_score <= 1.5 ~ "orange",
        TRUE ~ "red"
      ),
      label = sprintf("%s\n%.1fs (z=%.2f)", driver, duration_sec, z_score)
    )

  # Stagger label positions to avoid overlap
  pit_stops <- pit_stops %>%
    arrange(duration_sec) %>%
    mutate(
      label_y = max_y * (0.6 + 0.12 * (row_number() %% 4))
    )

  p <- ggplot(dist_df, aes(x = x, y = y)) +
    # Shade regions with category labels
    geom_area(data = subset(dist_df, x < circuit_mean - 1.5*circuit_sd),
              fill = "darkgreen", alpha = 0.3) +
    geom_area(data = subset(dist_df, x >= circuit_mean - 1.5*circuit_sd & x < circuit_mean - 0.5*circuit_sd),
              fill = "green3", alpha = 0.3) +
    geom_area(data = subset(dist_df, x >= circuit_mean - 0.5*circuit_sd & x <= circuit_mean + 0.5*circuit_sd),
              fill = "steelblue", alpha = 0.3) +
    geom_area(data = subset(dist_df, x > circuit_mean + 0.5*circuit_sd & x <= circuit_mean + 1.5*circuit_sd),
              fill = "orange", alpha = 0.3) +
    geom_area(data = subset(dist_df, x > circuit_mean + 1.5*circuit_sd),
              fill = "red", alpha = 0.3) +
    # Distribution curve
    geom_line(linewidth = 1.2) +
    # Mean line
    geom_vline(xintercept = circuit_mean, linetype = "dashed", color = "black", linewidth = 1)

  # Add each pit stop as a labeled marker
  for (i in 1:nrow(pit_stops)) {
    ps <- pit_stops[i, ]
    p <- p +
      # Vertical line from x-axis to curve
      annotate("segment", x = ps$duration_sec, xend = ps$duration_sec, y = 0, yend = ps$y_pos,
               color = ps$color, linewidth = 2, alpha = 0.8) +
      # Point on curve
      annotate("point", x = ps$duration_sec, y = ps$y_pos,
               color = ps$color, size = 5) +
      # Label with driver name and time
      annotate("label", x = ps$duration_sec, y = ps$label_y,
               label = ps$label, fill = "white", color = ps$color,
               fontface = "bold", size = 3, label.padding = unit(0.3, "lines"))
  }

  # Add category labels at top
  p <- p +
    annotate("text", x = circuit_mean - 2*circuit_sd, y = max_y * 1.15,
             label = "Excellent", color = "darkgreen", size = 3, fontface = "bold") +
    annotate("text", x = circuit_mean - 1*circuit_sd, y = max_y * 1.15,
             label = "Good", color = "green3", size = 3, fontface = "bold") +
    annotate("text", x = circuit_mean, y = max_y * 1.15,
             label = "Normal", color = "steelblue", size = 3, fontface = "bold") +
    annotate("text", x = circuit_mean + 1*circuit_sd, y = max_y * 1.15,
             label = "Slow", color = "orange", size = 3, fontface = "bold") +
    annotate("text", x = circuit_mean + 2*circuit_sd, y = max_y * 1.15,
             label = "Problem", color = "red", size = 3, fontface = "bold") +
    # Mean annotation
    annotate("text", x = circuit_mean, y = max_y * 0.05,
             label = sprintf("Track Avg: %.1fs", circuit_mean),
             hjust = 0.5, vjust = 0, size = 3.5, fontface = "italic") +
    # Labels
    labs(
      title = title,
      subtitle = sprintf("%s - Based on 5 years of historical pit stop data (SD = %.2fs)",
                         circuit_name, circuit_sd),
      x = "Pit Stop Duration (seconds)",
      y = "Probability Density"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"),
      axis.title = element_text(size = 10)
    ) +
    coord_cartesian(ylim = c(0, max_y * 1.25))

  p
}


#' Plot histogram of pit stops with z-score categories
#' @param pit_data Normalized pit stop data from get_normalized_pit_stops()
#' @param title Plot title
#' @return ggplot object
plot_pit_stop_zscore_distribution <- function(pit_data, title = "Pit Stop Performance Distribution") {
  # Set factor levels for proper ordering
  pit_data$category <- factor(pit_data$category, levels = c(
    "Excellent (< -1.5 SD)",
    "Good (-1.5 to -0.5 SD)",
    "Normal (-0.5 to +0.5 SD)",
    "Slow (+0.5 to +1.5 SD)",
    "Problem (> +1.5 SD)"
  ))

  ggplot(pit_data, aes(x = z_score, fill = category)) +
    geom_histogram(binwidth = 0.25, color = "white", alpha = 0.8) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    geom_vline(xintercept = c(-1.5, -0.5, 0.5, 1.5), linetype = "dotted", color = "gray50") +
    scale_fill_manual(values = c(
      "Excellent (< -1.5 SD)" = "darkgreen",
      "Good (-1.5 to -0.5 SD)" = "green",
      "Normal (-0.5 to +0.5 SD)" = "steelblue",
      "Slow (+0.5 to +1.5 SD)" = "orange",
      "Problem (> +1.5 SD)" = "red"
    )) +
    labs(
      title = title,
      subtitle = "Pit stops normalized to each track's historical average",
      x = "Z-Score (standard deviations from track mean)",
      y = "Count",
      fill = "Category"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray50"),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(nrow = 2))
}


#' Get lap time data for a specific driver in a specific race
#' @param race_id Race ID
#' @param driver_id Driver ID (optional, if NULL returns all drivers)
#' @return Data frame with lap times
get_lap_time_data <- function(race_id, driver_id = NULL) {
  lap_data <- lap_times %>%
    filter(raceId == race_id)

  if (!is.null(driver_id)) {
    lap_data <- lap_data %>%
      filter(driverId == driver_id)
  }

  # Add driver info and convert time to seconds
  lap_data <- lap_data %>%
    left_join(drivers %>% select(driverId, driver = surname, code), by = "driverId") %>%
    mutate(lap_time_sec = milliseconds / 1000) %>%
    arrange(driverId, lap)

  lap_data
}


#' Get race info by ID or name pattern
#' @param race_id Race ID (optional)
#' @param year Year (optional)
#' @param name_pattern Pattern to match race name (optional)
#' @return Data frame with race info
get_race_info <- function(race_id = NULL, year = NULL, name_pattern = NULL) {
  race_data <- races

  if (!is.null(race_id)) {
    race_data <- race_data %>% filter(raceId == race_id)
  }
  if (!is.null(year)) {
    race_data <- race_data %>% filter(year == !!year)
  }
  if (!is.null(name_pattern)) {
    race_data <- race_data %>% filter(grepl(name_pattern, name, ignore.case = TRUE))
  }

  race_data %>% select(raceId, year, round, name, date)
}


#' Organize lap times into real stints based on pit stops
#' @param lap_data Data frame with lap times for a single driver
#' @param pit_laps Vector of lap numbers when driver pitted (from pit stop data)
#' @param exclude_io Whether to exclude in-laps and out-laps (default TRUE)
#' @return List with stint data frames and summary info
organize_into_stints <- function(lap_data, pit_laps, exclude_io = TRUE) {
  # Sort pit laps
  pit_laps <- sort(pit_laps)

  # Create stint boundaries
  # Stint 1: lap 1 to first pit lap
  # Stint 2: lap after first pit to second pit lap
  # etc.
  total_laps <- max(lap_data$lap)

  boundaries <- c(0, pit_laps, total_laps + 1)

  stints <- list()
  stint_info <- data.frame(
    stint = integer(),
    start_lap = integer(),
    end_lap = integer(),
    n_laps = integer(),
    mean_time = numeric(),
    range_time = numeric()
  )

  for (i in 1:(length(boundaries) - 1)) {
    start_lap <- boundaries[i] + 1
    end_lap <- boundaries[i + 1] - 1

    if (exclude_io) {
      # Exclude out-lap (first lap of stint, except stint 1)
      if (i > 1) start_lap <- start_lap + 1
      # Exclude in-lap (last lap before pit)
      if (i < length(boundaries) - 1) end_lap <- end_lap
      # Actually the pit lap IS the in-lap, so end_lap is already correct
    }

    # Get laps for this stint
    stint_laps <- lap_data %>%
      filter(lap >= start_lap, lap <= end_lap)

    if (nrow(stint_laps) >= 3) {  # Need at least 3 laps for meaningful analysis
      stints[[i]] <- stint_laps

      stint_info <- rbind(stint_info, data.frame(
        stint = i,
        start_lap = start_lap,
        end_lap = end_lap,
        n_laps = nrow(stint_laps),
        mean_time = mean(stint_laps$lap_time_sec),
        range_time = max(stint_laps$lap_time_sec) - min(stint_laps$lap_time_sec)
      ))
    }
  }

  list(
    stints = stints,
    info = stint_info,
    pit_laps = pit_laps
  )
}


#' Get pit stop laps for a driver in a race
#' @param race_id Race ID
#' @param driver_id Driver ID
#' @return Vector of lap numbers when driver pitted
get_pit_laps <- function(race_id, driver_id) {
  pit_data <- pit_stops %>%
    filter(raceId == race_id, driverId == driver_id) %>%
    arrange(stop)

  pit_data$lap
}


#' Get a team's pit stops for a season with z-scores normalized per race
#' Each pit stop's z-score is calculated relative to all pit stops in that race
#' @param constructor_id Constructor/team ID (e.g., 9 = Red Bull)
#' @param year Season year (default 2024)
#' @return List with pit stop data and organized subgroups by race
get_team_season_pit_zscores <- function(constructor_id, year = 2024) {
  # Get races for the year
  year_races <- races %>%
    filter(year == !!year) %>%
    select(raceId, round, race_name = name, race_date = date)

  # Get team's driver IDs from results (drivers who raced for this team this year)
  team_drivers <- results %>%
    filter(raceId %in% year_races$raceId, constructorId == constructor_id) %>%
    distinct(driverId) %>%
    pull(driverId)

  # Get all pit stops for the year (to calculate race means/SDs)
  all_pits <- pit_stops %>%
    filter(raceId %in% year_races$raceId) %>%
    mutate(duration_sec = milliseconds / 1000) %>%
    filter(duration_sec >= 15, duration_sec <= 60)  # Filter outliers

  # Calculate race-level statistics
  race_stats <- all_pits %>%
    group_by(raceId) %>%
    summarize(
      race_mean = mean(duration_sec, na.rm = TRUE),
      race_sd = sd(duration_sec, na.rm = TRUE),
      race_n = n(),
      .groups = "drop"
    )

  # Get team's pit stops and calculate z-scores
  team_pits <- all_pits %>%
    filter(driverId %in% team_drivers) %>%
    left_join(race_stats, by = "raceId") %>%
    left_join(year_races, by = "raceId") %>%
    left_join(drivers %>% select(driverId, driver = surname), by = "driverId") %>%
    mutate(
      z_score = (duration_sec - race_mean) / race_sd
    ) %>%
    arrange(round, stop)

  # Organize into subgroups by race
  subgroups <- list()
  subgroup_info <- data.frame(
    subgroup = integer(),
    race_name = character(),
    round = integer(),
    race_date = character(),
    n_stops = integer(),
    mean_zscore = numeric(),
    range_zscore = numeric(),
    stringsAsFactors = FALSE
  )

  race_rounds <- sort(unique(team_pits$round))

  for (i in seq_along(race_rounds)) {
    r <- race_rounds[i]
    race_pits <- team_pits %>% filter(round == r)

    if (nrow(race_pits) >= 2) {  # Need at least 2 for range calculation
      subgroups[[i]] <- race_pits

      subgroup_info <- rbind(subgroup_info, data.frame(
        subgroup = i,
        race_name = race_pits$race_name[1],
        round = r,
        race_date = race_pits$race_date[1],
        n_stops = nrow(race_pits),
        mean_zscore = mean(race_pits$z_score, na.rm = TRUE),
        range_zscore = max(race_pits$z_score, na.rm = TRUE) - min(race_pits$z_score, na.rm = TRUE),
        stringsAsFactors = FALSE
      ))
    }
  }

  # Get team name
  team_name <- constructors %>%
    filter(constructorId == constructor_id) %>%
    pull(name)

  list(
    all_data = team_pits,
    subgroups = subgroups,
    info = subgroup_info,
    team_name = team_name,
    year = year,
    n_races = length(subgroups)
  )
}


# ==============================================================================
# PARETO ANALYSIS FUNCTIONS
# ==============================================================================

#' Calculate Pareto data from a vector of categories
#' @param categories Vector of category values
#' @return Data frame with counts, percentages, and cumulative values
calculate_pareto_data <- function(categories) {
  # Count occurrences
  counts <- table(categories)

  # Create data frame sorted by count (descending)
  pareto_df <- data.frame(
    category = names(counts),
    count = as.numeric(counts),
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(count)) %>%
    mutate(
      percentage = count / sum(count) * 100,
      cumulative_count = cumsum(count),
      cumulative_percentage = cumsum(percentage)
    )

  pareto_df
}


#' Create a Pareto chart
#' @param pareto_data Data frame from calculate_pareto_data
#' @param title Chart title
#' @param xlabel X-axis label
#' @param ylabel Y-axis label
plot_pareto_chart <- function(pareto_data, title = "Pareto Chart",
                               xlabel = "Category", ylabel = "Count") {
  # Set factor levels to maintain sort order
  pareto_data$category <- factor(pareto_data$category, levels = pareto_data$category)

  # Calculate scaling factor for secondary axis
  max_count <- max(pareto_data$count)
  scale_factor <- max_count / 100

  ggplot(pareto_data, aes(x = category)) +
    # Bars for counts
    geom_bar(aes(y = count), stat = "identity", fill = "steelblue", alpha = 0.7) +
    # Line for cumulative percentage
    geom_line(aes(y = cumulative_percentage * scale_factor, group = 1),
              color = "red", linewidth = 1) +
    geom_point(aes(y = cumulative_percentage * scale_factor),
               color = "red", size = 3) +
    # 80% reference line
    geom_hline(yintercept = 80 * scale_factor, linetype = "dashed",
               color = "darkred", alpha = 0.7) +
    # Secondary axis
    scale_y_continuous(
      name = ylabel,
      sec.axis = sec_axis(~ . / scale_factor, name = "Cumulative %")
    ) +
    labs(title = title, x = xlabel) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
}


# ==============================================================================
# CONTROL CHART FUNCTIONS
# ==============================================================================

#' Calculate subgroup means (X-bar values)
#' @param data Matrix where rows are subgroups, columns are observations
#' @return Vector of subgroup means
calculate_subgroup_means <- function(data) {
  apply(data, 1, mean)
}


#' Calculate subgroup ranges (R values)
#' @param data Matrix where rows are subgroups, columns are observations
#' @return Vector of subgroup ranges
calculate_subgroup_ranges <- function(data) {
  apply(data, 1, function(x) max(x) - min(x))
}


#' Calculate grand mean (X-double-bar)
#' @param x_bars Vector of subgroup means
#' @return Grand mean value
calculate_grand_mean <- function(x_bars) {
  mean(x_bars)
}


#' Calculate average range (R-bar)
#' @param ranges Vector of subgroup ranges
#' @return Average range value
calculate_average_range <- function(ranges) {
  mean(ranges)
}


#' Get control chart constants for a given subgroup size
#' @param n Subgroup size (2-40)
#' @return Data frame with A2, D3, D4, d2 constants
get_control_constants <- function(n) {
  # Control chart constants table (n = 2 to 40)
  # Extended to support larger subgroup sizes like F1 stint lengths
  # Source: Standard SPC tables (ASTM E2587, Montgomery's Introduction to SPC)
  constants <- data.frame(
    n = 2:40,
    A2 = c(1.880, 1.023, 0.729, 0.577, 0.483, 0.419, 0.373, 0.337, 0.308,  # 2-10
           0.285, 0.266, 0.249, 0.235, 0.223, 0.212, 0.203, 0.194, 0.187,  # 11-19
           0.180, 0.173, 0.167, 0.162, 0.157, 0.153,                       # 20-25
           0.148, 0.144, 0.141, 0.137, 0.134,                              # 26-30
           0.131, 0.128, 0.125, 0.122, 0.120,                              # 31-35
           0.118, 0.116, 0.114, 0.112, 0.110),                             # 36-40
    D3 = c(0, 0, 0, 0, 0, 0.076, 0.136, 0.184, 0.223,                       # 2-10
           0.256, 0.283, 0.307, 0.328, 0.347, 0.363, 0.378, 0.391, 0.404,   # 11-19
           0.415, 0.425, 0.435, 0.443, 0.452, 0.459,                        # 20-25
           0.466, 0.473, 0.479, 0.485, 0.491,                               # 26-30
           0.496, 0.501, 0.506, 0.510, 0.514,                               # 31-35
           0.518, 0.522, 0.525, 0.529, 0.532),                              # 36-40
    D4 = c(3.267, 2.574, 2.282, 2.114, 2.004, 1.924, 1.864, 1.816, 1.777,   # 2-10
           1.744, 1.717, 1.693, 1.672, 1.653, 1.637, 1.622, 1.609, 1.596,   # 11-19
           1.585, 1.575, 1.565, 1.557, 1.548, 1.541,                        # 20-25
           1.534, 1.527, 1.521, 1.515, 1.509,                               # 26-30
           1.504, 1.499, 1.494, 1.490, 1.486,                               # 31-35
           1.482, 1.478, 1.475, 1.471, 1.468),                              # 36-40
    d2 = c(1.128, 1.693, 2.059, 2.326, 2.534, 2.704, 2.847, 2.970, 3.078,   # 2-10
           3.173, 3.258, 3.336, 3.407, 3.472, 3.532, 3.588, 3.640, 3.689,   # 11-19
           3.735, 3.778, 3.819, 3.858, 3.895, 3.931,                        # 20-25
           3.964, 3.997, 4.027, 4.057, 4.086,                               # 26-30
           4.113, 4.139, 4.165, 4.189, 4.213,                               # 31-35
           4.236, 4.259, 4.280, 4.301, 4.322)                               # 36-40
  )

  if (n < 2 || n > 40) {
    stop("Subgroup size must be between 2 and 40")
  }

  constants[constants$n == n, ]
}


#' Calculate control limits for X-bar and R charts
#' @param x_double_bar Grand mean
#' @param r_bar Average range
#' @param subgroup_size Number of observations per subgroup
#' @return List with UCL, LCL for both charts and constants used
calculate_control_limits <- function(x_double_bar, r_bar, subgroup_size) {
  constants <- get_control_constants(subgroup_size)

  list(
    # X-bar chart limits
    UCL_xbar = x_double_bar + constants$A2 * r_bar,
    LCL_xbar = x_double_bar - constants$A2 * r_bar,

    # R chart limits
    UCL_R = constants$D4 * r_bar,
    LCL_R = constants$D3 * r_bar,

    # Process standard deviation estimate
    sigma_hat = r_bar / constants$d2,

    # Constants used
    A2 = constants$A2,
    D3 = constants$D3,
    D4 = constants$D4,
    d2 = constants$d2
  )
}


#' Check for out-of-control points
#' @param values Vector of values to check
#' @param center_line Center line value
#' @param ucl Upper control limit
#' @param lcl Lower control limit
#' @return Vector of indices for out-of-control points
check_out_of_control <- function(values, center_line, ucl, lcl) {
  which(values > ucl | values < lcl)
}


#' Create X-bar control chart
#' @param x_bars Vector of subgroup means
#' @param center_line Center line (X-double-bar)
#' @param ucl Upper control limit (single value or vector for variable limits)
#' @param lcl Lower control limit (single value or vector for variable limits)
#' @param title Chart title
#' @param out_of_control Indices of out-of-control points
#' @return ggplot object
plot_xbar_chart <- function(x_bars, center_line, ucl, lcl,
                             title = "X-bar Control Chart",
                             out_of_control = NULL,
                             race_dates = NULL) {
  n <- length(x_bars)

  # Handle both fixed and variable control limits
  if (length(ucl) == 1) {
    ucl <- rep(ucl, n)
    lcl <- rep(lcl, n)
  }

  df <- data.frame(
    subgroup = 1:n,
    value = x_bars,
    ucl = ucl,
    lcl = lcl,
    out_of_control = 1:n %in% out_of_control
  )

  # Build x-axis scale with optional month secondary axis
  if (!is.null(race_dates)) {
    parsed_dates <- as.Date(race_dates)
    month_labels <- format(parsed_dates, "%b")
    # Find first occurrence of each month for axis breaks
    month_breaks <- which(!duplicated(month_labels))
    month_names <- month_labels[month_breaks]

    x_scale <- scale_x_continuous(
      breaks = 1:n,
      sec.axis = dup_axis(
        breaks = month_breaks,
        labels = month_names,
        name = "Month"
      )
    )
  } else {
    x_scale <- scale_x_continuous(breaks = 1:n)
  }

  ggplot(df, aes(x = subgroup, y = value)) +
    # Variable control limits (step lines)
    geom_step(aes(y = ucl), direction = "mid", linetype = "dashed", color = "red", linewidth = 0.8) +
    geom_step(aes(y = lcl), direction = "mid", linetype = "dashed", color = "red", linewidth = 0.8) +
    # Center line
    geom_hline(yintercept = center_line, color = "darkgreen", linewidth = 1) +
    # Data points and line
    geom_line(color = "steelblue") +
    geom_point(aes(color = out_of_control), size = 3) +
    scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red"),
                       guide = "none") +
    x_scale +
    # Labels
    annotate("text", x = n + 0.3, y = mean(ucl), label = "UCL",
             hjust = 0, color = "red", size = 3) +
    annotate("text", x = n + 0.3, y = center_line, label = "CL",
             hjust = 0, color = "darkgreen", size = 3) +
    annotate("text", x = n + 0.3, y = mean(lcl), label = "LCL",
             hjust = 0, color = "red", size = 3) +
    labs(title = title,
         subtitle = if(sd(ucl) > 0.001) "Variable limits based on stint size" else NULL,
         x = "Stint", y = "Mean Lap Time (seconds)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray50")) +
    coord_cartesian(xlim = c(0.5, n + 1.5))
}


#' Create R control chart
#' @param ranges Vector of subgroup ranges
#' @param center_line Center line (R-bar)
#' @param ucl Upper control limit (single value or vector for variable limits)
#' @param lcl Lower control limit (single value or vector for variable limits)
#' @param title Chart title
#' @return ggplot object
plot_r_chart <- function(ranges, center_line, ucl, lcl,
                          title = "R Control Chart",
                          race_dates = NULL) {
  n <- length(ranges)

  # Handle both fixed and variable control limits
  if (length(ucl) == 1) {
    ucl <- rep(ucl, n)
    lcl <- rep(lcl, n)
  }

  df <- data.frame(
    subgroup = 1:n,
    value = ranges,
    ucl = ucl,
    lcl = lcl
  )

  # Check for out of control (comparing each to its own limit)
  df$out_of_control <- df$value > df$ucl | df$value < df$lcl

  # Build x-axis scale with optional month secondary axis
  if (!is.null(race_dates)) {
    parsed_dates <- as.Date(race_dates)
    month_labels <- format(parsed_dates, "%b")
    # Find first occurrence of each month for axis breaks
    month_breaks <- which(!duplicated(month_labels))
    month_names <- month_labels[month_breaks]

    x_scale <- scale_x_continuous(
      breaks = 1:n,
      sec.axis = dup_axis(
        breaks = month_breaks,
        labels = month_names,
        name = "Month"
      )
    )
  } else {
    x_scale <- scale_x_continuous(breaks = 1:n)
  }

  ggplot(df, aes(x = subgroup, y = value)) +
    # Variable control limits (step lines)
    geom_step(aes(y = ucl), direction = "mid", linetype = "dashed", color = "red", linewidth = 0.8) +
    geom_step(aes(y = lcl), direction = "mid", linetype = "dashed", color = "red", linewidth = 0.8) +
    # Center line
    geom_hline(yintercept = center_line, color = "darkgreen", linewidth = 1) +
    # Data points and line
    geom_line(color = "steelblue") +
    geom_point(aes(color = out_of_control), size = 3) +
    scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red"),
                       guide = "none") +
    x_scale +
    # Labels
    annotate("text", x = n + 0.3, y = mean(ucl), label = "UCL",
             hjust = 0, color = "red", size = 3) +
    annotate("text", x = n + 0.3, y = center_line, label = "CL",
             hjust = 0, color = "darkgreen", size = 3) +
    annotate("text", x = n + 0.3, y = mean(lcl), label = "LCL",
             hjust = 0, color = "red", size = 3) +
    labs(title = title,
         subtitle = if(sd(ucl) > 0.001) "Variable limits based on stint size" else NULL,
         x = "Stint", y = "Range (seconds)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray50")) +
    coord_cartesian(xlim = c(0.5, n + 1.5))
}


#' Plot lap times for each stint in a faceted view
#' @param stint_result Result from organize_into_stints()
#' @param title Chart title
#' @return ggplot object
plot_stint_lap_times <- function(stint_result, title = "Lap Times by Stint") {
  # Combine all stints into one data frame with stint labels
  all_laps <- data.frame()
  for (i in 1:length(stint_result$stints)) {
    stint_data <- stint_result$stints[[i]]
    stint_data$stint <- paste0("Stint ", i, " (", nrow(stint_data), " laps)")
    stint_data$lap_in_stint <- 1:nrow(stint_data)
    all_laps <- rbind(all_laps, stint_data)
  }

  # Calculate stint means for reference lines
  stint_means <- aggregate(lap_time_sec ~ stint, data = all_laps, FUN = mean)

  ggplot(all_laps, aes(x = lap_in_stint, y = lap_time_sec)) +
    geom_line(color = "steelblue", alpha = 0.7) +
    geom_point(color = "steelblue", size = 2) +
    geom_hline(data = stint_means, aes(yintercept = lap_time_sec),
               linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
    facet_wrap(~ stint, scales = "free_x", nrow = 1) +
    labs(
      title = title,
      subtitle = "Dashed line = stint mean",
      x = "Lap in Stint",
      y = "Lap Time (seconds)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray50"),
      strip.text = element_text(face = "bold"),
      panel.spacing = unit(1, "lines")
    )
}


# ==============================================================================
# SIMPLIFIED HELPER FUNCTIONS (for student labs)
# ==============================================================================

#' Calculate z-score for a single value
#' @param value The observed value
#' @param mean The population/sample mean
#' @param sd The population/sample standard deviation
#' @return The z-score
calculate_zscore <- function(value, mean, sd) {
  (value - mean) / sd
}


#' Categorize a z-score into performance categories
#' @param zscore The z-score value
#' @return Category string: "Excellent", "Good", "Normal", "Slow", or "Problem"
categorize_zscore <- function(zscore) {
  if (zscore < -1.5) {
    "Excellent"
  } else if (zscore < -0.5) {
    "Good"
  } else if (zscore <= 0.5) {
    "Normal"
  } else if (zscore <= 1.5) {
    "Slow"
  } else {
    "Problem"
  }
}


#' Calculate variable control limits for X-bar and R charts
#' When subgroup sizes vary, control limits must be calculated for each subgroup
#' @param x_double_bar Grand mean (center line for X-bar chart)
#' @param r_bar Average range (center line for R chart)
#' @param subgroup_sizes Vector of subgroup sizes
#' @return List with UCL_xbar, LCL_xbar, UCL_R, LCL_R vectors
calculate_variable_control_limits <- function(x_double_bar, r_bar, subgroup_sizes) {
  n <- length(subgroup_sizes)
  UCL_xbar <- numeric(n)
  LCL_xbar <- numeric(n)
  UCL_R <- numeric(n)
  LCL_R <- numeric(n)

  for (i in 1:n) {
    limits <- calculate_control_limits(x_double_bar, r_bar, subgroup_sizes[i])
    UCL_xbar[i] <- limits$UCL_xbar
    LCL_xbar[i] <- limits$LCL_xbar
    UCL_R[i] <- limits$UCL_R
    LCL_R[i] <- limits$LCL_R
  }

  list(
    UCL_xbar = UCL_xbar,
    LCL_xbar = LCL_xbar,
    UCL_R = UCL_R,
    LCL_R = LCL_R
  )
}


#' Find out-of-control points by comparing values to control limits
#' @param values Vector of values to check (e.g., x_bars or ranges)
#' @param ucl Upper control limit(s) - single value or vector
#' @param lcl Lower control limit(s) - single value or vector
#' @return Vector of indices where values are out of control
find_out_of_control_points <- function(values, ucl, lcl) {
  which(values > ucl | values < lcl)
}


#' Calculate all three moving averages for a data vector
#' Convenience function to calculate SMA, WMA, and EMA in one call
#' @param data Vector of values
#' @param window Window size for SMA and WMA (default 5)
#' @param alpha Smoothing factor for EMA (default 0.3)
#' @return Data frame with sma, wma, and ema columns
calculate_all_moving_averages <- function(data, window = 5, alpha = 0.3) {
  data.frame(
    sma = calculate_moving_average(data, window),
    wma = calculate_weighted_moving_average(data, window),
    ema = calculate_exponential_moving_average(data, alpha)
  )
}


# ==============================================================================
# MOVING AVERAGE FUNCTIONS
# ==============================================================================

#' Calculate simple moving average
#' @param data Vector of values
#' @param window Window size
#' @return Vector of moving average values (NA for initial values)
calculate_moving_average <- function(data, window = 5) {
  n <- length(data)
  ma <- rep(NA, n)

  for (i in window:n) {
    ma[i] <- mean(data[(i - window + 1):i])
  }

  ma
}


#' Calculate weighted moving average
#' @param data Vector of values
#' @param window Window size
#' @return Vector of weighted moving average values
calculate_weighted_moving_average <- function(data, window = 5) {
  n <- length(data)
  wma <- rep(NA, n)

  # Weights: 1, 2, 3, ..., window (more recent = higher weight)
  weights <- 1:window
  weight_sum <- sum(weights)

  for (i in window:n) {
    window_data <- data[(i - window + 1):i]
    wma[i] <- sum(window_data * weights) / weight_sum
  }

  wma
}


#' Calculate exponential moving average
#' @param data Vector of values
#' @param alpha Smoothing factor (0 to 1)
#' @return Vector of EMA values
calculate_exponential_moving_average <- function(data, alpha = 0.3) {
  n <- length(data)
  ema <- rep(NA, n)

  # Initialize with first value
  ema[1] <- data[1]

  # Calculate EMA for remaining values
  for (i in 2:n) {
    ema[i] <- alpha * data[i] + (1 - alpha) * ema[i - 1]
  }

  ema
}


# ==============================================================================
# VISUALIZATION FUNCTIONS
# ==============================================================================

#' Plot lap time progression with moving averages
#' @param lap_data Data frame with lap times
#' @param title Chart title
#' @param show_raw Show raw lap times
#' @param show_ma Show simple moving average
#' @param show_ema Show exponential moving average
#' @return ggplot object
plot_lap_times <- function(lap_data, title = "Lap Time Analysis",
                            show_raw = TRUE, show_ma = TRUE, show_ema = TRUE) {
  p <- ggplot(lap_data, aes(x = lap))

  if (show_raw) {
    p <- p + geom_point(aes(y = lap_time_sec), color = "gray50", alpha = 0.5, size = 2) +
      geom_line(aes(y = lap_time_sec), color = "gray50", alpha = 0.3)
  }

  if (show_ma && "ma_lap_time" %in% names(lap_data)) {
    p <- p + geom_line(aes(y = ma_lap_time), color = "blue", linewidth = 1, na.rm = TRUE)
  }

  if (show_ema && "ema_lap_time" %in% names(lap_data)) {
    p <- p + geom_line(aes(y = ema_lap_time), color = "red", linewidth = 1, na.rm = TRUE)
  }

  p + labs(
    title = title,
    x = "Lap Number",
    y = "Lap Time (seconds)",
    caption = "Gray = Raw data, Blue = SMA, Red = EMA"
  ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}


#' Plot pit stop duration distribution
#' @param pit_data Data frame with pit stop data
#' @param title Chart title
#' @return ggplot object
plot_pit_stop_distribution <- function(pit_data, title = "Pit Stop Duration Distribution") {
  ggplot(pit_data, aes(x = duration_sec)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "white", alpha = 0.7) +
    geom_vline(xintercept = 25, linetype = "dashed", color = "green", linewidth = 1) +
    geom_vline(xintercept = 30, linetype = "dashed", color = "orange", linewidth = 1) +
    annotate("text", x = 25, y = Inf, label = "Target (25s)", vjust = 2, hjust = -0.1, color = "green") +
    annotate("text", x = 30, y = Inf, label = "Slow (30s)", vjust = 2, hjust = -0.1, color = "orange") +
    labs(
      title = title,
      x = "Pit Stop Duration (seconds)",
      y = "Count"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}


# ==============================================================================
# HISTORICAL TRACK ANALYSIS FUNCTIONS
# ==============================================================================

#' Get races for a specific circuit across years
#' @param circuit_id Circuit ID (e.g., 9 for Silverstone)
#' @param start_year First year to include
#' @param end_year Last year to include
#' @return Data frame with race information and year labels
get_circuit_races <- function(circuit_id, start_year = 2014, end_year = 2024) {
  circuit_races <- races %>%
    filter(circuitId == circuit_id,
           year >= start_year,
           year <= end_year) %>%
    arrange(year, round) %>%
    left_join(circuits %>% select(circuitId, circuit_name = name), by = "circuitId")
  
  # Handle years with multiple races at same circuit (e.g., 2020)
  circuit_races <- circuit_races %>%
    group_by(year) %>%
    mutate(
      race_count = n(),
      race_num = row_number(),
      year_label = if_else(race_count > 1, 
                           paste0(year, ".", race_num), 
                           as.character(year))
    ) %>%
    ungroup() %>%
    select(raceId, year, year_label, round, name, date, circuit_name)
  
  circuit_races
}


#' Get historical race fastest laps for a circuit
#' Retrieves each driver's fastest lap per race across multiple years
#' @param circuit_id Circuit ID (e.g., 9 for Silverstone)
#' @param start_year First year to include
#' @param end_year Last year to include
#' @param threshold Filter out laps slower than this percentage above fastest (default 0.20 = 20%)
#' @return List with subgroup data and summary info
get_historical_race_laps <- function(circuit_id, start_year = 2014, end_year = 2024, threshold = 0.20) {
  # Get races at this circuit
  circuit_races <- get_circuit_races(circuit_id, start_year, end_year)
  
  # Get lap times for these races
  race_laps <- lap_times %>%
    filter(raceId %in% circuit_races$raceId) %>%
    left_join(circuit_races %>% select(raceId, year, year_label), by = "raceId") %>%
    left_join(drivers %>% select(driverId, driver = surname, code), by = "driverId") %>%
    mutate(lap_time_sec = milliseconds / 1000)
  
  # Get each driver's fastest lap per race
  driver_fastest <- race_laps %>%
    group_by(raceId, year_label, driverId, driver, code) %>%
    summarize(
      fastest_lap_sec = min(lap_time_sec, na.rm = TRUE),
      .groups = "drop"
    )
  
  # For each race, filter out laps > threshold slower than fastest
  laps_filtered <- driver_fastest %>%
    group_by(raceId, year_label) %>%
    mutate(
      race_fastest = min(fastest_lap_sec, na.rm = TRUE),
      threshold_time = race_fastest * (1 + threshold),
      is_valid = fastest_lap_sec <= threshold_time
    ) %>%
    filter(is_valid) %>%
    ungroup()
  
  # Organize into subgroups (one per race/year)
  subgroups <- laps_filtered %>%
    group_by(year_label) %>%
    group_split()
  
  names(subgroups) <- sapply(subgroups, function(sg) sg$year_label[1])
  
  # Summary info
  info <- laps_filtered %>%
    group_by(year_label) %>%
    summarize(
      year = first(as.numeric(sub("\\..*", "", year_label))),
      n_drivers = n(),
      mean_time = mean(fastest_lap_sec),
      min_time = min(fastest_lap_sec),
      max_time = max(fastest_lap_sec),
      range_time = max(fastest_lap_sec) - min(fastest_lap_sec),
      .groups = "drop"
    ) %>%
    arrange(year, year_label)
  
  list(
    subgroups = subgroups,
    info = info,
    data = laps_filtered,
    circuit_id = circuit_id,
    circuit_name = circuit_races$circuit_name[1],
    threshold = threshold
  )
}


cat("F1 Backend loaded successfully!\n")
cat("Ready for lab exercises.\n")
