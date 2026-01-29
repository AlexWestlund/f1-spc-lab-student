# ==============================================================================
# EIND 142 - Lab Session 2: X-bar and R Charts with Pit Stop Analysis
# ==============================================================================
#
# Student Name: ____________________
# Date: ____________________
#
# INSTRUCTIONS:
# -------------
# 1. Read through each section carefully
# 2. Complete the TODO items by filling in the missing code (replace ____)
# 3. Run each line/section with Ctrl+Enter (Positron/VS Code) or Cmd+Enter (Mac)
# 4. Answer the questions in the spaces provided
#
# LEARNING OBJECTIVES:
# - Build X-bar and R control charts for process monitoring
# - Understand how control limits are calculated from process data
# - Apply variable control limits when subgroup sizes differ
# - Interpret out-of-control signals in a real-world context
#
# SCENARIO:
# ---------
# You are a data analyst for a Formula 1 racing team. Your job is to monitor
# pit stop performance across an entire season using statistical process control.
# Pit stops are critical - a 2-second delay can cost multiple positions!
#
# The challenge: Different tracks have different pit lane layouts, so raw pit
# stop times can't be compared directly. A 22-second stop at Monaco (tight pit
# lane) might be excellent, while the same time at Silverstone (wide pit lane)
# would be slow.
#
# Solution: We use Z-SCORES to normalize pit stop times within each race:
#   z = (pit_stop_time - race_mean) / race_sd
#
# This tells us: "How did this team perform relative to competitors at THIS race?"
# Negative z-scores = faster than average, Positive = slower than average
#
# DATA SOURCE: Kaggle F1 World Championship Dataset (1950-2024)
#
# REFERENCE: Groover Chapter 21 - Statistical Process Control
# ==============================================================================


# First, load the backend library (DO NOT MODIFY - run from repo root directory)
source("scripts/f1_backend.R")

# Set up output directory for saving plots
OUTPUT_DIR <- "outputs"


# ==============================================================================
# PART 1: DATA LOADING & EXPLORATION
# ==============================================================================
#
# We'll analyze Red Bull Racing's pit stops for the 2024 season.
# Red Bull is known for excellent pit stops - let's see if they're "in control"!
#
# For this analysis:
# - Each RACE = one subgroup (all pit stops by the team in that race)
# - We calculate the MEAN z-score (X-bar) and RANGE of z-scores (R) per race
# - This shows if the team's pit crew performs consistently throughout the season
# ==============================================================================

cat("\n======================================================================\n")
cat("PART 1: DATA LOADING & EXPLORATION\n")
cat("======================================================================\n")

# Red Bull Racing constructor ID
CONSTRUCTOR_ID <- 9

# TODO 1: Get Red Bull's pit stops with z-scores for each race
# HINT: Use get_team_season_pit_zscores(constructor_id, year)
pit_zscore_data <- get_team_season_pit_zscores(constructor_id = ____, year = 2024)

cat(sprintf("\nTeam: %s\n", pit_zscore_data$team_name))
cat(sprintf("Season: %d\n", pit_zscore_data$year))
cat(sprintf("Races with valid data: %d\n", pit_zscore_data$n_races))

# --------------------------------------------------
# UNDERSTANDING THE DATA:
# --------------------------------------------------
# Each pit stop has been converted to a z-score:
#   z = (pit_stop_time - race_mean) / race_sd
#
# Where race_mean and race_sd are calculated from ALL teams' pit stops at that race.
#
# Interpretation:
#   z = -1.0  means 1 standard deviation FASTER than the race average
#   z =  0.0  means exactly at the race average
#   z = +1.0  means 1 standard deviation SLOWER than the race average
#
# For a top team like Red Bull, we expect NEGATIVE z-scores (faster than average).

cat("\n--------------------------------------------------\n")
cat("SAMPLE DATA (first 3 races):\n")
cat("--------------------------------------------------\n")
sample_data <- pit_zscore_data$all_data %>%
  filter(round <= 3) %>%
  select(race_name, driver, stop, duration_sec, race_mean, z_score) %>%
  mutate(duration_sec = round(duration_sec, 2),
         race_mean = round(race_mean, 2),
         z_score = round(z_score, 3))
print(sample_data)

# QUESTION 1.1: Looking at the sample data, are Red Bull's pit stops generally
# faster or slower than the race average? How can you tell from the z-scores?
# Your answer: ________________________________________________________________
# _____________________________________________________________________________


# ==============================================================================
# PART 2: SUBGROUP STATISTICS
# ==============================================================================
#
# For X-bar and R charts, we need to calculate statistics for each subgroup.
# Here, each RACE is a subgroup containing 2-4 pit stops (both drivers,
# sometimes multiple stops per driver).
#
# X-bar = mean z-score for the race (measures central tendency)
# R = range of z-scores for the race (measures within-race variation)
# ==============================================================================

cat("\n======================================================================\n")
cat("PART 2: SUBGROUP STATISTICS\n")
cat("======================================================================\n")

# The data structure pit_zscore_data$info contains per-race statistics
# Extract X-bar (mean z-score) for each race
x_bars <- pit_zscore_data$info$mean_zscore

# Extract Range of z-scores for each race
ranges <- pit_zscore_data$info$range_zscore

# TODO 2: Extract subgroup sizes (number of pit stops per race)
# HINT: Look at pit_zscore_data$info - which column has the count?
subgroup_sizes <- pit_zscore_data$info$____

cat("\n--------------------------------------------------\n")
cat("SUBGROUP STATISTICS (by race):\n")
cat("--------------------------------------------------\n")
subgroup_stats <- data.frame(
  Round = pit_zscore_data$info$round,
  Race = substr(pit_zscore_data$info$race_name, 1, 20),
  n = subgroup_sizes,
  Xbar_z = round(x_bars, 3),
  Range_z = round(ranges, 3)
)
print(subgroup_stats)

# QUESTION 2: What does it mean when the Range (R) is large for a race?
# Your answer: ________________________________________________________________
# _____________________________________________________________________________

# ==============================================================================
# PART 3: CALCULATING CENTER LINES
# ==============================================================================
#
# The center lines for X-bar and R charts are:
# - X-double-bar: The grand mean (average of all subgroup means)
# - R-bar: The average range (average of all subgroup ranges)
#
# These represent the "expected" values when the process is in control.
# ==============================================================================

cat("\n======================================================================\n")
cat("PART 3: CALCULATING CENTER LINES\n")
cat("======================================================================\n")

# TODO 3: Calculate grand mean (X-double-bar)
# HINT: Use calculate_grand_mean(x_bars)
x_double_bar <- ____

# TODO 4: Calculate average range (R-bar)
# HINT: Use calculate_average_range(ranges)
r_bar <- ____

cat(sprintf("Grand Mean (X-double-bar) = %.3f \n", x_double_bar))
cat(sprintf("Average Range (R-bar) = %.3f \n", r_bar))

if (x_double_bar < 0) {
  cat(sprintf("\nInterpretation: %s averages %.2f standard deviations FASTER than competitors!\n",
              pit_zscore_data$team_name, abs(x_double_bar)))
} else {
  cat(sprintf("\nInterpretation: %s averages %.2f standard deviations SLOWER than competitors.\n",
              pit_zscore_data$team_name, abs(x_double_bar)))
}

# ==============================================================================
# PART 4: CALCULATING CONTROL LIMITS
# ==============================================================================
#
# Control limits define the boundaries of expected variation:
# - UCL (Upper Control Limit): Upper boundary
# - LCL (Lower Control Limit): Lower boundary
#
# For X-bar charts:
#   UCL = X-double-bar + A2 * R-bar
#   LCL = X-double-bar - A2 * R-bar
#
# For R charts:
#   UCL = D4 * R-bar
#   LCL = D3 * R-bar (often 0 for small n)
#
# IMPORTANT: A2, D3, and D4 depend on subgroup size (n). We use lookup tables.
# Since our subgroup sizes vary (2-4 pit stops per race), we calculate
# VARIABLE control limits - different limits for each subgroup!
# ==============================================================================

cat("\n======================================================================\n")
cat("PART 4: CALCULATING CONTROL LIMITS\n")
cat("======================================================================\n")

# Since subgroup sizes (pitstops per race) vary (n = 2 to 4), we use VARIABLE control limits.
# Each race gets its own UCL and LCL based on its subgroup size.

# TODO 5: Calculate variable control limits for all subgroups
# HINT: Use calculate_variable_control_limits(x_double_bar, r_bar, subgroup_sizes)
control_limits <- calculate_variable_control_limits(____, ____, ____)

# Extract the limit vectors from the result
UCL_xbar <- control_limits$UCL_xbar
LCL_xbar <- control_limits$LCL_xbar
UCL_R <- control_limits$UCL_R
LCL_R <- control_limits$LCL_R

cat("\n--------------------------------------------------\n")
cat("VARIABLE CONTROL LIMITS:\n")
cat("--------------------------------------------------\n")
limits_df <- data.frame(
  Round = pit_zscore_data$info$round,
  n = subgroup_sizes,
  UCL_xbar = round(UCL_xbar, 3),
  LCL_xbar = round(LCL_xbar, 3),
  UCL_R = round(UCL_R, 3)
)
print(limits_df)

# QUESTION 4.1: Why do the control limits need to change when n changes?
# (Hint: Think about what happens to variability with larger samples)
# Your answer: ________________________________________________________________
# _____________________________________________________________________________


# ==============================================================================
# PART 5: IDENTIFYING OUT-OF-CONTROL POINTS
# ==============================================================================
#
# A point is "out of control" if it falls outside the control limits.
# This signals that something UNUSUAL happened - either good or bad!
#
# For pit stops:
# - Above UCL on X-bar chart = unusually SLOW pitstops (problem!)
# - Below LCL on X-bar chart = unusually FAST pitstops (excellent!)
# - Above UCL on R chart = inconsistent pitstop performance within the race
# ==============================================================================

cat("\n======================================================================\n")
cat("PART 5: IDENTIFYING OUT-OF-CONTROL POINTS\n")
cat("======================================================================\n")

# TODO 6: Identify out-of-control points on the X-bar chart
# A point is out of control if it falls outside the control limits
# HINT: Use find_out_of_control_points(values, ucl, lcl)
out_of_control_xbar <- find_out_of_control_points(____, ____, ____)

cat("\n--------------------------------------------------\n")
cat("OUT-OF-CONTROL ANALYSIS (X-bar Chart):\n")
cat("--------------------------------------------------\n")
if (length(out_of_control_xbar) == 0) {
  cat("All races are within control limits - Pit stop process is IN CONTROL!\n")
} else {
  cat("OUT OF CONTROL races:\n")
  for (idx in out_of_control_xbar) {
    cat(sprintf("  Round %d (%s): X-bar = %.3f (",
                pit_zscore_data$info$round[idx],
                pit_zscore_data$info$race_name[idx],
                x_bars[idx]))
    if (x_bars[idx] > UCL_xbar[idx]) {
      cat(sprintf("ABOVE UCL of %.3f - unusually slow)\n", UCL_xbar[idx]))
    } else {
      cat(sprintf("BELOW LCL of %.3f - unusually fast)\n", LCL_xbar[idx]))
    }
  }
}

# Check R chart for out-of-control points
out_of_control_R <- which(ranges > UCL_R)

cat("\n--------------------------------------------------\n")
cat("OUT-OF-CONTROL ANALYSIS (R Chart):\n")
cat("--------------------------------------------------\n")
if (length(out_of_control_R) == 0) {
  cat("All races show consistent within-race variation - R chart is IN CONTROL!\n")
} else {
  cat("Races with excessive variation:\n")
  for (idx in out_of_control_R) {
    cat(sprintf("  Round %d (%s): R = %.3f (ABOVE UCL of %.3f)\n",
                pit_zscore_data$info$round[idx],
                pit_zscore_data$info$race_name[idx],
                ranges[idx], UCL_R[idx]))
  }
}

# QUESTION 5.1: If a race is "out of control" on the X-bar chart,
# how can you tell? List some reasons a pitstops might be out of control.
# Your answer: ________________________________________________________________
# _____________________________________________________________________________


# ==============================================================================
# PART 6: CREATING THE CONTROL CHARTS
# ==============================================================================
#
# Now let's visualize everything! We'll create:
# 1. X-bar chart - monitors the average pit stop performance per race
# 2. R chart - monitors the consistency within each race
# ==============================================================================

cat("\n======================================================================\n")
cat("PART 6: CREATING THE CONTROL CHARTS\n")
cat("======================================================================\n")

# TODO 7: Create X-bar chart for pit stop z-scores
# Fill in the parameters for plot_xbar_chart()
fig_xbar <- plot_xbar_chart(
  ____,              # x_bars (the subgroup means)
  ____,              # center line (x_double_bar)
  ____,              # UCL (upper control limits)
  ____,              # LCL (lower control limits)
  sprintf("X-bar Chart: %s Pit Stop Performance (2024 Season)", pit_zscore_data$team_name),
  out_of_control_xbar,
  race_dates = pit_zscore_data$info$race_date
)

# Add custom labels
fig_xbar <- fig_xbar +
  labs(x = "Race (Round)", y = "Mean Z-Score",
       subtitle = "Variable limits based on pit stops per race | Negative = faster than average")

print(fig_xbar)
ggsave(file.path(OUTPUT_DIR, "xbar_pit_stops.png"), fig_xbar,
       width = 12, height = 6, dpi = 150)
cat("\nSaved: xbar_pit_stops.png\n")

# TODO 8: Create R chart for pit stop z-score variation
# Fill in the parameters for plot_r_chart()
fig_r <- plot_r_chart(
  ____,              # ranges (the subgroup ranges)
  ____,              # center line (r_bar)
  ____,              # UCL (upper control limits)
  LCL_R,             # LCL (lower control limits - already set to 0)
  sprintf("R Chart: %s Pit Stop Consistency (2024 Season)", pit_zscore_data$team_name),
  race_dates = pit_zscore_data$info$race_date
)

# Add custom labels
fig_r <- fig_r +
  labs(x = "Race (Round)", y = "Range of Z-Scores",
       subtitle = "Variable limits based on pit stops per race | Large R = inconsistent performance")

print(fig_r)
ggsave(file.path(OUTPUT_DIR, "r_chart_pit_stops.png"), fig_r,
       width = 12, height = 6, dpi = 150)
cat("Saved: r_chart_pit_stops.png\n")


# ==============================================================================
# PART 7: INTERPRETATION & CONCLUSIONS
# ==============================================================================

cat("\n======================================================================\n")
cat("PART 7: INTERPRETATION & CONCLUSIONS\n")
cat("======================================================================\n")

cat("\n--------------------------------------------------\n")
cat("SUMMARY STATISTICS:\n")
cat("--------------------------------------------------\n")
cat(sprintf("Team analyzed: %s\n", pit_zscore_data$team_name))
cat(sprintf("Total races: %d\n", length(x_bars)))
cat(sprintf("Total pit stops: %d\n", sum(subgroup_sizes)))
cat(sprintf("\nGrand Mean (X-double-bar): %.3f\n", x_double_bar))
cat(sprintf("Average Range (R-bar): %.3f\n", r_bar))
cat(sprintf("\nOut-of-control (X-bar): %d races\n", length(out_of_control_xbar)))
cat(sprintf("Out-of-control (R): %d races\n", length(out_of_control_R)))

# KEY CONCEPTS DEMONSTRATED:
# --------------------------
# 1. RATIONAL SUBGROUPS: Each race is a natural subgroup - pit stops within
#    a race share common conditions (same track, weather, competition).
#
# 2. NORMALIZATION: Using z-scores allows fair comparison across different
#    tracks with different pit lane characteristics.
#
# 3. VARIABLE CONTROL LIMITS: When subgroup sizes vary, control limits must
#    be calculated separately for each subgroup.
#
# 4. TWO-CHART SYSTEM: X-bar monitors the process average (are we fast?),
#    R monitors consistency (are we reliable?).


# ==============================================================================
# FINAL QUESTIONS
# ==============================================================================

cat("\n======================================================================\n")
cat("FINAL QUESTIONS\n")
cat("======================================================================\n")

# QUESTION 7.1: Based on your analysis, is Red Bull's pit stop process
# "in control"? What evidence supports your conclusion?
# Your answer: ________________________________________________________________
# _____________________________________________________________________________
# _____________________________________________________________________________

# QUESTION 7.2: The X-bar chart shows the AVERAGE performance per race.
# The R chart shows CONSISTENCY within each race. Why do we need both?
# Can a team have good average performance but poor consistency?
# Your answer: ________________________________________________________________
# _____________________________________________________________________________
# _____________________________________________________________________________

# QUESTION 7.3: If you were the pit crew manager and saw an out-of-control
# signal, what would you do? How would you investigate?
# Your answer: ________________________________________________________________
# _____________________________________________________________________________
# _____________________________________________________________________________

# QUESTION 7.4: Why is it important to use z-scores instead of raw pit stop
# times when comparing performance across different races?
# Your answer: ________________________________________________________________
# _____________________________________________________________________________
# _____________________________________________________________________________


cat("\n======================================================================\n")
cat("LAB SESSION 2 COMPLETE!\n")
cat("======================================================================\n")
cat("Check the 'outputs' folder for your control charts.\n")
cat("\nRemember to save your answers and submit your completed lab!\n")
