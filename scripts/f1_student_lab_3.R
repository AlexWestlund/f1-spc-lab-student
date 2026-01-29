# ==============================================================================
# EIND 142 - Lab Session 3: Moving Averages in F1 Racing
# ==============================================================================
#
# Student Name: ____________________
# Date: ____________________
#
# INSTRUCTIONS:
# -------------
# 1. Read through each section carefully
# 2. Complete the TODO items (5) by filling in the missing code (replace ____)
# 3. Run each line/section with Ctrl+Enter (Positron/VS Code) or Cmd+Enter (Mac)
# 4. Answer the questions in a separate document (e.g., Word, text, or markdown file)
#
# LEARNING OBJECTIVES:
# - Calculate and interpret Simple Moving Averages (SMA)
# - Calculate and interpret Weighted Moving Averages (WMA)
# - Calculate and interpret Exponential Moving Averages (EMA)
# - Compare how different moving averages respond to changes
#
# SCENARIO:
# ---------
# You are a race engineer for a Formula 1 team. During a race, you need to:
# 1. Track lap time trends using moving averages (are our tires degrading?)
# 2. Compare driver performance across stints
#
# Moving averages help smooth out lap-to-lap noise and reveal underlying trends,
# which is critical for making pit stop strategy decisions!
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
# PART 1: INTRODUCTION TO MOVING AVERAGES
# ==============================================================================
#
# BACKGROUND:
# -----------
# During an F1 race, lap times fluctuate due to many factors:
# - Traffic (getting stuck behind slower cars)
# - Battles with other drivers
# - Minor mistakes or lock-ups
# - Wind gusts, temperature changes
#
# These lap-to-lap variations make it hard to see the UNDERLYING TREND.
# Is the driver getting faster? Are the tires degrading?
#
# MOVING AVERAGES smooth out this noise by averaging recent values.
# This reveals the true performance trend hidden in the noisy data.
#
# We'll explore three types:
# 1. Simple Moving Average (SMA) - equal weight to all points in window
# 2. Weighted Moving Average (WMA) - more weight to recent points
# 3. Exponential Moving Average (EMA) - exponentially decaying weights
# ==============================================================================

cat("\n======================================================================\n")
cat("PART 1: INTRODUCTION TO MOVING AVERAGES\n")
cat("======================================================================\n")

# Load Hamilton's lap times from the 2024 British GP
RACE_ID <- 1132
DRIVER_ID_HAM <- 1  # Lewis Hamilton

race_info <- get_race_info(race_id = RACE_ID)
hamilton_data <- get_lap_time_data(race_id = RACE_ID, driver_id = DRIVER_ID_HAM)

cat("\nAnalyzing lap time trends for Lewis Hamilton\n")
cat("Race:", race_info$name, "-", race_info$date, "\n")

cat("\nLap time statistics:\n")
cat(sprintf("  Total laps: %d\n", nrow(hamilton_data)))
cat(sprintf("  First lap:  %.3f seconds (usually slow - traffic, cold tires)\n", hamilton_data$lap_time_sec[1]))
cat(sprintf("  Best lap:   %.3f seconds (Lap %d)\n",
            min(hamilton_data$lap_time_sec), which.min(hamilton_data$lap_time_sec)))
cat(sprintf("  Worst lap:  %.3f seconds (Lap %d)\n",
            max(hamilton_data$lap_time_sec), which.max(hamilton_data$lap_time_sec)))
cat(sprintf("  Mean:       %.3f seconds\n", mean(hamilton_data$lap_time_sec)))


# --- SECTION 1.1: Simple Moving Average (SMA) ---

# --------------------------------------------------
# SIMPLE MOVING AVERAGE (SMA):
# --------------------------------------------------
# Formula: SMA(t) = (x[t] + x[t-1] + ... + x[t-n+1]) / n
#
# The SMA gives EQUAL WEIGHT to all points in the window.
# - Window size n=5 means we average the current lap and previous 4 laps
# - Simple to calculate and interpret
# - Drawback: Older data has same influence as recent data

window_size <- 5

# TODO 1: Calculate the simple moving average of lap times
# HINT: Use calculate_moving_average(data, window)
sma_values <- calculate_moving_average(____, window = ____)

hamilton_data$sma <- sma_values

# Show example calculation
cat(sprintf("\nExample: SMA for Lap 10 (window = %d):\n", window_size))
cat(sprintf("  Lap 6:  %.3f\n", hamilton_data$lap_time_sec[6]))
cat(sprintf("  Lap 7:  %.3f\n", hamilton_data$lap_time_sec[7]))
cat(sprintf("  Lap 8:  %.3f\n", hamilton_data$lap_time_sec[8]))
cat(sprintf("  Lap 9:  %.3f\n", hamilton_data$lap_time_sec[9]))
cat(sprintf("  Lap 10: %.3f\n", hamilton_data$lap_time_sec[10]))
cat(sprintf("  ----------------\n"))
manual_sma <- mean(hamilton_data$lap_time_sec[6:10])
cat(sprintf("  SMA = %.3f (sum / 5)\n", manual_sma))
cat(sprintf("  Function result: %.3f\n", hamilton_data$sma[10]))


# --- SECTION 1.2: Weighted Moving Average (WMA) ---

# --------------------------------------------------
# WEIGHTED MOVING AVERAGE (WMA):
# --------------------------------------------------
# Formula: WMA(t) = (n*x[t] + (n-1)*x[t-1] + ... + 1*x[t-n+1]) / (n + n-1 + ... + 1)
#
# The WMA gives MORE WEIGHT to recent observations.
# - For n=5: weights are [1, 2, 3, 4, 5] (most recent gets weight 5)
# - Responds faster to changes than SMA
# - Still uses a fixed lookback window

# TODO 2: Calculate the weighted moving average
# HINT: Use calculate_weighted_moving_average(data, window)
wma_values <- calculate_weighted_moving_average(____, window = ____)

hamilton_data$wma <- wma_values

# Show example calculation
cat(sprintf("\nExample: WMA for Lap 10 (window = %d):\n", window_size))
cat(sprintf("  Lap 6:  %.3f x 1 = %.3f\n", hamilton_data$lap_time_sec[6], hamilton_data$lap_time_sec[6] * 1))
cat(sprintf("  Lap 7:  %.3f x 2 = %.3f\n", hamilton_data$lap_time_sec[7], hamilton_data$lap_time_sec[7] * 2))
cat(sprintf("  Lap 8:  %.3f x 3 = %.3f\n", hamilton_data$lap_time_sec[8], hamilton_data$lap_time_sec[8] * 3))
cat(sprintf("  Lap 9:  %.3f x 4 = %.3f\n", hamilton_data$lap_time_sec[9], hamilton_data$lap_time_sec[9] * 4))
cat(sprintf("  Lap 10: %.3f x 5 = %.3f\n", hamilton_data$lap_time_sec[10], hamilton_data$lap_time_sec[10] * 5))
weighted_sum <- sum(hamilton_data$lap_time_sec[6:10] * 1:5)
weight_total <- sum(1:5)
cat(sprintf("  ----------------\n"))
cat(sprintf("  WMA = %.3f / %d = %.3f\n", weighted_sum, weight_total, weighted_sum / weight_total))


# --- SECTION 1.3: Exponential Moving Average (EMA) ---

# --------------------------------------------------
# EXPONENTIAL MOVING AVERAGE (EMA):
# --------------------------------------------------
# Formula: EMA(t) = alpha * x[t] + (1 - alpha) * EMA(t-1)
#
# The EMA uses EXPONENTIALLY DECAYING weights.
# - Alpha (smoothing factor) controls responsiveness (typically 0.1 to 0.3)
# - Higher alpha = more weight on recent data = faster response
# - Lower alpha = smoother line = slower response
# - No fixed window - ALL past data contributes (with decreasing weight)

alpha <- 0.3

# TODO 3: Calculate the exponential moving average
# HINT: Use calculate_exponential_moving_average(data, alpha)
ema_values <- calculate_exponential_moving_average(____, alpha = ____)

hamilton_data$ema <- ema_values

cat(sprintf("\nUsing alpha = %.1f\n", alpha))
cat("This means each new lap gets 30%% weight, previous EMA gets 70%% weight.\n")

# Show example calculation
cat(sprintf("\nExample: EMA for first few laps:\n"))
cat(sprintf("  Lap 1: EMA = %.3f (first value = raw value)\n", hamilton_data$lap_time_sec[1]))
ema_2 <- alpha * hamilton_data$lap_time_sec[2] + (1 - alpha) * hamilton_data$lap_time_sec[1]
cat(sprintf("  Lap 2: EMA = 0.3 * %.3f + 0.7 * %.3f = %.3f\n",
            hamilton_data$lap_time_sec[2], hamilton_data$lap_time_sec[1], ema_2))

# QUESTION 1.1: If you increase the window size for SMA from 5 to 10, what
# happens to the smoothness of the line? What about responsiveness to changes?
# Your answer: ________________________________________________________________
# _____________________________________________________________________________

# QUESTION 1.2: For EMA, what happens if you use alpha = 0.9 vs alpha = 0.1?
# Which would respond faster to a sudden change in lap times?
# Your answer: ________________________________________________________________
# _____________________________________________________________________________


# ==============================================================================
# PART 2: COMPARING MOVING AVERAGE METHODS
# ==============================================================================
#
# Now let's visualize all three methods together to see how they differ!
# ==============================================================================

cat("\n======================================================================\n")
cat("PART 2: COMPARING MOVING AVERAGE METHODS\n")
cat("======================================================================\n")

cat("\n--------------------------------------------------\n")
cat("MOVING AVERAGE COMPARISON (Laps 5-20):\n")
cat("--------------------------------------------------\n")
ma_comparison <- hamilton_data %>%
  filter(lap >= 5 & lap <= 20) %>%
  select(Lap = lap, Raw = lap_time_sec, SMA = sma, WMA = wma, EMA = ema) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
print(ma_comparison)

# Create comparison plot
fig_ma_compare <- ggplot(hamilton_data, aes(x = lap)) +
  geom_line(aes(y = lap_time_sec, color = "Raw"), alpha = 0.5, linewidth = 0.5) +
  geom_point(aes(y = lap_time_sec, color = "Raw"), alpha = 0.3, size = 1) +
  geom_line(aes(y = sma, color = "SMA (n=5)"), linewidth = 1) +
  geom_line(aes(y = wma, color = "WMA (n=5)"), linewidth = 1) +
  geom_line(aes(y = ema, color = "EMA (a=0.3)"), linewidth = 1) +
  scale_color_manual(
    name = "Method",
    values = c("Raw" = "gray60", "SMA (n=5)" = "blue",
               "WMA (n=5)" = "green", "EMA (a=0.3)" = "red"),
    breaks = c("Raw", "SMA (n=5)", "WMA (n=5)", "EMA (a=0.3)")
  ) +
  labs(
    title = "Comparing Moving Average Methods - Hamilton 2024 British GP",
    subtitle = "SMA gives equal weight | WMA weights recent more | EMA uses exponential decay",
    x = "Lap Number",
    y = "Lap Time (seconds)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(fig_ma_compare)
ggsave(file.path(OUTPUT_DIR, "moving_average_comparison.png"), fig_ma_compare,
       width = 12, height = 6, dpi = 150)
cat("\nSaved: moving_average_comparison.png\n")

# Calculate lag/responsiveness metrics
cat("\n--------------------------------------------------\n")
cat("RESPONSIVENESS ANALYSIS:\n")
cat("--------------------------------------------------\n")

# Find the lap with the biggest single-lap change
lap_changes <- diff(hamilton_data$lap_time_sec)
biggest_change_lap <- which.max(abs(lap_changes)) + 1
change_amount <- lap_changes[biggest_change_lap - 1]

cat(sprintf("Biggest lap time change: Lap %d (%.3f seconds %s)\n",
            biggest_change_lap, abs(change_amount),
            ifelse(change_amount > 0, "slower", "faster")))

cat("\nHow each method responded to this change:\n")
cat(sprintf("  Raw change:  %.3f seconds\n", change_amount))
cat(sprintf("  SMA change:  %.3f seconds\n",
            hamilton_data$sma[biggest_change_lap] - hamilton_data$sma[biggest_change_lap - 1]))
cat(sprintf("  WMA change:  %.3f seconds\n",
            hamilton_data$wma[biggest_change_lap] - hamilton_data$wma[biggest_change_lap - 1]))
cat(sprintf("  EMA change:  %.3f seconds\n",
            hamilton_data$ema[biggest_change_lap] - hamilton_data$ema[biggest_change_lap - 1]))

# KEY INSIGHT:
# - SMA responds slowest (averages out the change over the full window)
# - WMA responds faster (gives more weight to the change)
# - EMA response depends on alpha (smaller alpha = slower response)

# QUESTION 2.1: Looking at the plot, which moving average method tracks the
# raw data most closely? Which produces the smoothest line?
# Your answer: ________________________________________________________________
# _____________________________________________________________________________


# ==============================================================================
# PART 3: DRIVER COMPARISON - STINT ANALYSIS
# ==============================================================================
#
# Now let's compare two drivers from the same race to see how moving averages
# can reveal differences in performance and tire degradation!
#
# Hamilton won this race, with Verstappen finishing P2 (+1.465s).
# Let's see if we can spot the performance difference in the lap time trends.
# ==============================================================================

cat("\n======================================================================\n")
cat("PART 3: DRIVER COMPARISON - Stint Analysis\n")
cat("======================================================================\n")

# Load Verstappen's lap times
DRIVER_ID_VER <- 830  # Max Verstappen
verstappen_data <- get_lap_time_data(race_id = RACE_ID, driver_id = DRIVER_ID_VER)

cat("\nComparing lap time trends:\n")
cat("  Lewis Hamilton (Winner)\n")
cat("  Max Verstappen (P2, +1.465s)\n")

# TODO 4: Calculate all three moving averages for Verstappen in one call
# HINT: Use calculate_all_moving_averages(data, window, alpha)
ver_ma <- calculate_all_moving_averages(____, window = window_size, alpha = alpha)

# Extract the results into verstappen_data
verstappen_data$sma <- ver_ma$sma
verstappen_data$wma <- ver_ma$wma
verstappen_data$ema <- ver_ma$ema

cat("\n--------------------------------------------------\n")
cat("STINT SUMMARY:\n")
cat("--------------------------------------------------\n")

# Calculate average lap times for each driver (excluding first lap)
ham_avg <- mean(hamilton_data$lap_time_sec[-1])
ver_avg <- mean(verstappen_data$lap_time_sec[-1])

cat(sprintf("Hamilton average lap time: %.3f seconds\n", ham_avg))
cat(sprintf("Verstappen average lap time: %.3f seconds\n", ver_avg))
cat(sprintf("Difference: %.3f seconds per lap (%.1f%% %s)\n",
            abs(ham_avg - ver_avg),
            abs(ham_avg - ver_avg) / ham_avg * 100,
            ifelse(ham_avg < ver_avg, "Hamilton faster", "Verstappen faster")))

# Create driver comparison plot using EMA (best balance of smoothing and responsiveness)
comparison_data <- rbind(
  hamilton_data %>% mutate(driver = "Hamilton"),
  verstappen_data %>% mutate(driver = "Verstappen")
)

fig_driver_compare <- ggplot(comparison_data, aes(x = lap, color = driver)) +
  geom_line(aes(y = lap_time_sec), alpha = 0.3, linewidth = 0.5) +
  geom_line(aes(y = ema), linewidth = 1.2) +
  scale_color_manual(values = c("Hamilton" = "#00D2BE", "Verstappen" = "#1E41FF")) +
  labs(
    title = "Driver Comparison: Hamilton vs Verstappen - 2024 British GP",
    subtitle = "Solid lines show EMA (alpha=0.3) | Faded lines show raw lap times",
    x = "Lap Number",
    y = "Lap Time (seconds)",
    color = "Driver"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(fig_driver_compare)
ggsave(file.path(OUTPUT_DIR, "driver_comparison_ema.png"), fig_driver_compare,
       width = 12, height = 6, dpi = 150)
cat("\nSaved: driver_comparison_ema.png\n")


# --- SECTION 3.1: Analyze Tire Degradation ---

cat("\n--------------------------------------------------\n")
cat("TIRE DEGRADATION ANALYSIS:\n")
cat("--------------------------------------------------\n")

# TODO 5: Calculate the average lap-over-lap change for each driver
# This shows the overall trend (positive = getting slower = tire degradation)
# HINT: Use diff() to get lap-to-lap changes, then mean()
hamilton_deltas <- diff(hamilton_data$lap_time_sec)
verstappen_deltas <- diff(verstappen_data$lap_time_sec)

ham_avg_delta <- mean(____)
ver_avg_delta <- mean(____)

cat(sprintf("Hamilton avg lap-to-lap change: %+.3f seconds\n", ham_avg_delta))
cat(sprintf("Verstappen avg lap-to-lap change: %+.3f seconds\n", ver_avg_delta))

cat("\nInterpretation:\n")
if (ham_avg_delta > 0) {
  cat("  Hamilton: Lap times INCREASING (tire degradation dominant)\n")
} else {
  cat("  Hamilton: Lap times DECREASING (fuel effect/track evolution dominant)\n")
}
if (ver_avg_delta > 0) {
  cat("  Verstappen: Lap times INCREASING (tire degradation dominant)\n")
} else {
  cat("  Verstappen: Lap times DECREASING (fuel effect/track evolution dominant)\n")
}

# QUESTION 3.1: Looking at the driver comparison plot, where do you see the
# biggest differences between Hamilton and Verstappen? What might explain this?
# Your answer: ________________________________________________________________
# _____________________________________________________________________________

# QUESTION 3.2: If you were Verstappen's race engineer and saw his EMA trending
# upward faster than Hamilton's, what strategy adjustment might you consider?
# Your answer: ________________________________________________________________
# _____________________________________________________________________________

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n======================================================================\n")
cat("LAB 3 SUMMARY\n")
cat("======================================================================\n")

# KEY TAKEAWAYS:
# ==============
#
# 1. MOVING AVERAGES smooth noise to reveal trends:
#    - SMA: Simple, equal weights, slow response
#    - WMA: Weighted toward recent, moderate response
#    - EMA: Exponential decay, fastest response, tunable via alpha
#
# 2. DRIVER COMPARISON shows how moving averages reveal:
#    - Relative pace differences
#    - Tire degradation patterns
#    - Strategic opportunities


# ==============================================================================
# FINAL QUESTIONS
# ==============================================================================

cat("\n======================================================================\n")
cat("FINAL QUESTIONS\n")
cat("======================================================================\n")

# QUESTION 4.1: A race engineer needs to make pit stop decisions in real-time.
# Which moving average method would you recommend? Why?
# Your answer: ________________________________________________________________
# _____________________________________________________________________________
# _____________________________________________________________________________

# QUESTION 4.2: How might you combine control charts (from Lab 2) with moving
# averages (from this lab) to monitor a race team's performance?
# Your answer: ________________________________________________________________
# _____________________________________________________________________________
# _____________________________________________________________________________


cat("\n======================================================================\n")
cat("LAB SESSION 3 COMPLETE!\n")
cat("======================================================================\n")
cat("Check the 'outputs' folder for your charts.\n")
cat("\nRemember to save your answers and submit your completed lab!\n")
