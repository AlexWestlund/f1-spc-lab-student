# ==============================================================================
# EIND 142 - Figure 7: Bridging the Three Labs
# ==============================================================================
#
# Student Name: ____________________
# Date: ____________________
#
# PURPOSE:
# --------
# This script creates Figure 7 for your technical memo — a two-panel figure
# that connects all three labs by carrying your Pareto "vital few" categories
# (Lab 1) into a Mercedes vs Red Bull team comparison using grouped bars
# (connecting to Lab 2's team-level analysis) and EMA trend lines
# (connecting to Lab 3's moving average methods).
#
# INSTRUCTIONS:
# -------------
# 1. Complete the ONE TODO below using your Lab 1 Pareto output
# 2. Run the entire script (Ctrl+Shift+Enter or Source button)
# 3. Check outputs/figure7_bridge.png
#
# ==============================================================================

library(dplyr)
library(ggplot2)

# Load the backend library
source("scripts/f1_backend.R")

OUTPUT_DIR <- "outputs"

cat("\n======================================================================\n")
cat("FIGURE 7: BRIDGING THE THREE LABS\n")
cat("======================================================================\n")


# ==============================================================================
# TODO: From your Lab 1 Pareto output, list the "vital few" DNF categories
# that accounted for ~80% of fleet-wide DNFs. Copy the exact category names.
# You can replace the placeholder "____" value below with your categories.
# ==============================================================================


vital_few_categories <- c("___","___", "___")  # <-- REPLACE with your categories, e.g. c("Electrical", "Mechanical", "Accident")


==============================================================================
# INPUT VALIDATION — catches typos before they produce empty plots
# ==============================================================================

cat("\nValidating your vital-few categories...\n")

valid_statuses <- unique(status$status)
bad <- vital_few_categories[!vital_few_categories %in% valid_statuses]

if (length(bad) > 0) {
  stop(
    "\n\n",
    "===== ERROR: Unrecognized DNF category =====\n",
    "The following categories you entered do NOT match any value in the status table:\n\n",
    paste("  ->", bad, collapse = "\n"), "\n\n",
    "Double-check your Lab 1 Pareto output and copy the exact category names.\n",
    "Valid status values include:\n",
    paste("  ", head(sort(valid_statuses), 20), collapse = "\n"), "\n",
    "  ... (", length(valid_statuses), " total)\n",
    "============================================\n"
  )
}

cat("  All categories valid!\n")


# ==============================================================================
# PANEL (a): GROUPED HORIZONTAL BAR CHART — DNF Counts by Vital-Few Category
# ==============================================================================

cat("\nBuilding Panel (a): DNF comparison for Mercedes vs Red Bull...\n")

# Define team colors and constructor IDs
team_colors <- c("Mercedes" = "#00A19B", "Red Bull" = "#1E3A8A")
MERC_ID <- 131
RB_ID   <- 9

# Filter to hybrid-era DNFs for Mercedes and Red Bull, vital-few categories only
finished_statuses <- c(1, 11:19, 45, 50, 53, 55, 58, 88, 111:128)

hybrid_race_ids <- races %>%
  filter(year >= 2014, year <= 2024) %>%
  pull(raceId)

dnf_team <- results %>%
  filter(
    raceId %in% hybrid_race_ids,
    constructorId %in% c(MERC_ID, RB_ID),
    !statusId %in% finished_statuses
  ) %>%
  left_join(status, by = "statusId") %>%
  left_join(constructors %>% select(constructorId, team = name), by = "constructorId") %>%
  filter(status %in% vital_few_categories)

# Count DNFs per team per category
dnf_counts <- dnf_team %>%
  count(status, team) %>%
  rename(category = status, count = n)

cat(sprintf("  Mercedes DNFs in vital-few categories: %d\n",
            sum(dnf_counts$count[dnf_counts$team == "Mercedes"])))
cat(sprintf("  Red Bull DNFs in vital-few categories: %d\n",
            sum(dnf_counts$count[dnf_counts$team == "Red Bull"])))

# Order categories by total count (descending) for a clean chart
category_order <- dnf_counts %>%
  group_by(category) %>%
  summarise(total = sum(count)) %>%
  arrange(total) %>%
  pull(category)

dnf_counts$category <- factor(dnf_counts$category, levels = category_order)

# Build grouped horizontal bar chart
panel_a <- ggplot(dnf_counts, aes(x = count, y = category, fill = team)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = count),
    position = position_dodge(width = 0.7),
    hjust = -0.3, size = 3.5
  ) +
  scale_fill_manual(values = team_colors, name = "Team") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "DNF Counts by Vital-Few Category: Mercedes vs Red Bull (2014\u20132024)",
    x = "Number of DNFs",
    y = "DNF Category (from Lab 1 Pareto)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )


# ==============================================================================
# PANEL (b): EMA LINE CHART — Points per Race, 2014–2024
# ==============================================================================

cat("Building Panel (b): Points-per-race EMA trend...\n")

# Calculate points per race for each team-season
team_season <- results %>%
  filter(
    raceId %in% hybrid_race_ids,
    constructorId %in% c(MERC_ID, RB_ID)
  ) %>%
  left_join(races %>% select(raceId, year), by = "raceId") %>%
  left_join(constructors %>% select(constructorId, team = name), by = "constructorId") %>%
  group_by(team, year) %>%
  summarise(
    total_points   = sum(points),
    races_in_year  = n_distinct(raceId),
    points_per_race = total_points / races_in_year,
    .groups = "drop"
  ) %>%
  arrange(team, year)

cat("  Points-per-race summary:\n")
print(team_season %>% select(team, year, points_per_race) %>%
        mutate(points_per_race = round(points_per_race, 1)))

# Apply EMA (alpha = 0.4) to each team's points-per-race series
# Feel free to experiment with other alpha values (0.2–0.6) to see how
# the smoothing changes — how does a lower alpha affect the crossover point?
ema_alpha <- 0.4

team_season <- team_season %>%
  group_by(team) %>%
  mutate(ema = calculate_exponential_moving_average(points_per_race, alpha = ema_alpha)) %>%
  ungroup()

# Build EMA line chart with raw dots
panel_b <- ggplot(team_season, aes(x = year, color = team)) +
  geom_point(aes(y = points_per_race), alpha = 0.35, size = 2.5) +
  geom_line(aes(y = ema), linewidth = 1.2) +
  scale_color_manual(values = team_colors, name = "Team") +
  scale_x_continuous(breaks = 2014:2024) +
  labs(
    title = "Points per Race: Mercedes vs Red Bull (2014\u20132024, EMA \u03b1 = 0.4)",
    x = "Season",
    y = "Points per Race"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "bottom",
    panel.grid.minor.x = element_blank()
  )


# ==============================================================================
# COMBINE PANELS & SAVE
# ==============================================================================

cat("Combining panels and saving figure...\n")

# Use patchwork to stack panels (auto-install if needed, matching backend pattern)
if (!require("patchwork")) install.packages("patchwork", repos = "https://cloud.r-project.org")
library(patchwork)

fig7 <- panel_a / panel_b +
  plot_annotation(
    caption = "Figure 7: Bridging Labs 1\u20133 \u2014 Vital-few DNF categories (Pareto) applied to a team comparison (Control Charts context) with EMA smoothing (Moving Averages)"
  ) &
  theme(plot.caption = element_text(hjust = 0.5, size = 9, face = "italic"))

ggsave(
  file.path(OUTPUT_DIR, "figure7_final_memo.png"),
  fig7,
  width = 10, height = 10, dpi = 150
)

cat("\n======================================================================\n")
cat("FIGURE 7 COMPLETE!\n")
cat("======================================================================\n")
cat("Saved: outputs/figure7_final_memo.png\n\n")
cat("Use this figure in your memo's Integrated Discussion section to show\n")
cat("how Pareto analysis, control charts, and moving averages connect.\n")
cat("======================================================================\n")
