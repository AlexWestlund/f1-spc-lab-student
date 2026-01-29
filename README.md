# EIND 142 - Formula 1 Racing Lab

A hands-on lab series using real Formula 1 World Championship data to teach Statistical Process Control (SPC) concepts.

## Overview

Students analyze actual F1 race data from the hybrid era (2014-2024) to learn:
- Pareto analysis for root cause identification
- Control charts (X-bar and R) for process monitoring
- Moving averages for trend analysis
- Z-scores for performance evaluation

## Setup Instructions

### 1. Install R

Download and install R from: https://cran.r-project.org/

### 2. Install Positron (Recommended IDE)

Download Positron from: https://positron.posit.co/

### 3. Clone or Download This Repository

The F1 dataset is already included and pre-filtered to the hybrid era (2014-2024).

### 4. Set Your Working Directory

In Positron, set your working directory to the project root folder:
- **Option 1:** Open this folder in Positron (File > Open Folder) - working directory is set automatically
- **Option 2:** Command Palette (Ctrl+Shift+P): "Interpreter: Set as Working Directory"
- **Option 3:** Run `setwd("/path/to/racing_lab")` in the R console

### 5. Run the Labs

Open and run the lab scripts in order:
1. `scripts/f1_student_lab_1.R` - Data Exploration & Pareto Analysis
2. `scripts/f1_student_lab_2.R` - Lap Time Analysis (Control Charts & Moving Averages)
3. `scripts/f1_student_lab_3.R` - Pit Stop Analysis (Z-Scores & Control Charts)

## Lab Sessions

### Session 1: Data Exploration & Pareto Analysis
- Explore the F1 dataset structure
- Identify root causes of DNFs using Pareto analysis
- Apply the 80/20 rule to prioritize failure modes

### Session 2: Lap Time Analysis
- Build X-bar and R control charts for lap time monitoring
- Apply moving averages (SMA, WMA, EMA) to detect trends
- Analyze driver consistency across race stints

### Session 3: Pit Stop Analysis
- Use Z-scores to evaluate pit stop performance
- Monitor team pit stop consistency across a season
- Compare performance relative to competitors

## Project Structure

```
racing_lab/
├── scripts/
│   ├── f1_backend.R          # Backend functions (do not modify)
│   ├── f1_student_lab_1.R    # Lab 1: Pareto Analysis
│   ├── f1_student_lab_2.R    # Lab 2: Lap Times
│   └── f1_student_lab_3.R    # Lab 3: Pit Stops
├── data/                     # Pre-filtered F1 data (2014-2024)
├── outputs/                  # Generated charts saved here
└── README.md
```

## Requirements

- R (version 4.0 or higher recommended)
- Positron IDE (recommended)
- R packages: `dplyr`, `ggplot2`

## Reference

Groover, M. P. - *Fundamentals of Modern Manufacturing* - Chapter 21: Six Sigma and Other Quality Programs

## Data Source

Kaggle F1 World Championship Dataset (1950-2024)
https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020
