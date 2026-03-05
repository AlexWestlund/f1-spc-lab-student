# EIND 142 - Formula 1 Statistical Process Control Lab

A hands-on lab series using real Formula 1 World Championship data to teach Statistical Process Control (SPC) concepts.

## Overview

Students analyze actual F1 race data from the hybrid era (2014-2024) to learn:
- **Lab 1:** Pareto analysis for root cause identification (DNF failures)
- **Lab 2:** X-bar and R control charts for pit stop monitoring (z-scores, variable limits)
- **Lab 3:** Moving averages (SMA, WMA, EMA) for lap time trend analysis

## Getting Started

### 1. Install R

Download and install R (version 4.0 or higher) from: https://cran.r-project.org/

### 2. Install Positron IDE (Recommended)

Download Positron from: https://positron.posit.co/

> Positron is a modern data-science IDE built by the creators of RStudio. You may also use VS Code with the R extension.

### 3. Clone This Repository

```bash
git clone https://github.com/AlexWestlund/f1-spc-lab-student.git
```

Or download the ZIP file from GitHub and extract it.

### 4. Open the Project

Open the `f1-spc-lab-student` folder in Positron (File > Open Folder). This automatically sets your working directory to the correct location.

> **Important:** All lab scripts must be run from the project root directory (the folder containing `scripts/`, `data/`, etc.). If you're not using Positron's "Open Folder" feature, set your working directory manually:
> ```r
> setwd("/path/to/f1-spc-lab-student")
> ```

### 5. Run the Labs in Order

Open each lab script and run it line-by-line (Ctrl+Enter or Cmd+Enter):
Each script is associated with a separate lab session.

1. `scripts/f1_student_lab_1.R` -- Data Exploration & Pareto Analysis *(3 TODOs)*
2. `scripts/f1_student_lab_2.R` -- X-bar and R Charts with Pit Stop Analysis *(8 TODOs)*
3. `scripts/f1_student_lab_3.R` -- Moving Averages in F1 Racing *(5 TODOs)*

### 6. Complete Your Answers

- Fill in the `____` blanks in each lab script with the correct R code
- Answer the discussion questions in a **separate document** (Word (preferred), text, or markdown file)
- Submit both your completed `.R` scripts and your answer document

## Lab Sessions

### Session 1: Data Exploration & Pareto Analysis (3 TODOs)
- Explore the F1 dataset structure (hybrid era 2014-2024)
- Identify root causes of DNFs (Did Not Finish) using Pareto analysis
- Apply the 80/20 rule to prioritize failure modes
- Create and interpret a Pareto chart

### Session 2: X-bar and R Control Charts (8 TODOs)
- Analyze Red Bull Racing's pit stop performance across the 2024 season
- Use z-scores to normalize pit stop times across different tracks
- Build X-bar and R control charts with **variable control limits**
- Identify out-of-control races and interpret what they mean

### Session 3: Moving Averages (5 TODOs)
- Calculate Simple Moving Averages (SMA), Weighted Moving Averages (WMA), and Exponential Moving Averages (EMA)
- Compare how different moving average methods respond to changes
- Analyze Hamilton vs Verstappen lap time trends at the 2024 British GP
- Assess tire degradation using lap-over-lap changes

## Project Structure

```
f1-spc-lab-student/
├── scripts/
│   ├── f1_backend.R          # Backend functions (DO NOT MODIFY)
│   ├── prepare_data.R        # Data prep script (reference only)
│   ├── f1_student_lab_1.R    # Lab 1: Pareto Analysis
│   ├── f1_student_lab_2.R    # Lab 2: Control Charts (Pit Stops)
│   └── f1_student_lab_3.R    # Lab 3: Moving Averages (Lap Times)
├── data/                     # Pre-filtered F1 data (2014-2024)
│   ├── circuits.csv
│   ├── constructors.csv
│   ├── drivers.csv
│   ├── lap_times.csv
│   ├── pit_stops.csv
│   ├── races.csv
│   ├── results.csv
│   └── status.csv
├── outputs/                  # Generated charts saved here (auto-created)
├── .gitignore
└── README.md
```

## Requirements

- **R** version 4.0 or higher
- **IDE:** Positron (recommended) or VS Code with R extension
- **R packages:** `dplyr`, `ggplot2`, `tidyr` (auto-installed by the backend script)

## Reference

Groover, M. P. -- *Fundamentals of Modern Manufacturing* -- Chapter 21: Statistical Process Control

## Data Source

[Kaggle F1 World Championship Dataset (1950-2024)](https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020)

The dataset has been pre-filtered to the hybrid era (2014-2024) and is included in the `data/` folder. No additional downloads are required.
