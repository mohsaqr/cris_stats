# crisstats

University Publication Productivity Analysis for CRIS Data

## Overview

`crisstats` provides tools for analyzing university publication productivity from CRIS (Current Research Information System) data exports. The package includes functions for:

- **Author productivity analysis** with multiple fractional counting methods
- **Publication statistics** and visualizations
- **Unit-level analysis** and growth tracking
- **Data cleaning** utilities for author name standardization

Designed for JUFO-ranked publications from Finnish universities, particularly the University of Eastern Finland (UEF).

## Installation

```r
# Install from local source
devtools::install("/path/to/crisstats")

# Or install from GitHub (when available)
# devtools::install_github("mohsaqr/crisstats")
```

## Quick Start

```r
library(crisstats)

# Read CRIS data from Excel files
data <- read_cris_data("Data/")

# Clean author names
cleaned <- clean_author_data(data)

# Generate comprehensive productivity analysis
results <- generate_author_productivity_tables(
  cleaned,
  years = 2020:2024,
  top_n = 50
)

# View the comprehensive analysis table
results$comprehensive_analysis

# Access raw data for further analysis
head(results$raw_data)
```

## Main Functions

### Data Import and Cleaning

| Function | Description |
|----------|-------------|
| `read_cris_data()` | Read and combine CRIS Excel exports |
| `clean_author_data()` | Clean and standardize author names |
| `simplify_author_string()` | Convert names to uppercase ASCII |

### Productivity Analysis

| Function | Description |
|----------|-------------|
| `generate_author_productivity_tables()` | Comprehensive author productivity with 6 fractional counting methods |
| `generate_publication_stats()` | Publication statistics and visualizations |
| `analyze_author_publications()` | Individual author publication history |
| `compute_fractional_counts()` | Calculate fractional author contributions |

### Unit Analysis

| Function | Description |
|----------|-------------|
| `plot_unit_growth()` | Unit productivity growth visualization |
| `abbreviate_units()` | Shorten unit names for plotting |
| `get_unit_abbreviations()` | Get UEF unit name abbreviations |
| `add_unit_column()` | Add abbreviated unit column to data |

## Fractional Counting Methods

The package supports 6 different methods for calculating fractional author contributions:

1. **Standard**: Equal share (1/n for n authors)
2. **Harmonic**: Weights by 1/position, normalized
3. **Proportional**: Linear decrease by position
4. **First-Last**: Extra weight for first and last authors
5. **Geometric**: Exponential decay (0.5^position)
6. **Position Weighted**: Custom weights for specific positions

```r
# Use specific counting methods
results <- generate_author_productivity_tables(
  cleaned,
  counting_methods = c("standard", "harmonic", "first_last")
)

# Or compute fractional counts directly
frac <- compute_fractional_counts(
  cleaned,
  counting_methods = c("standard", "geometric"),
  position_weights = c(1.0, 0.8, 0.6, 0.4, 0.2)
)
```

## Unit Name Abbreviations

The package includes abbreviations for 34 UEF units:

```r
# Get all abbreviations
abbrevs <- get_unit_abbreviations()

# Abbreviate unit names
units <- c("School of Computing", "Business School")
abbreviate_units(units)
# Returns: c("Computing", "Business")

# Add custom abbreviations
custom <- c("My New Unit" = "MNU")
abbrevs <- get_unit_abbreviations(custom = custom)
```

## Example Workflow

```r
library(crisstats)
library(dplyr)

# 1. Read data
data <- read_cris_data("Data/", pattern = "Publications_en_*.xlsx")

# 2. Clean author names
cleaned <- clean_author_data(data)

# 3. Filter to JUFO levels 1-3
cleaned <- cleaned %>%
  filter(jufo_level_of_publication %in% c(1, 2, 3))

# 4. Generate statistics
stats <- generate_publication_stats(cleaned %>% filter(publication_year > 2014))
print(stats$articles_per_year_barplot)

# 5. Analyze top authors
results <- generate_author_productivity_tables(
  cleaned,
  years = 2019:2024,
  top_n = 100,
  counting_methods = c("standard", "harmonic", "first_last")
)

# 6. View results
results$comprehensive_analysis
results$most_productive_overall

# 7. Analyze individual author
author_analysis <- analyze_author_publications(
  results$data_filtered,
  author_name = "SMITH JOHN",
  year_range = 2020:2024
)
print(author_analysis$plot)

# 8. Analyze unit growth
unit_plots <- plot_unit_growth(cleaned, n_years = 5, top_n = 10)
print(unit_plots$counts_plot)
```

## Requirements

- R >= 4.1.0
- dplyr, tidyr, stringr, ggplot2, gt
- janitor, datawizard, splitstackshape
- readxl, scales, rlang, purrr

## License

MIT License
