# crisstats: Package Architecture & Reference

> Comprehensive R package for analyzing university publication productivity from CRIS data exports.

**Version:** 0.2.0 | **Author:** Mohammed Saqr | **License:** MIT | **R:** >= 4.1.0

---

## Table of Contents

1. [Overview](#overview)
2. [Data Pipeline](#data-pipeline)
3. [Data Structures](#data-structures)
4. [S3 Classes & Methods](#s3-classes--methods)
5. [Function Reference](#function-reference)
6. [Fractional Counting Methods](#fractional-counting-methods)
7. [Organizational Hierarchy](#organizational-hierarchy)
8. [Visualization System](#visualization-system)
9. [File Structure](#file-structure)
10. [Dependencies](#dependencies)
11. [Workflow Examples](#workflow-examples)
12. [Design Patterns](#design-patterns)

---

## Overview

**crisstats** provides an end-to-end pipeline for analyzing university publication output from CRIS (Current Research Information System) Excel exports. It handles:

- Reading and cleaning raw CRIS data
- Standardizing author names (ASCII, uppercase, deduplication)
- Mapping organizational units to departments and faculties
- Computing author productivity with 6 fractional counting methods
- Generating publication statistics, rankings, and comparisons
- Producing formatted gt tables and ggplot2 visualizations
- Multi-year trend analysis with heatmaps and line plots

---

## Data Pipeline

```
Raw Excel Files (*.xlsx)
       |
       v
[read_cris_data()]
  - Reads all .xlsx files from directory
  - Cleans column names (janitor::clean_names)
  - Removes empty columns
  - Deduplicates by publication_id
       |
       v
[prepare_cris_data() / process_data()]
  - Filters by publication_year (optional)
  - Filters by jufo_level_of_publication (default: 1-3)
  - clean_author_data() -> splits authors, standardizes names
  - add_hierarchy_columns() -> maps units to departments/faculties
       |
       v
Analysis-Ready Data Frame
  (one row per author per publication)
       |
       v
[Analysis Functions]
  author_stats(), top_authors(), compare_years(), etc.
       |
       v
S3 Objects with $table, $gt_table, $plot, $descriptives
```

---

## Data Structures

### Raw CRIS Excel Columns

| Column | Type | Description |
|--------|------|-------------|
| `publication_id` | character | Unique publication identifier |
| `authors` | character | Semicolon-separated author list |
| `first_author` | character | Name of first author |
| `first_authors_unit` | character | Unit affiliation of first author |
| `publication_year` | integer | Year of publication |
| `jufo_level_of_publication` | character | JUFO ranking: 0, 1, 2, or 3 |
| `title_of_publication` | character | Publication title |
| `title_of_journal_series` | character | Journal/venue name |
| `type_of_publication` | character | Publication type (optional) |

### After `process_data()` — Added Columns

| Column | Source | Description |
|--------|--------|-------------|
| `Authorsclean` | `clean_author_data()` | Uppercase ASCII-standardized author name |
| `unit_en` | `add_hierarchy_columns()` | Standardized English unit name |
| `unit_abbrev` | `add_hierarchy_columns()` | Abbreviated unit name (for plots) |
| `department` | `add_hierarchy_columns()` | Parent department |
| `faculty` | `add_hierarchy_columns()` | Parent faculty |

**Note:** After author cleaning, the data is at **author-level** — one row per author per publication. A publication with 3 authors produces 3 rows.

### Author Name Standardization Pipeline

```
"Müller, Hans Jr."
  -> remove " Jr."
  -> uppercase: "MÜLLER, HANS"
  -> iconv transliteration: "MULLER, HANS"
  -> ASCII filter [A-Z0-9 ,.()-]: "MULLER, HANS"
  -> collapse spaces: "MULLER, HANS"
```

### JUFO Level System

| Level | Meaning | Default Filter |
|-------|---------|----------------|
| 0 | Non-ranked | Excluded |
| 1 | Basic level | Included |
| 2 | Leading level | Included (high-impact) |
| 3 | Top level | Included (high-impact) |

Default across all functions: `jufo = 1:3` (excludes level 0).

---

## S3 Classes & Methods

The package defines 8 S3 classes. Each wraps analysis results with `print()`, `plot()`, and `summary()` methods.

### `cris_author_stats`

Returned by: `author_stats(data, name)`

```r
list(
  plot         = ggplot        # Publication trend line
  articles     = data.frame    # Publication list for the author
  descriptives = list(
    name, n_publications, n_years_active, year_range,
    pubs_per_year, jufo_distribution, pct_high_impact,
    most_productive_year, coauthor_count
  )
  gt_table     = gt            # Formatted publication list
  jufo_plot    = ggplot        # JUFO distribution bar chart
)
```

| Method | Behavior |
|--------|----------|
| `print(x)` | Summary header with key stats |
| `plot(x, type="trend")` | Publication count over years |
| `plot(x, type="jufo")` | JUFO level distribution |
| `summary(x)` | Returns `x$descriptives` |

### `cris_faculty_stats`

Returned by: `faculty_stats(data)`

```r
list(
  table        = data.frame    # faculty, publications, level_1/2/3, pct_high_impact
  summary      = list(...)     # From compare_faculties()
  gt_table     = gt            # Formatted comparison table
  descriptives = list(
    n_publications, n_faculties, pct_high_impact, top_faculty,
    sd_publications, cv_publications, gini_coefficient
  )
  plots = list(comparison, trends, jufo_dist)
  top_authors  = list(...)     # Top authors per faculty
)
```

| Method | Behavior |
|--------|----------|
| `print(x)` | Faculty count + top faculty |
| `plot(x, type="comparison")` | Bar chart |
| `plot(x, type="jufo")` | JUFO stacked bars |
| `plot(x, type="trends")` | Line plot over years |
| `summary(x)` | Returns `x$descriptives` |

### `cris_department_stats`

Returned by: `department_stats(data)` — same structure as `cris_faculty_stats`, department-level.

### `cris_unit_stats`

Returned by: `unit_stats(data)`

```r
list(
  table, summary, pattern_used, gt_table,
  descriptives = list(
    n_publications, n_units, pct_high_impact, top_unit,
    sd_publications, cv_publications, mean_pct_high,
    median_pct_high, gini_coefficient
  ),
  plots = list(comparison, trends, jufo_dist),
  growth = list(growth_plot, counts_plot, growth_line_plot),
  top_authors = list(...)
)
```

Extra plot types: `plot(x, type="growth")`, `plot(x, type="counts")`.

### `cris_productivity_stats`

Returned by: `productivity_stats(data)`

```r
list(
  most_productive_overall    = gt
  most_productive_by_level   = list(level_1, level_2, level_3)  # gt tables
  detailed_productivity      = gt
  author_positions           = gt       # First/Second/Last/Other
  fractional_contributions   = gt
  comprehensive_analysis     = gt       # All metrics combined
  raw_data                   = data.frame
  fractional_counts          = data.frame
  position_counts            = data.frame
  first_author_counts        = data.frame
  authors_by_level           = data.frame
  counting_methods_used      = character
  descriptives = list(
    n_authors, n_publications, mean_pubs_per_author,
    median_pubs_per_author, gini_productivity,
    top_author, top_author_count
  )
  plots = list(productivity_distribution, position_distribution)
)
```

| Method | Behavior |
|--------|----------|
| `plot(x, type="distribution")` | Histogram of publication counts |
| `plot(x, type="positions")` | Author position distribution |

### `cris_yearly_comparison`

Returned by: `compare_years(data, by)` or `yearly(data, by)`

```r
list(
  table     = data.frame    # Wide: entity | 2020 | 2021 | ... | total | change
  long_data = data.frame    # Long: entity, year, publications
  gt_table  = gt            # Color-coded year columns (white->steelblue)
  plots     = list(heatmap, lines)
  by        = character     # "author", "department", or "faculty"
  years     = integer       # All years in the data
  n         = integer       # Number of top entities shown
)
```

| Method | Behavior |
|--------|----------|
| `print(x)` | Comparison type + entity/year counts |
| `plot(x, type="heatmap")` | Tile plot with cell values |
| `plot(x, type="lines")` | Trend lines per entity |
| `summary(x)` | Growth summary stats |

### `cris_describe`

Returned by: `describe(data, by=NULL)`

```r
list(
  overall = list(
    n_publications, n_authors, year_range, n_years,
    jufo_distribution, pct_high_impact, mean_pubs_per_year
  )
  by_group = data.frame or NULL   # Breakdown if by= specified
)
```

### `cris_report`

Returned by: `university_stats(data)` and `generate_full_report(data)`

Contains: `summary_table`, `by_jufo`, `by_type`, `by_faculty`, `by_department`, `by_unit`, `top_authors`, `gt_table`, `plots`.

---

## Function Reference

### Data I/O & Preparation

| Function | Purpose | Returns |
|----------|---------|---------|
| `read_cris_data(path)` | Read Excel files from directory | data.frame |
| `clean_author_data(data)` | Split authors, standardize names | data.frame (author-level) |
| `simplify_author_string(x)` | Uppercase ASCII normalization | character vector |
| `prepare_cris_data(path, years, jufo)` | Full pipeline: read + clean + hierarchy | data.frame |
| `process_data(...)` | Alias for `prepare_cris_data` | data.frame |
| `add_hierarchy_columns(data)` | Map units to departments/faculties | data.frame |
| `get_unit_translations()` | Load 179-row unit mapping table | data.frame |

### Stats Functions (return S3 objects)

| Function | S3 Class | Key Parameters |
|----------|----------|----------------|
| `author_stats(data, name)` | `cris_author_stats` | `years`, `jufo` |
| `faculty_stats(data)` | `cris_faculty_stats` | `years`, `jufo`, `top_n` |
| `department_stats(data)` | `cris_department_stats` | `pattern`, `faculty`, `years`, `jufo`, `top_n` |
| `unit_stats(data)` | `cris_unit_stats` | `pattern`, `years`, `jufo`, `top_n` |
| `productivity_stats(data)` | `cris_productivity_stats` | `years`, `top_n`, `levels` |
| `describe(data, by)` | `cris_describe` | `by` = year/faculty/department/unit |
| `university_stats(data)` | `cris_report` | `years`, `jufo`, `top_n` |
| `compare_years(data, by)` | `cris_yearly_comparison` | `by`, `n`, `years`, `jufo` |

**Aliases:** `stats_author`, `stats_faculty`, `stats_department`, `stats_unit`, `stats_productivity`, `stats_university`, `stats_describe`, `yearly`.

### Ranking Functions

All return `list(table, gt_table, plot)`.

| Function | Ranks By | Default n |
|----------|----------|-----------|
| `top_authors(data, n)` | Publication count | 20 |
| `top_faculties(data, n)` | Publication count | 20 |
| `top_departments(data, n)` | Publication count | 20 |
| `top_units(data, n)` | Publication count | 20 |
| `top_venues(data, n)` | Publication count | 20 |

### Comparison Functions

| Function | Returns | Key Feature |
|----------|---------|-------------|
| `compare_faculties(data)` | list(table, summary) | JUFO breakdown per faculty |
| `compare_departments(data, pattern)` | list(table, summary) | Wildcard pattern filtering |
| `compare_units(data, pattern)` | list(table, summary) | Wildcard pattern filtering |
| `compare_authors(data, top_n)` | list(table, gt_table, summary) | Years active, pubs/year |

### Visualization Functions

| Function | Plot Type | Key Parameters |
|----------|-----------|----------------|
| `plot_comparison(data, level)` | Bar chart | `fill_by` = total/jufo/faculty |
| `plot_trends(data, level)` | Line chart | `top_n`, `show_points` |
| `plot_jufo_distribution(data, by)` | Stacked bars | `proportional` = TRUE/FALSE |
| `plot_unit_growth(data)` | Growth analysis | `n_years`, `top_n`, `keyword` |
| `create_all_plots(data)` | All 6 standard plots | — |

### Analysis Functions

| Function | Purpose | Returns |
|----------|---------|---------|
| `compute_fractional_counts(data)` | 6 counting methods | data.frame (one row/author) |
| `generate_author_productivity_tables(data)` | Comprehensive productivity | list (14 items) |
| `analyze_author_publications(data, name)` | Individual author trend | list(plot, articles) |
| `generate_publication_stats(data)` | Basic pub stats | list(4 items) |
| `generate_full_report(data)` | Everything combined | list(9 items) |

### Utility Functions

| Function | Purpose |
|----------|---------|
| `validate_cris_data(data, cols)` | Input validation (errors if cols missing) |
| `compute_gini(x)` | Gini coefficient (0=equal, 1=unequal) |
| `build_stats_gt(table, title)` | Shared gt table formatter |
| `detect_venue_column(cols)` | Auto-detect journal column name |
| `build_top_result(df, col, title, n)` | Builder for top_* function results |
| `filter_by_criteria(data, years, jufo)` | Filter by year and JUFO level |
| `filter_by_pattern(data, col, pattern)` | Wildcard glob matching |
| `get_unit_abbreviations()` | 34 UEF unit abbreviations |
| `abbreviate_units(units)` | Map unit names to abbreviations |
| `add_unit_column(data)` | Add abbreviated unit column |

---

## Fractional Counting Methods

Six methods for distributing publication credit among co-authors. Each normalizes so the total credit per paper sums to 1.

### 1. Standard (Equal Share)
Each author gets `1/n` where n = number of authors.

```
3 authors: each gets 0.333
```

### 2. Harmonic
Weight by `1/position`, then normalize.

```
3 authors: 1/1 + 1/2 + 1/3 = 1.833
  Author 1: 0.545, Author 2: 0.273, Author 3: 0.182
```

### 3. Proportional
Linear decrease by position: weight = `(n - position + 1) / sum(1:n)`.

```
3 authors: weights 3, 2, 1 -> sum 6
  Author 1: 0.500, Author 2: 0.333, Author 3: 0.167
```

### 4. First-Last
Extra weight to first and last authors (default multiplier: 2).

```
3 authors: First and Last get 2x, Middle gets proportional
```

### 5. Geometric
Exponential decay: `0.5^position`, normalized.

```
3 authors: 0.5, 0.25, 0.125 -> sum 0.875
  Author 1: 0.571, Author 2: 0.286, Author 3: 0.143
```

### 6. Position Weighted
Custom weights per position (default: `c(1, 0.8, 0.6, 0.4)` for positions 1-4+), normalized.

### Output Columns

| Column | Method |
|--------|--------|
| `Standard_Fractional` | Standard |
| `Harmonic_Fractional` | Harmonic |
| `Proportional_Fractional` | Proportional |
| `FirstLast_Fractional` | First-Last |
| `Geometric_Fractional` | Geometric |
| `PositionWeighted_Fractional` | Position Weighted |

---

## Organizational Hierarchy

The package maps UEF organizational units into a 3-level hierarchy:

```
Faculty (5)
  └── Department
        └── Unit (179 known variants)
```

### Faculties

| Faculty | Example Departments |
|---------|--------------------|
| Health Sciences | Medicine, Nursing, Pharmacy, Dentistry |
| Science and Forestry | Computing, Physics, Chemistry, Biology, Forestry |
| Social Sciences and Business | Business, Law, Education, Social Sciences |
| Philosophical | Humanities, Theology, History |
| Administration | Library, Administration |

### Unit Translation Table

- **179 rows** mapping unit name variants (including Finnish names) to standardized forms
- Columns: `unit`, `unit_en`, `unit_abbrev`, `department`, `faculty`
- Loaded via `get_unit_translations()`
- Applied via `add_hierarchy_columns()`
- Unmatched units: original name used as abbreviation, department/faculty set to NA

### Wildcard Pattern Filtering

Functions like `compare_departments()` and `compare_units()` support glob patterns:

```r
compare_departments(data, pattern = "*Science*")   # contains
compare_departments(data, pattern = "Computing*")  # starts with
compare_departments(data, pattern = "*Education")  # ends with
compare_departments(data, pattern = "*")           # all (default)
```

---

## Visualization System

### Plot Types by S3 Class

| Class | Default Plot | Alt Plot Types |
|-------|-------------|----------------|
| `cris_author_stats` | Trend line | `"jufo"` |
| `cris_faculty_stats` | Comparison bars | `"jufo"`, `"trends"` |
| `cris_department_stats` | Comparison bars | `"jufo"`, `"trends"` |
| `cris_unit_stats` | Comparison bars | `"jufo"`, `"trends"`, `"growth"`, `"counts"` |
| `cris_productivity_stats` | Distribution histogram | `"positions"` |
| `cris_yearly_comparison` | Heatmap | `"lines"` |

### Heatmap (`compare_years`)

- `geom_tile(color = "white")` + `geom_text(aes(label = publications))`
- `scale_fill_gradient(low = "white", high = "steelblue")`
- Entities ordered by total (highest on top)

### gt Tables

- Color-coded year columns via `gt::data_color()` (white to steelblue)
- Formatted numbers with `gt::fmt_number()`
- Consistent styling via `build_stats_gt()` helper

---

## File Structure

```
cris_stats/
├── DESCRIPTION                  # Package metadata, dependencies
├── NAMESPACE                    # 67 exports, imports, S3 methods
├── LICENSE / LICENSE.md
├── README.md
├── .Rbuildignore / .gitignore
│
├── R/                           # Source code (16 files)
│   ├── crisstats-package.R      # Package-level docs & imports
│   ├── data.R                   # Dataset documentation
│   ├── data-reader.R            # read_cris_data()
│   ├── data-cleaning.R          # clean_author_data(), simplify_author_string()
│   ├── prepare-data.R           # prepare_cris_data(), add_hierarchy_columns()
│   ├── process-data.R           # process_data(), all stats/top/describe wrappers,
│   │                            #   compare_years(), S3 methods
│   ├── compare.R                # compare_faculties/departments/units/authors(),
│   │                            #   filter_by_criteria(), filter_by_pattern()
│   ├── fractional-counting.R    # compute_fractional_counts()
│   ├── author-productivity.R    # generate_author_productivity_tables()
│   ├── author-analysis.R        # analyze_author_publications()
│   ├── publication-stats.R      # generate_publication_stats()
│   ├── unit-mapping.R           # get_unit_abbreviations(), abbreviate_units()
│   ├── unit-analysis.R          # plot_unit_growth()
│   ├── auto-plots.R             # plot_comparison(), plot_trends(), plot_jufo_distribution()
│   ├── full-report.R            # generate_full_report()
│   └── utils.R                  # validate_cris_data(), compute_gini(), build_stats_gt(),
│                                #   detect_venue_column(), build_top_result()
│
├── data/                        # Built-in datasets (.rda)
│   ├── unit_translations.rda    # 179 unit name mappings
│   ├── uef_units.rda            # 34 unit abbreviations (legacy)
│   └── sample_publications.rda  # 100-row example dataset
│
├── data-raw/                    # Scripts to generate built-in data
│
├── man/                         # roxygen2 documentation (35 .Rd files)
│
├── tests/
│   ├── testthat.R
│   └── testthat/
│       ├── test-fractional-counting.R
│       ├── test-data-cleaning.R
│       └── test-unit-mapping.R
│
├── vignettes/
│
└── crisstats-demo.Rmd           # Full demo notebook
```

---

## Dependencies

### Imports

| Package | Used For |
|---------|----------|
| `dplyr` | Data manipulation (full namespace) |
| `ggplot2` | Visualization (full namespace) |
| `gt` | Formatted HTML tables |
| `tidyr` | Pivoting, completing, reshaping |
| `stringr` | String manipulation and regex |
| `readxl` | Reading Excel files |
| `janitor` | Column name cleaning |
| `splitstackshape` | Splitting author strings |
| `purrr` | Functional programming (`map_dfr`) |
| `rlang` | Tidy evaluation (`.data`, `:=`, `sym`) |
| `scales` | Axis breaks formatting |
| `datawizard` | Column prefix, empty column removal |
| `tools` | `toTitleCase` for plot titles |

### Suggests

| Package | Used For |
|---------|----------|
| `testthat` (>= 3.0.0) | Unit testing |
| `knitr` | Document generation |
| `rmarkdown` | R Markdown rendering |

---

## Workflow Examples

### Quick University Overview

```r
data <- process_data("Data/", years = 2025)
describe(data)
uni <- university_stats(data)
uni$gt_table
```

### Author Deep Dive

```r
data <- process_data("Data/", years = 2020:2025)

# Individual author
me <- author_stats(data, "Mohammed Saqr")
me$gt_table          # Publication list
plot(me)             # Trend
plot(me, "jufo")     # JUFO distribution
summary(me)          # Key stats

# Productivity across all authors
prod <- productivity_stats(data, top_n = 50)
prod$comprehensive_analysis
plot(prod, "distribution")
plot(prod, "positions")
```

### Multi-Year Comparison

```r
data_all <- process_data("Data/", years = 2020:2025)

# Faculty trends
yf <- compare_years(data_all, "faculty")
yf$gt_table
plot(yf, "lines")

# Top 15 authors over time
ya <- compare_years(data_all, "author", n = 15)
plot(ya)             # Heatmap with cell values
ya$gt_table

# Department trends
yd <- compare_years(data_all, "department", n = 15)
plot(yd)             # Heatmap
plot(yd, "lines")    # Line chart
```

### Filtered Comparisons

```r
# Only computing-related departments
dept <- department_stats(data, pattern = "*Comput*")
plot(dept)

# Only Health Sciences faculty
dept_hs <- department_stats(data, faculty = "Health Sciences")
dept_hs$gt_table

# Authors with 5+ publications
auth <- compare_authors(data, min_publications = 5)
auth$gt_table
```

### Fractional Counting

```r
data <- process_data("Data/")
frac <- compute_fractional_counts(
  data,
  counting_methods = c("standard", "harmonic", "first_last")
)
# Compare credit distribution across methods
head(frac[order(-frac$Standard_Fractional), ])
```

---

## Design Patterns

### 1. Rich S3 Return Objects

Every main analysis function returns an S3 object containing raw data, formatted tables, plots, and descriptive statistics — accessible via `$` or generic methods:

```r
result <- faculty_stats(data)
result$table         # Raw data frame
result$gt_table      # Formatted gt table
plot(result)         # ggplot visualization
summary(result)      # Key statistics
```

### 2. Consistent Default Filtering

All analysis functions share the same defaults:
- `years = NULL` (use all years in data)
- `jufo = 1:3` (exclude JUFO level 0)
- `top_n = 20` (show top 20 entities)

### 3. Wildcard Pattern Matching

`compare_departments()`, `compare_units()`, and their stat wrappers accept glob patterns via the `pattern` parameter, converted internally to regex with `glob_to_regex()`.

### 4. Deduplication Strategy

- `read_cris_data()`: deduplicates by `publication_id` (one row per pub)
- After `clean_author_data()`: data is at author-level (one row per author per pub)
- Analysis functions use `distinct(publication_id, group_col)` to count each pub once per entity

### 5. Helper Function Reuse

Common operations extracted into utilities:
- `filter_by_criteria()` — shared year/JUFO filtering
- `build_top_result()` — shared ranking table + bar plot builder
- `build_stats_gt()` — shared gt table formatting
- `validate_cris_data()` — shared input validation

### 6. Function Aliases

Two naming conventions for user convenience:
- Primary: `author_stats()`, `faculty_stats()`, ...
- Reversed: `stats_author()`, `stats_faculty()`, ...
- Short: `process_data()` = `prepare_cris_data()`, `yearly()` = `compare_years()`
