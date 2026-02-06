#' @importFrom stats median
NULL

#' Compare Units
#'
#' @description
#' Compares publication statistics across organizational units. Supports
#' wildcard pattern matching for flexible filtering.
#'
#' @param data A data frame from \code{\link{prepare_cris_data}}.
#' @param pattern Character string. Wildcard pattern to filter units.
#'   Use \code{"*"} for all units, \code{"Computing*"} for units starting with
#'   "Computing", \code{"*Science*"} for units containing "Science", etc.
#'   Default is \code{"*"} (all units).
#' @param years Numeric vector. Years to include. If NULL, uses all years.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#' @param top_n Integer. Maximum number of units to return. Default is 50.
#' @param unit_col Character string. Name of the unit column.
#'   Default is \code{"first_authors_unit"}.
#'
#' @return A list containing:
#'   \describe{
#'     \item{table}{Data frame with unit statistics}
#'     \item{summary}{Summary statistics}
#'     \item{pattern_used}{The pattern used for filtering}
#'   }
#'
#' @examples
#' \dontrun{
#' data <- prepare_cris_data("Data/")
#'
#' # All units
#' compare_units(data)
#'
#' # Units containing "Computing"
#' compare_units(data, pattern = "*Computing*")
#'
#' # Specific years
#' compare_units(data, years = 2022:2024)
#' }
#'
#' @export
compare_units <- function(data,
                          pattern = "*",
                          years = NULL,
                          jufo = 1:3,
                          top_n = 50,
                          unit_col = "first_authors_unit") {

  # Validate input
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (!(unit_col %in% names(data))) {
    stop("Column '", unit_col, "' not found in data")
  }

  # Apply year filter
  filtered <- filter_by_criteria(data, years = years, jufo = jufo)

  # Apply pattern filter
  filtered <- filter_by_pattern(filtered, unit_col, pattern)

  # Deduplicate to one row per publication per unit for accurate JUFO counts
  deduped <- filtered %>%
    dplyr::distinct(.data$publication_id, .data[[unit_col]],
                    .data$jufo_level_of_publication)

  # Calculate statistics
  stats <- deduped %>%
    dplyr::group_by(.data[[unit_col]]) %>%
    dplyr::summarise(
      publications = dplyr::n_distinct(.data$publication_id),
      level_1 = sum(.data$jufo_level_of_publication == "1", na.rm = TRUE),
      level_2 = sum(.data$jufo_level_of_publication == "2", na.rm = TRUE),
      level_3 = sum(.data$jufo_level_of_publication == "3", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      level_2_3 = .data$level_2 + .data$level_3,
      pct_high_impact = round(100 * .data$level_2_3 / .data$publications, 1)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$publications)) %>%
    dplyr::slice_head(n = top_n)

  # Add abbreviations if available
  if ("unit_abbrev" %in% names(filtered)) {
    abbrev_map <- filtered %>%
      dplyr::select(dplyr::all_of(c(unit_col, "unit_abbrev"))) %>%
      dplyr::distinct()
    stats <- stats %>%
      dplyr::left_join(abbrev_map, by = unit_col)
  }

  list(
    table = stats,
    summary = list(
      total_units = nrow(stats),
      total_publications = sum(stats$publications),
      mean_publications = round(mean(stats$publications), 1),
      median_publications = median(stats$publications)
    ),
    pattern_used = pattern
  )
}

#' Compare Departments
#'
#' @description
#' Compares publication statistics across departments. Supports wildcard
#' pattern matching and filtering by faculty.
#'
#' @param data A data frame from \code{\link{prepare_cris_data}} with
#'   hierarchy columns added.
#' @param pattern Character string. Wildcard pattern to filter departments.
#'   Default is \code{"*"} (all departments).
#' @param faculty Character string. Wildcard pattern to filter by faculty.
#'   Default is NULL (all faculties).
#' @param years Numeric vector. Years to include. If NULL, uses all years.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#' @param top_n Integer. Maximum number of departments to return. Default is 50.
#'
#' @return A list containing:
#'   \describe{
#'     \item{table}{Data frame with department statistics}
#'     \item{summary}{Summary statistics}
#'     \item{pattern_used}{The pattern used for filtering}
#'   }
#'
#' @examples
#' \dontrun{
#' data <- prepare_cris_data("Data/")
#'
#' # All departments
#' compare_departments(data)
#'
#' # Departments in Health Sciences
#' compare_departments(data, faculty = "Health*")
#'
#' # Departments matching pattern
#' compare_departments(data, pattern = "*Science*")
#' }
#'
#' @export
compare_departments <- function(data,
                                pattern = "*",
                                faculty = NULL,
                                years = NULL,
                                jufo = 1:3,
                                top_n = 50) {

  # Validate input
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (!("department" %in% names(data))) {
    stop("Column 'department' not found. Use prepare_cris_data() with add_hierarchy = TRUE")
  }

  # Apply year filter
  filtered <- filter_by_criteria(data, years = years, jufo = jufo)

  # Apply faculty filter if specified
  if (!is.null(faculty)) {
    if ("faculty" %in% names(filtered)) {
      filtered <- filter_by_pattern(filtered, "faculty", faculty)
    } else {
      warning("Column 'faculty' not found. Faculty filtering skipped.")
    }
  }

  # Apply department pattern filter
  filtered <- filter_by_pattern(filtered, "department", pattern)

  # Deduplicate to one row per publication per department for accurate JUFO counts
  deduped <- filtered %>%
    dplyr::distinct(.data$publication_id, .data$department,
                    .data$jufo_level_of_publication, .data$faculty)

  # Calculate statistics
  stats <- deduped %>%
    dplyr::group_by(.data$department) %>%
    dplyr::summarise(
      publications = dplyr::n_distinct(.data$publication_id),
      level_1 = sum(.data$jufo_level_of_publication == "1", na.rm = TRUE),
      level_2 = sum(.data$jufo_level_of_publication == "2", na.rm = TRUE),
      level_3 = sum(.data$jufo_level_of_publication == "3", na.rm = TRUE),
      faculty = dplyr::first(.data$faculty),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      level_2_3 = .data$level_2 + .data$level_3,
      pct_high_impact = round(100 * .data$level_2_3 / .data$publications, 1)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$publications)) %>%
    dplyr::slice_head(n = top_n)

  list(
    table = stats,
    summary = list(
      total_departments = nrow(stats),
      total_publications = sum(stats$publications),
      mean_publications = round(mean(stats$publications), 1),
      median_publications = median(stats$publications)
    ),
    pattern_used = pattern
  )
}

#' Compare Faculties
#'
#' @description
#' Compares publication statistics across faculties.
#'
#' @param data A data frame from \code{\link{prepare_cris_data}} with
#'   hierarchy columns added.
#' @param years Numeric vector. Years to include. If NULL, uses all years.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#'
#' @return A list containing:
#'   \describe{
#'     \item{table}{Data frame with faculty statistics}
#'     \item{summary}{Summary statistics}
#'   }
#'
#' @examples
#' \dontrun{
#' data <- prepare_cris_data("Data/")
#' compare_faculties(data)
#' compare_faculties(data, years = 2022:2024)
#' }
#'
#' @export
compare_faculties <- function(data,
                              years = NULL,
                              jufo = 1:3) {

  # Validate input
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (!("faculty" %in% names(data))) {
    stop("Column 'faculty' not found. Use prepare_cris_data() with add_hierarchy = TRUE")
  }

  # Apply filters
  filtered <- filter_by_criteria(data, years = years, jufo = jufo)

  # Remove NA faculties
  filtered <- filtered %>%
    dplyr::filter(!is.na(.data$faculty))

  # Count distinct departments per faculty before deduplication
  dept_counts <- filtered %>%
    dplyr::group_by(.data$faculty) %>%
    dplyr::summarise(departments = dplyr::n_distinct(.data$department),
                     .groups = "drop")

  # Deduplicate to one row per publication per faculty for accurate JUFO counts
  deduped <- filtered %>%
    dplyr::distinct(.data$publication_id, .data$faculty,
                    .data$jufo_level_of_publication)

  # Calculate statistics
  stats <- deduped %>%
    dplyr::group_by(.data$faculty) %>%
    dplyr::summarise(
      publications = dplyr::n_distinct(.data$publication_id),
      level_1 = sum(.data$jufo_level_of_publication == "1", na.rm = TRUE),
      level_2 = sum(.data$jufo_level_of_publication == "2", na.rm = TRUE),
      level_3 = sum(.data$jufo_level_of_publication == "3", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(dept_counts, by = "faculty") %>%
    dplyr::mutate(
      level_2_3 = .data$level_2 + .data$level_3,
      pct_high_impact = round(100 * .data$level_2_3 / .data$publications, 1)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$publications))

  list(
    table = stats,
    summary = list(
      total_faculties = nrow(stats),
      total_publications = sum(stats$publications),
      mean_publications = round(mean(stats$publications), 1)
    )
  )
}

#' Compare Authors
#'
#' @description
#' Compares publication productivity across authors.
#'
#' @param data A data frame from \code{\link{prepare_cris_data}}.
#' @param top_n Integer. Number of top authors to return. Default is 100.
#' @param years Numeric vector. Years to include. If NULL, uses all years.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#' @param min_publications Integer. Minimum publications to include an author.
#'   Default is 1.
#'
#' @return A list containing:
#'   \describe{
#'     \item{table}{Data frame with author statistics}
#'     \item{gt_table}{Formatted gt table of top authors}
#'     \item{summary}{Summary statistics}
#'   }
#'
#' @examples
#' \dontrun{
#' data <- prepare_cris_data("Data/")
#'
#' # Top 100 authors
#' compare_authors(data, top_n = 100)
#'
#' # Authors with at least 5 publications
#' compare_authors(data, min_publications = 5)
#' }
#'
#' @export
compare_authors <- function(data,
                            top_n = 100,
                            years = NULL,
                            jufo = 1:3,
                            min_publications = 1) {

  # Validate input
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  author_col <- if ("Authorsclean" %in% names(data)) "Authorsclean" else "authors"
  if (!(author_col %in% names(data))) {
    stop("No author column found in data")
  }

  # Apply filters
  filtered <- filter_by_criteria(data, years = years, jufo = jufo)

  # Calculate author statistics
  stats <- filtered %>%
    dplyr::group_by(.data[[author_col]]) %>%
    dplyr::summarise(
      publications = dplyr::n_distinct(.data$publication_id),
      level_1 = sum(.data$jufo_level_of_publication == "1", na.rm = TRUE),
      level_2 = sum(.data$jufo_level_of_publication == "2", na.rm = TRUE),
      level_3 = sum(.data$jufo_level_of_publication == "3", na.rm = TRUE),
      years_active = dplyr::n_distinct(.data$publication_year),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      level_2_3 = .data$level_2 + .data$level_3,
      pct_high_impact = round(100 * .data$level_2_3 / .data$publications, 1),
      pubs_per_year = round(.data$publications / .data$years_active, 2)
    ) %>%
    dplyr::filter(.data$publications >= min_publications) %>%
    dplyr::arrange(dplyr::desc(.data$publications)) %>%
    dplyr::slice_head(n = top_n)

  # Rename author column for clarity
  stats <- stats %>%
    dplyr::rename(author = dplyr::all_of(author_col))

  # Create gt table
  gt_table <- stats %>%
    dplyr::select(.data$author, .data$publications, .data$level_1,
                  .data$level_2, .data$level_3, .data$level_2_3,
                  .data$pct_high_impact, .data$pubs_per_year) %>%
    gt::gt() %>%
    gt::tab_header(
      title = "Author Productivity Comparison",
      subtitle = paste("Top", min(top_n, nrow(stats)), "authors")
    ) %>%
    gt::cols_label(
      author = "Author",
      publications = "Total",
      level_1 = "L1",
      level_2 = "L2",
      level_3 = "L3",
      level_2_3 = "L2+L3",
      pct_high_impact = "% High",
      pubs_per_year = "Per Year"
    ) %>%
    gt::fmt_number(columns = .data$pubs_per_year, decimals = 1)

  list(
    table = stats,
    gt_table = gt_table,
    summary = list(
      total_authors = nrow(stats),
      total_publications = sum(stats$publications),
      mean_publications = round(mean(stats$publications), 1),
      median_publications = median(stats$publications),
      max_publications = max(stats$publications)
    )
  )
}

# =============================================================================
# Helper Functions
# =============================================================================

#' Filter Data by Pattern
#'
#' @description
#' Internal function to filter data by wildcard pattern.
#'
#' @param data Data frame to filter.
#' @param col Column name to filter on.
#' @param pattern Wildcard pattern (* for any characters).
#'
#' @return Filtered data frame.
#'
#' @keywords internal
filter_by_pattern <- function(data, col, pattern) {
  if (is.null(pattern) || pattern == "*") {
    return(data)
  }

  # Convert wildcard to regex
  regex_pattern <- glob_to_regex(pattern)

  data %>%
    dplyr::filter(stringr::str_detect(.data[[col]], stringr::regex(regex_pattern, ignore_case = TRUE)))
}

#' Filter Data by Year and JUFO Criteria
#'
#' @description
#' Internal function to filter data by year and JUFO level.
#'
#' @param data Data frame to filter.
#' @param years Numeric vector of years (NULL for all).
#' @param jufo Numeric vector of JUFO levels (NULL for all).
#'
#' @return Filtered data frame.
#'
#' @keywords internal
filter_by_criteria <- function(data, years = NULL, jufo = NULL) {
  result <- data

  # Filter by year
  if (!is.null(years) && "publication_year" %in% names(data)) {
    result <- result %>%
      dplyr::filter(.data$publication_year %in% years)
  }

  # Filter by JUFO
  if (!is.null(jufo) && "jufo_level_of_publication" %in% names(data)) {
    result <- result %>%
      dplyr::mutate(
        jufo_level_of_publication = as.character(.data$jufo_level_of_publication)
      ) %>%
      dplyr::filter(.data$jufo_level_of_publication %in% as.character(jufo))
  }

  result
}

#' Convert Glob Pattern to Regex
#'
#' @description
#' Internal function to convert wildcard glob patterns to regex.
#'
#' @param pattern Glob pattern with * wildcards.
#'
#' @return Regex pattern string.
#'
#' @keywords internal
glob_to_regex <- function(pattern) {
  # Escape regex special characters except *
  pattern <- gsub("([.+?^${}()|\\[\\]\\\\])", "\\\\\\1", pattern)
  # Convert * to .*
 pattern <- gsub("\\*", ".*", pattern)
  # Anchor the pattern
  paste0("^", pattern, "$")
}
