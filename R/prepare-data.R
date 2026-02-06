#' Prepare CRIS Data - One-Stop Data Preparation
#'
#' @description
#' Comprehensive data preparation function that reads Excel files, deduplicates,
#' cleans author names, and adds organizational hierarchy (unit, department,
#' faculty). This is the recommended entry point for analyzing CRIS data.
#'
#' @param path Character string. Path to a single Excel file or a folder
#'   containing Excel files.
#' @param years Numeric vector. Years to include in the data. If NULL (default),
#'   includes all years.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}
#'   (levels 1, 2, and 3). Use \code{0:3} to include all levels.
#' @param pattern Character string. Glob pattern for matching files when
#'   \code{path} is a directory. Default is \code{"*.xlsx"}.
#' @param clean_authors Logical. If TRUE (default), cleans and standardizes
#'   author names.
#' @param add_hierarchy Logical. If TRUE (default), adds unit_abbrev, department,
#'   and faculty columns based on the unit translations data.
#'
#' @return A data frame with cleaned and enriched publication data containing:
#'   \describe{
#'     \item{publication_id}{Unique identifier for each publication}
#'     \item{authors}{Original author list}
#'     \item{Authorsclean}{Cleaned author names (if clean_authors = TRUE)}
#'     \item{first_authors_unit}{Original unit name}
#'     \item{unit_abbrev}{Abbreviated unit name for plots}
#'     \item{department}{Parent department}
#'     \item{faculty}{Parent faculty}
#'   }
#'   Plus all other columns from the original data.
#'
#' @details
#' The function performs the following operations in order:
#' \enumerate{
#'   \item Reads all matching Excel files using \code{\link{read_cris_data}}
#'   \item Deduplicates by \code{publication_id}
#'   \item Filters by year (if specified)
#'   \item Filters by JUFO level (if specified)
#'   \item Cleans author names (if requested)
#'   \item Adds organizational hierarchy columns (if requested)
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage - read and prepare all data
#' data <- prepare_cris_data("Data/")
#'
#' # Filter to specific years and JUFO levels
#' data <- prepare_cris_data("Data/", years = 2020:2025, jufo = 1:3)
#'
#' # Include all JUFO levels including 0
#' data <- prepare_cris_data("Data/", jufo = 0:3)
#'
#' # Skip author cleaning for faster processing
#' data <- prepare_cris_data("Data/", clean_authors = FALSE)
#' }
#'
#' @seealso
#' \code{\link{read_cris_data}} for reading raw data,
#' \code{\link{clean_author_data}} for author name cleaning,
#' \code{\link{generate_full_report}} for comprehensive analysis
#'
#' @export
prepare_cris_data <- function(path,
                              years = NULL,
                              jufo = 1:3,
                              pattern = "*.xlsx",
                              clean_authors = TRUE,
                              add_hierarchy = TRUE) {

  # Read raw data
  message("Step 1/4: Reading data...")
  data <- read_cris_data(path, pattern = pattern, remove_duplicates = TRUE)

  # Filter by year if specified
  if (!is.null(years)) {
    if ("publication_year" %in% names(data)) {
      n_before <- nrow(data)
      data <- data %>%
        dplyr::filter(.data$publication_year %in% years)
      message("Step 2/4: Filtered to years ", min(years), "-", max(years),
              " (", nrow(data), " of ", n_before, " publications)")
    } else {
      warning("Column 'publication_year' not found. Year filtering skipped.")
    }
  } else {
    message("Step 2/4: No year filter applied")
  }

  # Filter by JUFO level if specified
  if (!is.null(jufo)) {
    if ("jufo_level_of_publication" %in% names(data)) {
      n_before <- nrow(data)
      # Convert to character for comparison (JUFO levels may be stored as text)
      data <- data %>%
        dplyr::mutate(
          jufo_level_of_publication = as.character(.data$jufo_level_of_publication)
        ) %>%
        dplyr::filter(.data$jufo_level_of_publication %in% as.character(jufo))
      message("Step 3/4: Filtered to JUFO levels ", paste(jufo, collapse = ", "),
              " (", nrow(data), " of ", n_before, " publications)")
    } else {
      warning("Column 'jufo_level_of_publication' not found. JUFO filtering skipped.")
    }
  } else {
    message("Step 3/4: No JUFO filter applied")
  }

  # Clean author names if requested
  if (clean_authors) {
    if ("authors" %in% names(data)) {
      message("Step 4/4: Cleaning author names...")
      data <- clean_author_data(data, exclude_et_al = TRUE)
    } else {
      warning("Column 'authors' not found. Author cleaning skipped.")
    }
  } else {
    message("Step 4/4: Author cleaning skipped")
  }

  # Add organizational hierarchy if requested
  if (add_hierarchy) {
    data <- add_hierarchy_columns(data)
  }

  message("Data preparation complete: ", nrow(data), " rows")
  return(data)
}

#' Add Organizational Hierarchy Columns
#'
#' @description
#' Adds unit_en (English unit name), unit_abbrev, department, and faculty columns
#' to data based on the unit translations mapping. Handles both Finnish and
#' English unit names from CRIS data.
#'
#' @param data A data frame containing a unit column.
#' @param unit_col Character string. Name of the column containing unit names.
#'   Default is \code{"first_authors_unit"}.
#'
#' @return Data frame with added hierarchy columns:
#'   \describe{
#'     \item{unit_en}{Standardized English unit name}
#'     \item{unit_abbrev}{Short abbreviation for plots}
#'     \item{department}{Parent department}
#'     \item{faculty}{Parent faculty}
#'   }
#'
#' @details
#' Uses the built-in \code{unit_translations} dataset to map Finnish and English
#' unit names to their standardized forms. Unknown units are kept as-is for
#' abbreviation with NA for department and faculty.
#'
#' @examples
#' \dontrun{
#' data <- add_hierarchy_columns(raw_data)
#' }
#'
#' @export
add_hierarchy_columns <- function(data, unit_col = "first_authors_unit") {

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (!(unit_col %in% names(data))) {
    warning("Column '", unit_col, "' not found. Hierarchy columns not added.")
    return(data)
  }

  # Get the unit translations data
  translations <- get_unit_translations()

  # Select only needed columns for the join
  join_cols <- translations[, c("unit", "unit_en", "unit_abbrev", "department", "faculty")]

  # Add hierarchy columns via left join
  data <- data %>%
    dplyr::left_join(
      join_cols,
      by = stats::setNames("unit", unit_col)
    )

  # For any unmatched units, use the original name as abbreviation
  if ("unit_abbrev" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(
        unit_abbrev = ifelse(is.na(.data$unit_abbrev),
                             .data[[unit_col]],
                             .data$unit_abbrev)
      )
  }

  # For unmatched units, use original name as unit_en
  if ("unit_en" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(
        unit_en = ifelse(is.na(.data$unit_en),
                         .data[[unit_col]],
                         .data$unit_en)
      )
  }

  return(data)
}

#' Get Unit Translations Data
#'
#' @description
#' Returns the unit translations mapping with Finnish and English unit names,
#' abbreviations, departments, and faculties.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{unit}{Original unit name (Finnish or English)}
#'     \item{unit_en}{Standardized English unit name}
#'     \item{unit_abbrev}{Short abbreviation for plots}
#'     \item{department}{Parent department}
#'     \item{faculty}{Parent faculty}
#'   }
#'
#' @importFrom utils data
#'
#' @examples
#' translations <- get_unit_translations()
#' head(translations)
#'
#' # See all faculties
#' table(translations$faculty)
#'
#' @export
get_unit_translations <- function() {
  # Load the package data
  # The unit_translations dataset is loaded from data/unit_translations.rda
  unit_translations <- NULL
  utils::data("unit_translations", package = "crisstats", envir = environment())
  return(unit_translations)
}
