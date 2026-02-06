#' Get UEF Unit Abbreviations
#'
#' @description
#' Returns a named character vector mapping full UEF (University of Eastern
#' Finland) unit names to their abbreviated forms. These abbreviations are
#' suitable for use in plots and tables where space is limited.
#'
#' @param custom Named character vector of custom abbreviations to add or
#'   override. Names should be full unit names, values should be abbreviations.
#'   Default is NULL (use only built-in abbreviations).
#'
#' @return A named character vector where names are full unit names and
#'   values are abbreviated names.
#'
#' @examples
#' # Get default abbreviations
#' abbrevs <- get_unit_abbreviations()
#' abbrevs["School of Computing"]  # Returns "Computing"
#'
#' # Add custom abbreviations
#' custom <- c("My Custom Unit" = "MCU")
#' abbrevs <- get_unit_abbreviations(custom = custom)
#'
#' @export
get_unit_abbreviations <- function(custom = NULL) {

  # Default UEF unit abbreviations
  abbreviations <- c(
    "A.I. Virtanen Institute" = "AI Virtanen Inst.",
    "Biomedicine" = "Biomedicine",
    "Business School" = "Business",
    "Clinical Medicine" = "Clinical Med.",
    "Dentistry" = "Dentistry",
    "Department of Chemistry and Sustainable Technology" = "Chemistry & Sust. Tech.",
    "Department of Environmental and Biological Sciences" = "Env. & Bio. Sci.",
    "Department of Geographical and Historical Studies" = "Geog. & Hist.",
    "Department of Health and Social Management" = "Health & Social Mgmt.",
    "Department of Nursing Science" = "Nursing Science",
    "Department of Physics and Mathematics" = "Physics & Math",
    "Department of Social Sciences" = "Social Sciences",
    "Department of Technical Physics" = "Technical Physics",
    "Faculty of Health Sciences, shared activities" = "Health Sci. (shared)",
    "Faculty of Social Sciences and Business Studies, shared activities" = "Soc. Sci. & Bus. (shared)",
    "Human Resources Services" = "HR Services",
    "Karelian Institute" = "Karelian Inst.",
    "Lab Animal Centre" = "Lab Animal Centre",
    "Language Centre" = "Language Centre",
    "Law School" = "Law",
    "Library" = "Library",
    "Public Health and Clinical Nutrition" = "Public Health & Nutrition",
    "School of Applied Educational Science and Teacher Education" = "Applied Educ. & Teacher Ed.",
    "School of Computing" = "Computing",
    "School of Educational Sciences and Psychology" = "Educ. Sci. & Psychology",
    "School of Forest Sciences" = "Forest Sciences",
    "School of Humanities" = "Humanities",
    "School of Medicine" = "Medicine",
    "School of Pharmacy" = "Pharmacy",
    "School of Theology" = "Theology",
    "Student and Learning Services" = "Student Services",
    "Tulliportti School, basic education" = "Tulliportti School",
    "University of Eastern Finland" = "UEF",
    "University Teacher Training School, shared activities" = "Teacher Training (shared)"
  )

  # Add custom abbreviations (override if same name exists)
  if (!is.null(custom)) {
    if (!is.character(custom) || is.null(names(custom))) {
      stop("'custom' must be a named character vector")
    }
    abbreviations[names(custom)] <- custom
  }

  return(abbreviations)
}

#' Abbreviate Unit Names
#'
#' @description
#' Converts full unit names to their abbreviated forms for use in plots
#' and tables. Uses the built-in UEF abbreviation mapping and optionally
#' custom abbreviations.
#'
#' @param units Character vector of unit names to abbreviate.
#' @param custom Named character vector of custom abbreviations.
#'   See \code{\link{get_unit_abbreviations}} for format.
#' @param keep_unknown Logical. If TRUE (default), units not found in the
#'   abbreviation mapping are kept as-is. If FALSE, they are set to NA.
#'
#' @return Character vector of abbreviated unit names.
#'
#' @examples
#' units <- c("School of Computing", "Business School", "Unknown Unit")
#'
#' # Abbreviate with defaults
#' abbreviate_units(units)
#' # Returns: c("Computing", "Business", "Unknown Unit")
#'
#' # Don't keep unknown units
#' abbreviate_units(units, keep_unknown = FALSE)
#' # Returns: c("Computing", "Business", NA)
#'
#' @export
abbreviate_units <- function(units, custom = NULL, keep_unknown = TRUE) {

  if (!is.character(units)) {
    units <- as.character(units)
  }

  abbreviations <- get_unit_abbreviations(custom = custom)

  # Map units to abbreviations
  abbreviated <- abbreviations[units]

  # Handle unknown units
  if (keep_unknown) {
    # Replace NAs with original values
    abbreviated <- ifelse(is.na(abbreviated), units, abbreviated)
  }

  # Remove names from result
  names(abbreviated) <- NULL

  return(abbreviated)
}

#' Add Abbreviated Unit Column to Data
#'
#' @description
#' Adds a new column with abbreviated unit names to a data frame.
#' Useful for preparing data for visualization.
#'
#' @param data A data frame containing a unit name column.
#' @param unit_col Character string. Name of the column containing unit names.
#'   Default is \code{"first_authors_unit"}.
#' @param new_col Character string. Name for the new abbreviated column.
#'   Default is \code{"unit_abbrev"}.
#' @param custom Named character vector of custom abbreviations.
#' @param keep_unknown Logical. If TRUE (default), keeps unrecognized units as-is.
#'
#' @return The input data frame with an additional column containing
#'   abbreviated unit names.
#'
#' @examples
#' \dontrun{
#' # Add abbreviated unit column
#' data <- add_unit_column(cleaned_data)
#'
#' # Use custom column names
#' data <- add_unit_column(
#'   cleaned_data,
#'   unit_col = "department",
#'   new_col = "dept_short"
#' )
#' }
#'
#' @export
add_unit_column <- function(data,
                            unit_col = "first_authors_unit",
                            new_col = "unit_abbrev",
                            custom = NULL,
                            keep_unknown = TRUE) {

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (!(unit_col %in% names(data))) {
    stop("Column '", unit_col, "' not found in data")
  }

  # Add the abbreviated column
  data[[new_col]] <- abbreviate_units(
    data[[unit_col]],
    custom = custom,
    keep_unknown = keep_unknown
  )

  return(data)
}
