#' Clean Author Data
#'
#' @description
#' Cleans and standardizes author names from CRIS publication data. Splits
#' the authors field into separate rows, removes common artifacts like "Jr.",
#' year patterns, and standardizes names to uppercase for consistent matching.
#'
#' @param data A data frame containing publication data with an \code{authors}
#'   column.
#' @param authors_col Character string. Name of the column containing author
#'   names separated by semicolons. Default is \code{"authors"}.
#' @param first_author_col Character string. Name of the column containing the
#'   first author name. Default is \code{"first_author"}.
#' @param exclude_et_al Logical. If TRUE, removes rows where cleaned author
#'   name is "ET AL". Default is TRUE.
#'
#' @return A data frame with one row per author per publication, containing
#'   the original columns plus:
#'   \describe{
#'     \item{Authorsclean}{Cleaned and standardized author name in uppercase}
#'   }
#'
#' @details
#' The cleaning process includes:
#' \enumerate{
#'   \item Splitting semicolon-separated author names into separate rows
#'   \item Removing " Jr." suffixes
#'   \item Removing leading year patterns like "(2024)."
#'   \item Removing leading commas and spaces
#'   \item Converting to uppercase ASCII characters for consistent matching
#'   \item Optionally filtering out "ET AL" entries
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' cleaned_data <- clean_author_data(publications)
#'
#' # Keep "ET AL" entries
#' cleaned_data <- clean_author_data(publications, exclude_et_al = FALSE)
#'
#' # Use different column names
#' cleaned_data <- clean_author_data(
#'   publications,
#'   authors_col = "author_list",
#'   first_author_col = "lead_author"
#' )
#' }
#'
#' @export
clean_author_data <- function(data,
                              authors_col = "authors",
                              first_author_col = "first_author",
                              exclude_et_al = TRUE) {
  # Validate input
  validate_cris_data(
    data,
    required_cols = authors_col,
    func_name = "clean_author_data"
  )

  # Split the authors field into separate rows
  authorseparated <- splitstackshape::cSplit(
    data,
    authors_col,
    sep = ";",
    direction = "long"
  )

  # Convert to data frame if data.table
  authorseparated <- as.data.frame(authorseparated)

  # Clean the authors field
  authorseparated <- authorseparated %>%
    dplyr::mutate(
      # Remove " Jr." suffix
      Authorsclean = stringr::str_remove(.data[[authors_col]], " Jr\\."),
      # Remove leading year pattern like "(2024)." or "(2024) "
      Authorsclean = stringr::str_remove(.data$Authorsclean, "^\\(\\d{4}\\)(\\.)?( )?"),
      # Remove leading comma and space
      Authorsclean = stringr::str_remove(.data$Authorsclean, "^, "),
      # Handle NA cases
      Authorsclean = ifelse(is.na(.data$Authorsclean), .data[[authors_col]], .data$Authorsclean)
    )

  # Simplify the string for better matching (uppercase, remove accents)
  authorseparated$Authorsclean <- simplify_author_string(
    authorseparated$Authorsclean,
    utf8_only = TRUE
  )

  # Filter out "ET AL" entries if requested
  if (exclude_et_al) {
    authorseparated <- authorseparated %>%
      dplyr::filter(.data$Authorsclean != "ET AL")
  }

  # Clean the first_author field if it exists
  if (first_author_col %in% names(authorseparated)) {
    authorseparated <- authorseparated %>%
      dplyr::mutate(
        # Remove " Jr." suffix
        !!first_author_col := stringr::str_remove(.data[[first_author_col]], " Jr\\."),
        # Remove leading year pattern
        !!first_author_col := stringr::str_remove(.data[[first_author_col]], "^\\(\\d{4}\\)(\\.)?( )?"),
        # Remove leading comma and space
        !!first_author_col := stringr::str_remove(.data[[first_author_col]], "^, ")
      )

    # Simplify first author names
    authorseparated[[first_author_col]] <- simplify_author_string(
      authorseparated[[first_author_col]],
      utf8_only = TRUE
    )

    # Filter out "ET AL" in first author if excluding
    if (exclude_et_al) {
      authorseparated <- authorseparated %>%
        dplyr::filter(.data[[first_author_col]] != "ET AL")
    }
  }

  return(authorseparated)
}
