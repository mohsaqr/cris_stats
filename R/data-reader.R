#' Read CRIS Data from Excel Files
#'
#' @description
#' Reads one or more Excel files exported from a CRIS (Current Research
#' Information System) and combines them into a single clean data frame.
#' Automatically cleans column names and removes empty columns.
#'
#' @param path Character string. Either a path to a single Excel file or
#'   a path to a folder containing Excel files.
#' @param pattern Character string. Glob pattern for matching files when
#'   \code{path} is a directory. Default is \code{"*.xlsx"}.
#' @param remove_duplicates Logical. If TRUE, removes duplicate rows. When
#'   \code{publication_id} column exists, deduplication is based on that column;
#'   otherwise uses all columns. Default is TRUE.
#'
#' @return A data frame with cleaned column names containing the combined
#'   publication data.
#'
#' @details
#' The function performs the following operations:
#' \enumerate{
#'   \item Reads all matching Excel files
#'   \item Cleans column names using \code{janitor::clean_names()}
#'   \item Combines all data frames row-wise
#'   \item Removes empty columns
#'   \item Optionally removes duplicate rows (by \code{publication_id} if available)
#' }
#'
#' @examples
#' \dontrun{
#' # Read a single file
#' data <- read_cris_data("publications.xlsx")
#'
#' # Read all Excel files from a folder
#' data <- read_cris_data("Data/")
#'
#' # Read specific files matching a pattern
#' data <- read_cris_data("Data/", pattern = "Publications_en_*.xlsx")
#' }
#'
#' @export
read_cris_data <- function(path, pattern = "*.xlsx", remove_duplicates = TRUE) {
  # Check if path exists
  if (!file.exists(path) && !dir.exists(path)) {
    stop("Path does not exist: ", path)
  }

  # Determine if path is a file or directory
  if (file.info(path)$isdir) {
    # Get all matching files in the directory
    files <- list.files(
      path,
      pattern = utils::glob2rx(pattern),
      full.names = TRUE
    )

    if (length(files) == 0) {
      stop("No files matching pattern '", pattern, "' found in: ", path)
    }

    message("Reading ", length(files), " Excel file(s)...")
  } else {
    # Single file
    files <- path
  }

  # Read and combine all files
  data <- purrr::map_dfr(files, function(f) {
    tryCatch(
      {
        readxl::read_xlsx(f) %>%
          janitor::clean_names()
      },
      error = function(e) {
        warning("Error reading file ", f, ": ", e$message)
        NULL
      }
    )
  })

  if (nrow(data) == 0) {
    stop("No data could be read from the specified files.")
  }

  # Remove empty columns
  data <- datawizard::remove_empty_columns(data)

  # Remove duplicates if requested
  if (remove_duplicates) {
    n_before <- nrow(data)

    # Dedupe by publication_id if available, otherwise by all columns
    if ("publication_id" %in% names(data)) {
      data <- dplyr::distinct(data, .data$publication_id, .keep_all = TRUE)
      dedup_method <- "publication_id"
    } else {
      data <- dplyr::distinct(data)
      dedup_method <- "all columns"
    }

    n_removed <- n_before - nrow(data)
    if (n_removed > 0) {
      message("Removed ", n_removed, " duplicate rows (by ", dedup_method, ").")
    }
  }

  message("Read ", nrow(data), " publications from ", length(files), " file(s).")

  return(data)
}
