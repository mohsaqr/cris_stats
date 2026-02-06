#' Simplify Author String
#'
#' @description
#' Converts author names to a standardized uppercase format, removing
#' accents and special characters. This is a replacement for the
#' \code{upstartr::simplify_string()} function to avoid the external dependency.
#'
#' @param x Character vector of author names to simplify.
#' @param utf8_only Logical. If TRUE, converts to ASCII-compatible characters.
#'   Default is TRUE.
#'
#' @return Character vector with simplified author names in uppercase.
#'
#' @examples
#' simplify_author_string("Müller, Hans")
#' simplify_author_string("José García")
#' simplify_author_string(c("Björk, Anna", "O'Connor, John"))
#'
#' @export
simplify_author_string <- function(x, utf8_only = TRUE) {
  if (!is.character(x)) {
    x <- as.character(x)
  }

  # Convert to uppercase

result <- toupper(x)

  if (utf8_only) {
    # Remove accents by converting to ASCII
    # Use iconv to transliterate accented characters
    result <- iconv(result, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")

    # Remove transliteration artifacts: iconv turns accents into
    # apostrophes/backticks/tildes/carets (e.g. ó -> 'O, ü -> "U)
    # Strip these before the final character filter
    result <- gsub("['\"`~^]", "", result)

    # Keep only letters, digits, spaces, commas, periods, hyphens, parentheses
    result <- gsub("[^A-Z0-9 ,.()-]", "", result)

    # Clean up multiple spaces
    result <- gsub("\\s+", " ", result)

    # Trim whitespace
    result <- trimws(result)
  }

  return(result)
}

#' Validate CRIS Data Input
#'
#' @description
#' Internal function to validate that input data contains required columns
#' for CRIS analysis functions.
#'
#' @param data A data frame to validate.
#' @param required_cols Character vector of required column names.
#' @param func_name Name of the calling function for error messages.
#'
#' @return Invisible TRUE if validation passes; stops with error otherwise.
#'
#' @keywords internal
validate_cris_data <- function(data, required_cols, func_name = "function") {
  if (!is.data.frame(data)) {
    stop(paste0(func_name, ": Input 'data' must be a data frame."))
  }

  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(paste0(
      func_name, ": Input data is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    ))
  }

  invisible(TRUE)
}

#' Convert Columns to Numeric
#'
#' @description
#' Internal helper to convert specified columns to numeric type.
#'
#' @param data A data frame.
#' @param cols Character vector of column names to convert.
#'
#' @return Data frame with specified columns converted to numeric.
#'
#' @keywords internal
convert_to_numeric <- function(data, cols) {
  cols_present <- intersect(cols, names(data))

  for (col in cols_present) {
    if (is.character(data[[col]])) {
      data[[col]] <- as.numeric(data[[col]])
    }
  }

  return(data)
}


#' Compute Gini Coefficient
#'
#' @description
#' Computes the Gini coefficient for a numeric vector, measuring inequality
#' in the distribution of values (e.g., publications across units).
#'
#' @param x Numeric vector of non-negative values.
#'
#' @return A single numeric value between 0 (perfect equality) and 1
#'   (perfect inequality).
#'
#' @keywords internal
compute_gini <- function(x) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n == 0) return(NA_real_)
  if (n == 1) return(0)
  numerator <- 2 * sum(seq_along(x) * x) - (n + 1) * sum(x)
  denominator <- n * sum(x)
  if (denominator == 0) return(0)
  numerator / denominator
}


#' Build Stats GT Table
#'
#' @description
#' Shared gt formatter for faculty/department/unit comparison tables.
#' Formats the output from \code{compare_*} functions consistently.
#'
#' @param table_data Data frame from a \code{compare_*} function's \code{table}
#'   element.
#' @param title Character. Table title.
#' @param subtitle Character. Table subtitle. Default is NULL.
#' @param level_col Character. Name of the grouping column (e.g., "faculty",
#'   "department", "first_authors_unit").
#'
#' @return A \code{gt} table object.
#'
#' @keywords internal
build_stats_gt <- function(table_data, title, subtitle = NULL, level_col) {
  labels_list <- list()
  labels_list[[level_col]] <- "Name"
  labels_list[["publications"]] <- "Publications"
  if ("level_1" %in% names(table_data)) labels_list[["level_1"]] <- "L1"
  if ("level_2" %in% names(table_data)) labels_list[["level_2"]] <- "L2"
  if ("level_3" %in% names(table_data)) labels_list[["level_3"]] <- "L3"
  if ("level_2_3" %in% names(table_data)) labels_list[["level_2_3"]] <- "L2+3"
  if ("pct_high_impact" %in% names(table_data)) {
    labels_list[["pct_high_impact"]] <- "% High"
  }

  gt_tbl <- table_data %>%
    gt::gt() %>%
    gt::tab_header(title = title, subtitle = subtitle) %>%
    gt::fmt_number(
      columns = dplyr::any_of(c("publications", "level_1", "level_2",
                                 "level_3", "level_2_3")),
      decimals = 0
    ) %>%
    gt::fmt_number(
      columns = dplyr::any_of("pct_high_impact"),
      decimals = 1
    ) %>%
    gt::cols_label(.list = labels_list) %>%
    gt::cols_align(align = "center")

  gt_tbl
}


#' Detect Venue/Journal Column
#'
#' @description
#' Detects a journal or venue column from the data by searching for common
#' column name patterns (case-insensitive).
#'
#' @param col_names Character vector of column names to search.
#'
#' @return The matching original column name, or \code{NULL} if none found.
#'
#' @keywords internal
detect_venue_column <- function(col_names) {
  candidates <- c("title_of_journal_series", "journal", "venue",
                   "publication_channel", "publication_series")
  lower_names <- tolower(col_names)
  for (cand in candidates) {
    idx <- which(lower_names == cand)
    if (length(idx) > 0) return(col_names[idx[1]])
  }
  NULL
}


#' Build Top Result
#'
#' @description
#' Shared builder for \code{top_*} ranking functions. Takes a ranked data
#' frame, builds a gt table and horizontal bar plot.
#'
#' @param ranked_df Data frame with rankings, must contain a \code{publications}
#'   column.
#' @param name_col Character. The column name to use for labels.
#' @param title Character. Title for the gt table and plot.
#' @param n Integer. Number of top items (used in subtitle).
#'
#' @return A list with \code{table}, \code{gt_table}, and \code{plot}.
#'
#' @keywords internal
build_top_result <- function(ranked_df, name_col, title, n) {
  # GT table
  gt_tbl <- ranked_df %>%
    gt::gt() %>%
    gt::tab_header(
      title = title,
      subtitle = paste("Top", nrow(ranked_df))
    ) %>%
    gt::fmt_number(
      columns = dplyr::any_of(c("publications", "departments")),
      decimals = 0
    ) %>%
    gt::fmt_number(
      columns = dplyr::any_of(c("pct_high_impact", "mean_jufo_level")),
      decimals = 1
    ) %>%
    gt::cols_align(align = "center")

  # Bar plot
  plot_df <- ranked_df
  plot_df[["label"]] <- plot_df[[name_col]]
  plot_df[["label"]] <- factor(plot_df[["label"]],
                                levels = rev(plot_df[["label"]]))

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$label, y = .data$publications)
  ) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = title, x = NULL, y = "Publications") +
    ggplot2::theme_minimal()

  list(
    table = ranked_df,
    gt_table = gt_tbl,
    plot = p
  )
}
