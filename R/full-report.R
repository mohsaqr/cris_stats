#' Generate Full Report
#'
#' @description
#' Generates a comprehensive analysis report with all comparisons at faculty,
#' department, unit, and author levels. This is the main entry point for
#' complete publication analysis.
#'
#' @param data A data frame from \code{\link{prepare_cris_data}}.
#' @param years Numeric vector. Years to include. If NULL, uses all years in data.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#' @param top_n Integer. Number of top items to include in author rankings.
#'   Default is 50.
#'
#' @return A list containing comprehensive analysis results:
#'   \describe{
#'     \item{$by_faculty}{Faculty-level comparison with table, summary, and plot}
#'     \item{$by_department}{Department-level comparison with table, summary, and plot}
#'     \item{$by_unit}{Unit-level comparison with table, summary, and plot}
#'     \item{$by_author}{Author productivity with table, gt_table, and summary}
#'     \item{$trends}{Trend data and plots for each level}
#'     \item{$plots}{All visualization plots in one list}
#'     \item{$summary}{Overall summary statistics}
#'     \item{$data}{The filtered data used for analysis}
#'     \item{$parameters}{The parameters used for this report}
#'   }
#'
#' @details
#' This function orchestrates all comparison and visualization functions to
#' provide a complete analysis in one call. It's designed for the workflow:
#'
#' \preformatted{
#' data <- prepare_cris_data("Data/")
#' report <- generate_full_report(data, years = 2020:2025)
#'
#' # Access results
#' report$by_faculty$table
#' report$by_faculty$plot
#' report$by_department$table
#' report$by_author$gt_table
#' report$plots$faculty_trends
#' }
#'
#' @examples
#' \dontrun{
#' # Full analysis workflow
#' data <- prepare_cris_data("Data/", years = 2020:2025, jufo = 1:3)
#' report <- generate_full_report(data)
#'
#' # View faculty comparison
#' report$by_faculty$table
#' report$by_faculty$plot
#'
#' # View department comparison
#' report$by_department$table
#'
#' # View top authors
#' report$by_author$gt_table
#'
#' # View trends
#' report$trends$faculty
#'
#' # Get all plots
#' report$plots$faculty_comparison
#' report$plots$faculty_trends
#' report$plots$jufo_by_year
#' }
#'
#' @seealso
#' \code{\link{prepare_cris_data}} for data preparation,
#' \code{\link{compare_faculties}}, \code{\link{compare_departments}},
#' \code{\link{compare_units}}, \code{\link{compare_authors}} for individual comparisons,
#' \code{\link{plot_comparison}}, \code{\link{plot_trends}} for visualizations
#'
#' @export
generate_full_report <- function(data,
                                 years = NULL,
                                 jufo = 1:3,
                                 top_n = 50) {

  # Validate input
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  message("Generating comprehensive report...")

  # Determine years range from data if not specified
  if (is.null(years) && "publication_year" %in% names(data)) {
    years <- sort(unique(data$publication_year))
    message("Using all years in data: ", min(years), "-", max(years))
  }

  # Filter data for the report
  report_data <- filter_by_criteria(data, years = years, jufo = jufo)
  message("Analyzing ", dplyr::n_distinct(report_data$publication_id), " publications")

  # Initialize result list
  result <- list(
    by_faculty = NULL,
    by_department = NULL,
    by_unit = NULL,
    by_author = NULL,
    trends = list(),
    plots = list(),
    summary = NULL,
    data = report_data,
    parameters = list(
      years = years,
      jufo = jufo,
      top_n = top_n,
      generated_at = Sys.time()
    )
  )

  # Faculty comparison
  if ("faculty" %in% names(data)) {
    message("  - Comparing faculties...")
    result$by_faculty <- compare_faculties(data, years = years, jufo = jufo)
    result$by_faculty$plot <- plot_comparison(data, level = "faculty",
                                               years = years, jufo = jufo)
    result$trends$faculty <- plot_trends(data, level = "faculty",
                                          years = years, jufo = jufo)
    result$plots$faculty_comparison <- result$by_faculty$plot
    result$plots$faculty_trends <- result$trends$faculty
  }

  # Department comparison
  if ("department" %in% names(data)) {
    message("  - Comparing departments...")
    result$by_department <- compare_departments(data, years = years, jufo = jufo)
    result$by_department$plot <- plot_comparison(data, level = "department",
                                                   years = years, jufo = jufo,
                                                   fill_by = "faculty")
    result$trends$department <- plot_trends(data, level = "department",
                                             years = years, jufo = jufo)
    result$plots$department_comparison <- result$by_department$plot
    result$plots$department_trends <- result$trends$department
  }

  # Unit comparison
  if ("first_authors_unit" %in% names(data)) {
    message("  - Comparing units...")
    result$by_unit <- compare_units(data, years = years, jufo = jufo)
    result$by_unit$plot <- plot_comparison(data, level = "unit",
                                            years = years, jufo = jufo)
    result$trends$unit <- plot_trends(data, level = "unit",
                                       years = years, jufo = jufo)
    result$plots$unit_comparison <- result$by_unit$plot
    result$plots$unit_trends <- result$trends$unit
  }

  # Author comparison
  message("  - Comparing authors...")
  result$by_author <- compare_authors(data, top_n = top_n,
                                       years = years, jufo = jufo)

  # JUFO distribution plots
  if ("publication_year" %in% names(data)) {
    message("  - Creating JUFO distribution plots...")
    result$plots$jufo_by_year <- plot_jufo_distribution(data, by = "year",
                                                         years = years)
  }

  if ("faculty" %in% names(data)) {
    result$plots$jufo_by_faculty <- plot_jufo_distribution(data, by = "faculty",
                                                            years = years,
                                                            proportional = TRUE)
  }

  # Calculate overall summary
  message("  - Calculating summary statistics...")
  result$summary <- calculate_report_summary(report_data)

  message("Report generation complete!")
  class(result) <- c("cris_report", class(result))

  return(result)
}

#' Calculate Report Summary Statistics
#'
#' @description
#' Internal function to calculate overall summary statistics for a report.
#'
#' @param data Filtered data frame.
#'
#' @return List of summary statistics.
#'
#' @keywords internal
calculate_report_summary <- function(data) {
  summary <- list(
    total_publications = dplyr::n_distinct(data$publication_id),
    year_range = if ("publication_year" %in% names(data)) {
      range(data$publication_year, na.rm = TRUE)
    } else {
      NA
    }
  )

  # JUFO level breakdown
  if ("jufo_level_of_publication" %in% names(data)) {
    jufo_counts <- data %>%
      dplyr::distinct(.data$publication_id, .keep_all = TRUE) %>%
      dplyr::count(.data$jufo_level_of_publication) %>%
      dplyr::arrange(.data$jufo_level_of_publication)

    summary$jufo_breakdown <- jufo_counts
    summary$level_2_3_count <- sum(
      jufo_counts$n[jufo_counts$jufo_level_of_publication %in% c("2", "3")]
    )
    summary$level_2_3_pct <- round(
      100 * summary$level_2_3_count / summary$total_publications, 1
    )
  }

  # Faculty count
  if ("faculty" %in% names(data)) {
    summary$n_faculties <- dplyr::n_distinct(data$faculty, na.rm = TRUE)
  }

  # Department count
  if ("department" %in% names(data)) {
    summary$n_departments <- dplyr::n_distinct(data$department, na.rm = TRUE)
  }

  # Unit count
  if ("first_authors_unit" %in% names(data)) {
    summary$n_units <- dplyr::n_distinct(data$first_authors_unit, na.rm = TRUE)
  }

  # Author count
  author_col <- if ("Authorsclean" %in% names(data)) "Authorsclean" else "authors"
  if (author_col %in% names(data)) {
    summary$n_authors <- dplyr::n_distinct(data[[author_col]], na.rm = TRUE)
  }

  summary
}

#' Print CRIS Report
#'
#' @description
#' Print method for cris_report objects.
#'
#' @param x A cris_report object.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisible x.
#'
#' @export
print.cris_report <- function(x, ...) {
  cat("\n")
  cat("CRIS Publication Analysis Report\n")
  cat("=================================\n\n")

  # Parameters
  if (!is.null(x$parameters$years)) {
    cat("Period: ", min(x$parameters$years), "-", max(x$parameters$years), "\n")
  }
  cat("JUFO levels: ", paste(x$parameters$jufo, collapse = ", "), "\n")
  cat("Generated: ", format(x$parameters$generated_at, "%Y-%m-%d %H:%M"), "\n\n")

  # Summary
  cat("Summary Statistics:\n")
  cat("-------------------\n")
  cat("Total publications: ", x$summary$total_publications, "\n")

  if (!is.null(x$summary$level_2_3_pct)) {
    cat("High-impact (L2+L3): ", x$summary$level_2_3_count,
        " (", x$summary$level_2_3_pct, "%)\n")
  }

  if (!is.null(x$summary$n_faculties)) {
    cat("Faculties: ", x$summary$n_faculties, "\n")
  }
  if (!is.null(x$summary$n_departments)) {
    cat("Departments: ", x$summary$n_departments, "\n")
  }
  if (!is.null(x$summary$n_units)) {
    cat("Units: ", x$summary$n_units, "\n")
  }
  if (!is.null(x$summary$n_authors)) {
    cat("Authors: ", x$summary$n_authors, "\n")
  }

  cat("\nAvailable components:\n")
  cat("---------------------\n")
  cat("$by_faculty     - Faculty-level analysis\n")
  cat("$by_department  - Department-level analysis\n")
  cat("$by_unit        - Unit-level analysis\n")
  cat("$by_author      - Author productivity\n")
  cat("$trends         - Trend plots by level\n")
  cat("$plots          - All visualization plots\n")
  cat("$summary        - Summary statistics\n")
  cat("$data           - Filtered data used\n")
  cat("$parameters     - Report parameters\n")
  cat("\n")

  invisible(x)
}

#' Quick Summary of CRIS Data
#'
#' @description
#' Generates a quick summary of prepared CRIS data without the full report.
#'
#' @param data A data frame from \code{\link{prepare_cris_data}}.
#'
#' @return A list with summary statistics, printed to console.
#'
#' @examples
#' \dontrun{
#' data <- prepare_cris_data("Data/")
#' quick_summary(data)
#' }
#'
#' @export
quick_summary <- function(data) {

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  summary <- calculate_report_summary(data)

  cat("\n")
  cat("CRIS Data Quick Summary\n")
  cat("=======================\n\n")

  cat("Total publications: ", summary$total_publications, "\n")

  if (!is.null(summary$year_range) && !any(is.na(summary$year_range))) {
    cat("Year range: ", summary$year_range[1], "-", summary$year_range[2], "\n")
  }

  if (!is.null(summary$jufo_breakdown)) {
    cat("\nJUFO Level Distribution:\n")
    for (i in seq_len(nrow(summary$jufo_breakdown))) {
      cat("  Level ", summary$jufo_breakdown$jufo_level_of_publication[i],
          ": ", summary$jufo_breakdown$n[i], "\n")
    }
    if (!is.null(summary$level_2_3_pct)) {
      cat("  High-impact (L2+L3): ", summary$level_2_3_pct, "%\n")
    }
  }

  cat("\nOrganizational Coverage:\n")
  if (!is.null(summary$n_faculties)) {
    cat("  Faculties: ", summary$n_faculties, "\n")
  }
  if (!is.null(summary$n_departments)) {
    cat("  Departments: ", summary$n_departments, "\n")
  }
  if (!is.null(summary$n_units)) {
    cat("  Units: ", summary$n_units, "\n")
  }
  if (!is.null(summary$n_authors)) {
    cat("  Authors: ", summary$n_authors, "\n")
  }
  cat("\n")

  invisible(summary)
}
