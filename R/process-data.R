#' Process CRIS Data
#'
#' @description
#' Convenience wrapper around \code{\link{prepare_cris_data}} that reads,
#' deduplicates, cleans author names, and adds organizational hierarchy.
#' Identical behavior to \code{prepare_cris_data()} under a shorter name.
#'
#' @inheritParams prepare_cris_data
#'
#' @return A data frame with cleaned and enriched publication data.
#'   See \code{\link{prepare_cris_data}} for full details on columns returned.
#'
#' @examples
#' \dontrun{
#' # Basic usage - read and prepare all data
#' data <- process_data("Data/")
#'
#' # Filter to specific years and JUFO levels
#' data <- process_data("Data/", years = 2020:2025, jufo = 1:3)
#'
#' # Include all JUFO levels including 0
#' data <- process_data("Data/", jufo = 0:3)
#'
#' # Skip author cleaning for faster processing
#' data <- process_data("Data/", clean_authors = FALSE)
#' }
#'
#' @seealso
#' \code{\link{prepare_cris_data}},
#' \code{\link{university_stats}} for aggregate statistics
#'
#' @export
process_data <- function(path,
                         years = NULL,
                         jufo = 1:3,
                         pattern = "*.xlsx",
                         clean_authors = TRUE,
                         add_hierarchy = TRUE) {
  prepare_cris_data(path, years, jufo, pattern, clean_authors, add_hierarchy)
}


#' University-Wide Publication Statistics
#'
#' @description
#' One-stop function for university-wide publication analysis. Computes yearly
#' aggregate statistics, JUFO distributions, and breakdowns by faculty,
#' department, unit, and top authors. Includes formatted tables and plots.
#'
#' @param data A data frame from \code{\link{prepare_cris_data}} or
#'   \code{\link{process_data}}, containing at minimum \code{publication_id},
#'   \code{publication_year}, and \code{jufo_level_of_publication}.
#' @param years Numeric vector. Years to include. If NULL (default), uses all
#'   years in the data.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#' @param top_n Integer. Number of top items to return in author and unit
#'   rankings. Default is 20.
#'
#' @return A list containing:
#'   \describe{
#'     \item{summary_table}{Data frame with one row per year: total publications,
#'       JUFO level counts, percentage high-impact (levels 2+3), average authors,
#'       and type breakdown columns if detected.}
#'     \item{by_jufo}{Publication counts grouped by year and JUFO level.}
#'     \item{by_type}{Publication counts grouped by year and type, or \code{NULL}.}
#'     \item{by_faculty}{Faculty-level comparison from \code{\link{compare_faculties}},
#'       or \code{NULL} if no faculty column.}
#'     \item{by_department}{Department-level comparison from
#'       \code{\link{compare_departments}}, or \code{NULL}.}
#'     \item{by_unit}{Unit-level comparison from \code{\link{compare_units}},
#'       or \code{NULL}.}
#'     \item{top_authors}{Author ranking from \code{\link{compare_authors}},
#'       or \code{NULL}.}
#'     \item{gt_table}{Formatted \code{gt} summary table.}
#'     \item{plots}{List of ggplot objects: \code{articles_per_year} and
#'       \code{jufo_trends}.}
#'   }
#'
#' @details
#' Type column detection looks for \code{type_of_publication},
#' \code{publication_type}, \code{type}, or \code{publication_type_code}
#' (case-insensitive).
#'
#' Author counts are computed by counting rows per \code{publication_id} when
#' data is at the author level (post \code{\link{clean_author_data}}), or by
#' counting semicolons in the \code{authors} column otherwise.
#'
#' Faculty, department, unit, and author breakdowns are only included when
#' the relevant columns exist in the data.
#'
#' @examples
#' \dontrun{
#' data <- process_data("Data/")
#' stats <- university_stats(data, years = 2020:2025)
#'
#' # Yearly summary
#' stats$summary_table
#' stats$gt_table
#'
#' # Breakdowns
#' stats$by_faculty$table
#' stats$by_unit$table
#' stats$top_authors$gt_table
#'
#' # Plots
#' stats$plots$articles_per_year
#' stats$plots$jufo_trends
#' }
#'
#' @seealso
#' \code{\link{process_data}}, \code{\link{prepare_cris_data}},
#' \code{\link{generate_full_report}}, \code{\link{author_stats}}
#'
#' @export
university_stats <- function(data, years = NULL, jufo = 1:3, top_n = 20) {

  validate_cris_data(
    data,
    required_cols = c("publication_id", "publication_year",
                      "jufo_level_of_publication"),
    func_name = "university_stats"
  )

  # Apply year/JUFO filters
  filtered <- filter_by_criteria(data, years = years, jufo = jufo)

  # Deduplicate to one row per publication for counting
  pubs <- filtered %>%
    dplyr::distinct(.data$publication_id, .keep_all = TRUE)

  # --- Author counts per publication ---
  is_author_level <- nrow(filtered) > nrow(pubs)
  author_counts <- NULL

  if (is_author_level) {
    author_counts <- filtered %>%
      dplyr::count(.data$publication_id, name = "n_authors")
    author_counts <- author_counts %>%
      dplyr::left_join(
        pubs %>% dplyr::select(dplyr::all_of(c("publication_id",
                                                 "publication_year"))),
        by = "publication_id"
      )
  } else {
    auth_col <- if ("authors" %in% names(pubs)) {
      "authors"
    } else if ("Authorsclean" %in% names(pubs)) {
      "Authorsclean"
    } else {
      NULL
    }
    if (!is.null(auth_col)) {
      author_counts <- pubs %>%
        dplyr::transmute(
          publication_id = .data$publication_id,
          publication_year = .data$publication_year,
          n_authors = stringr::str_count(.data[[auth_col]], ";") + 1L
        )
    }
  }

  # --- JUFO counts by year ---
  by_jufo <- pubs %>%
    dplyr::group_by(
      year = .data$publication_year,
      jufo_level = .data$jufo_level_of_publication
    ) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  # --- Summary table ---
  yearly <- pubs %>%
    dplyr::group_by(.data$publication_year) %>%
    dplyr::summarise(total_publications = dplyr::n(), .groups = "drop")

  jufo_wide <- pubs %>%
    dplyr::group_by(.data$publication_year, .data$jufo_level_of_publication) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = "jufo_level_of_publication",
      values_from = "n",
      names_prefix = "jufo_",
      values_fill = 0
    )

  yearly <- yearly %>%
    dplyr::left_join(jufo_wide, by = "publication_year")

  for (j in 0:3) {
    col <- paste0("jufo_", j)
    if (!(col %in% names(yearly))) yearly[[col]] <- 0L
  }

  yearly <- yearly %>%
    dplyr::mutate(
      pct_high_impact = round(
        100 * (.data$jufo_2 + .data$jufo_3) / .data$total_publications, 1
      )
    )

  if (!is.null(author_counts)) {
    avg_auth <- author_counts %>%
      dplyr::group_by(.data$publication_year) %>%
      dplyr::summarise(
        avg_authors = round(mean(.data$n_authors, na.rm = TRUE), 1),
        .groups = "drop"
      )
    yearly <- yearly %>%
      dplyr::left_join(avg_auth, by = "publication_year")
  }

  # --- Type detection and breakdown ---
  type_col <- detect_type_column(names(pubs))
  by_type <- NULL

  if (!is.null(type_col)) {
    by_type <- pubs %>%
      dplyr::group_by(
        year = .data$publication_year,
        type = .data[[type_col]]
      ) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop")

    type_wide <- by_type %>%
      tidyr::pivot_wider(
        names_from = "type",
        values_from = "count",
        values_fill = 0
      )

    yearly <- yearly %>%
      dplyr::left_join(type_wide, by = c("publication_year" = "year"))
  }

  # Rename and reorder
  names(yearly)[names(yearly) == "publication_year"] <- "year"

  core_cols <- c("year", "total_publications",
                 "jufo_0", "jufo_1", "jufo_2", "jufo_3",
                 "pct_high_impact")
  if ("avg_authors" %in% names(yearly)) {
    core_cols <- c(core_cols, "avg_authors")
  }
  other_cols <- setdiff(names(yearly), core_cols)
  yearly <- yearly[, c(core_cols, other_cols)]

  # --- GT table ---
  gt_tbl <- yearly %>%
    gt::gt() %>%
    gt::tab_header(
      title = "University Publication Statistics",
      subtitle = paste("Years:", min(yearly$year), "-", max(yearly$year))
    ) %>%
    gt::fmt_number(
      columns = c("total_publications", "jufo_0", "jufo_1", "jufo_2", "jufo_3"),
      decimals = 0
    ) %>%
    gt::fmt_number(columns = "pct_high_impact", decimals = 1) %>%
    gt::cols_label(
      year = "Year",
      total_publications = "Total",
      jufo_0 = "JUFO 0",
      jufo_1 = "JUFO 1",
      jufo_2 = "JUFO 2",
      jufo_3 = "JUFO 3",
      pct_high_impact = "% High Impact"
    ) %>%
    gt::cols_align(align = "center")

  if ("avg_authors" %in% names(yearly)) {
    gt_tbl <- gt_tbl %>%
      gt::fmt_number(columns = "avg_authors", decimals = 1) %>%
      gt::cols_label(avg_authors = "Avg Authors")
  }

  # --- Faculty breakdown ---
  by_faculty <- NULL
  if ("faculty" %in% names(data)) {
    by_faculty <- compare_faculties(data, years = years, jufo = jufo)
  }

  # --- Department breakdown ---
  by_department <- NULL
  if ("department" %in% names(data)) {
    by_department <- compare_departments(data, years = years, jufo = jufo)
  }

  # --- Unit breakdown ---
  by_unit <- NULL
  if ("first_authors_unit" %in% names(data)) {
    by_unit <- compare_units(data, years = years, jufo = jufo, top_n = top_n)
  }

  # --- Top authors ---
  top_authors <- NULL
  has_authors <- "Authorsclean" %in% names(data) || "authors" %in% names(data)
  if (has_authors) {
    top_authors <- compare_authors(data, years = years, jufo = jufo,
                                   top_n = top_n)
  }

  # --- Plots ---
  plots <- list()

  plots$articles_per_year <- ggplot2::ggplot(
    yearly,
    ggplot2::aes(x = as.factor(.data$year), y = .data$total_publications)
  ) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::labs(title = "Publications Per Year",
                  x = "Year", y = "Publications") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  plots$jufo_trends <- ggplot2::ggplot(
    by_jufo,
    ggplot2::aes(
      x = .data$year,
      y = .data$count,
      color = as.factor(.data$jufo_level),
      group = .data$jufo_level
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(title = "JUFO Level Distribution Over Time",
                  x = "Year", y = "Publications", color = "JUFO Level") +
    ggplot2::theme_minimal()

  list(
    summary_table = yearly,
    by_jufo = by_jufo,
    by_type = by_type,
    by_faculty = by_faculty,
    by_department = by_department,
    by_unit = by_unit,
    top_authors = top_authors,
    gt_table = gt_tbl,
    plots = plots
  )
}


# ============================================================================
# top_*() ranking functions
# ============================================================================

#' Top Authors by Publication Count
#'
#' @description
#' Returns a ranked table, formatted gt table, and bar plot of the most
#' prolific authors in the data.
#'
#' @param data A data frame from \code{\link{process_data}}.
#' @param n Integer. Number of top authors to return. Default is 20.
#' @param years Numeric vector. Years to include. If NULL, uses all years.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#'
#' @return A list with \code{table} (data frame), \code{gt_table} (gt object),
#'   and \code{plot} (ggplot).
#'
#' @examples
#' \dontrun{
#' data <- process_data("Data/")
#' top_authors(data)$gt_table
#' top_authors(data, n = 10)$plot
#' }
#'
#' @export
top_authors <- function(data, n = 20, years = NULL, jufo = 1:3) {
  filtered <- filter_by_criteria(data, years = years, jufo = jufo)

  auth_col <- if ("Authorsclean" %in% names(filtered)) {
    "Authorsclean"
  } else if ("authors" %in% names(filtered)) {
    "authors"
  } else {
    stop("top_authors: No author column found (Authorsclean or authors).")
  }

  pubs <- filtered %>%
    dplyr::distinct(.data$publication_id, .data[[auth_col]],
                    .data$jufo_level_of_publication)

  ranked <- pubs %>%
    dplyr::group_by(author = .data[[auth_col]]) %>%
    dplyr::summarise(
      publications = dplyr::n_distinct(.data$publication_id),
      pct_high_impact = round(
        100 * sum(.data$jufo_level_of_publication %in% c(2, 3)) /
          dplyr::n(), 1
      ),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$publications)) %>%
    utils::head(n)

  build_top_result(ranked, "author", "Top Authors", n)
}


#' Top Faculties by Publication Count
#'
#' @description
#' Returns a ranked table, formatted gt table, and bar plot of faculties
#' by publication count.
#'
#' @param data A data frame from \code{\link{process_data}} with a
#'   \code{faculty} column.
#' @param n Integer. Number of top faculties to return. Default is 20.
#' @param years Numeric vector. Years to include. If NULL, uses all years.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#'
#' @return A list with \code{table}, \code{gt_table}, and \code{plot}.
#'
#' @examples
#' \dontrun{
#' data <- process_data("Data/")
#' top_faculties(data)$plot
#' }
#'
#' @export
top_faculties <- function(data, n = 20, years = NULL, jufo = 1:3) {
  validate_cris_data(data, required_cols = "faculty",
                     func_name = "top_faculties")
  filtered <- filter_by_criteria(data, years = years, jufo = jufo)
  pubs <- filtered %>%
    dplyr::distinct(.data$publication_id, .keep_all = TRUE)

  ranked <- pubs %>%
    dplyr::group_by(faculty = .data$faculty) %>%
    dplyr::summarise(
      publications = dplyr::n(),
      departments = if ("department" %in% names(pubs)) {
        dplyr::n_distinct(.data$department)
      } else {
        NA_integer_
      },
      pct_high_impact = round(
        100 * sum(.data$jufo_level_of_publication %in% c(2, 3)) /
          dplyr::n(), 1
      ),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$publications)) %>%
    utils::head(n)

  if (all(is.na(ranked$departments))) ranked$departments <- NULL

  build_top_result(ranked, "faculty", "Top Faculties", n)
}


#' Top Departments by Publication Count
#'
#' @description
#' Returns a ranked table, formatted gt table, and bar plot of departments
#' by publication count.
#'
#' @param data A data frame from \code{\link{process_data}} with a
#'   \code{department} column.
#' @param n Integer. Number of top departments to return. Default is 20.
#' @param years Numeric vector. Years to include. If NULL, uses all years.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#'
#' @return A list with \code{table}, \code{gt_table}, and \code{plot}.
#'
#' @examples
#' \dontrun{
#' data <- process_data("Data/")
#' top_departments(data, n = 10)$table
#' }
#'
#' @export
top_departments <- function(data, n = 20, years = NULL, jufo = 1:3) {
  validate_cris_data(data, required_cols = "department",
                     func_name = "top_departments")
  filtered <- filter_by_criteria(data, years = years, jufo = jufo)
  pubs <- filtered %>%
    dplyr::distinct(.data$publication_id, .keep_all = TRUE)

  ranked <- pubs %>%
    dplyr::group_by(department = .data$department) %>%
    dplyr::summarise(
      faculty = if ("faculty" %in% names(pubs)) {
        dplyr::first(stats::na.omit(.data$faculty))
      } else {
        NA_character_
      },
      publications = dplyr::n(),
      pct_high_impact = round(
        100 * sum(.data$jufo_level_of_publication %in% c(2, 3)) /
          dplyr::n(), 1
      ),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$publications)) %>%
    utils::head(n)

  if (all(is.na(ranked$faculty))) ranked$faculty <- NULL

  build_top_result(ranked, "department", "Top Departments", n)
}


#' Top Units by Publication Count
#'
#' @description
#' Returns a ranked table, formatted gt table, and bar plot of organizational
#' units by publication count.
#'
#' @param data A data frame from \code{\link{process_data}} with a
#'   \code{first_authors_unit} column.
#' @param n Integer. Number of top units to return. Default is 20.
#' @param years Numeric vector. Years to include. If NULL, uses all years.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#'
#' @return A list with \code{table}, \code{gt_table}, and \code{plot}.
#'
#' @examples
#' \dontrun{
#' data <- process_data("Data/")
#' top_units(data)$gt_table
#' }
#'
#' @export
top_units <- function(data, n = 20, years = NULL, jufo = 1:3) {
  validate_cris_data(data, required_cols = "first_authors_unit",
                     func_name = "top_units")
  filtered <- filter_by_criteria(data, years = years, jufo = jufo)
  pubs <- filtered %>%
    dplyr::distinct(.data$publication_id, .keep_all = TRUE)

  ranked <- pubs %>%
    dplyr::group_by(unit = .data$first_authors_unit) %>%
    dplyr::summarise(
      publications = dplyr::n(),
      pct_high_impact = round(
        100 * sum(.data$jufo_level_of_publication %in% c(2, 3)) /
          dplyr::n(), 1
      ),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$publications)) %>%
    utils::head(n)

  # Use abbreviations for plot labels if available
  if ("unit_abbrev" %in% names(pubs)) {
    abbrev_map <- pubs %>%
      dplyr::distinct(.data$first_authors_unit, .data$unit_abbrev) %>%
      dplyr::filter(!is.na(.data$unit_abbrev))
    ranked <- ranked %>%
      dplyr::left_join(abbrev_map,
                       by = c("unit" = "first_authors_unit"))
  }

  build_top_result(ranked, "unit", "Top Units", n)
}


#' Top Venues by Publication Count
#'
#' @description
#' Returns a ranked table, formatted gt table, and bar plot of publication
#' venues (journals, series) by publication count. Auto-detects the venue
#' column.
#'
#' @param data A data frame from \code{\link{process_data}}.
#' @param n Integer. Number of top venues to return. Default is 20.
#' @param years Numeric vector. Years to include. If NULL, uses all years.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#'
#' @return A list with \code{table}, \code{gt_table}, and \code{plot}.
#'
#' @examples
#' \dontrun{
#' data <- process_data("Data/")
#' top_venues(data, n = 15)$gt_table
#' }
#'
#' @export
top_venues <- function(data, n = 20, years = NULL, jufo = 1:3) {
  venue_col <- detect_venue_column(names(data))
  if (is.null(venue_col)) {
    stop("top_venues: No venue/journal column found. ",
         "Expected one of: title_of_journal_series, journal, venue, ",
         "publication_channel, publication_series.")
  }

  filtered <- filter_by_criteria(data, years = years, jufo = jufo)
  pubs <- filtered %>%
    dplyr::distinct(.data$publication_id, .keep_all = TRUE) %>%
    dplyr::filter(!is.na(.data[[venue_col]]) & .data[[venue_col]] != "")

  ranked <- pubs %>%
    dplyr::group_by(venue = .data[[venue_col]]) %>%
    dplyr::summarise(
      publications = dplyr::n_distinct(.data$publication_id),
      mean_jufo_level = round(
        mean(as.numeric(.data$jufo_level_of_publication), na.rm = TRUE), 2
      ),
      pct_high_impact = round(
        100 * sum(.data$jufo_level_of_publication %in% c(2, 3)) /
          dplyr::n(), 1
      ),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$publications)) %>%
    utils::head(n)

  build_top_result(ranked, "venue", "Top Venues", n)
}


# ============================================================================
# describe() function
# ============================================================================

#' Quick Descriptive Statistics
#'
#' @description
#' Returns quick descriptive statistics for any prepared CRIS data.
#' Optionally breaks down by a grouping variable.
#'
#' @param data A data frame from \code{\link{process_data}}.
#' @param by Character string. Optional grouping variable: \code{"year"},
#'   \code{"faculty"}, \code{"department"}, or \code{"unit"}. Default is NULL
#'   (overall summary only).
#' @param years Numeric vector. Years to include. If NULL, uses all years.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#'
#' @return A list of class \code{"cris_describe"} with:
#'   \describe{
#'     \item{overall}{List with n_publications, n_authors, year_range, n_years,
#'       jufo_distribution, pct_high_impact, mean_pubs_per_year}
#'     \item{by_group}{Data frame with one row per group, or NULL if by=NULL}
#'   }
#'
#' @examples
#' \dontrun{
#' data <- process_data("Data/")
#' describe(data)
#' describe(data, by = "faculty")
#' describe(data, by = "year", years = 2020:2025)
#' }
#'
#' @export
describe <- function(data, by = NULL, years = NULL, jufo = 1:3) {
  validate_cris_data(
    data,
    required_cols = c("publication_id", "publication_year",
                      "jufo_level_of_publication"),
    func_name = "describe"
  )

  filtered <- filter_by_criteria(data, years = years, jufo = jufo)
  pubs <- filtered %>%
    dplyr::distinct(.data$publication_id, .keep_all = TRUE)

  # Overall stats
  yr <- pubs$publication_year
  jufo_vals <- as.numeric(pubs$jufo_level_of_publication)
  jufo_dist <- table(factor(pubs$jufo_level_of_publication, levels = 0:3))

  auth_col <- if ("Authorsclean" %in% names(filtered)) "Authorsclean"
              else if ("authors" %in% names(filtered)) "authors"
              else NULL
  n_authors <- if (!is.null(auth_col)) {
    dplyr::n_distinct(filtered[[auth_col]], na.rm = TRUE)
  } else {
    NA_integer_
  }

  n_pubs <- nrow(pubs)
  n_years <- dplyr::n_distinct(yr)

  overall <- list(
    n_publications = n_pubs,
    n_authors = n_authors,
    year_range = if (length(yr) > 0) range(yr, na.rm = TRUE) else c(NA, NA),
    n_years = n_years,
    jufo_distribution = as.integer(jufo_dist),
    pct_high_impact = if (n_pubs > 0) {
      round(100 * sum(jufo_vals %in% c(2, 3), na.rm = TRUE) / n_pubs, 1)
    } else 0,
    mean_pubs_per_year = if (n_years > 0) round(n_pubs / n_years, 1) else 0
  )
  names(overall$jufo_distribution) <- paste0("jufo_", 0:3)

  # Group breakdown
  by_group <- NULL
  if (!is.null(by)) {
    by <- match.arg(by, c("year", "faculty", "department", "unit"))
    grp_col <- switch(by,
      year = "publication_year",
      faculty = "faculty",
      department = "department",
      unit = "first_authors_unit"
    )

    if (!(grp_col %in% names(pubs))) {
      warning("describe: Column '", grp_col, "' not found in data. ",
              "Skipping group breakdown.")
    } else {
      by_group <- pubs %>%
        dplyr::group_by(group = .data[[grp_col]]) %>%
        dplyr::summarise(
          publications = dplyr::n(),
          pct_high_impact = round(
            100 * sum(.data$jufo_level_of_publication %in% c(2, 3)) /
              dplyr::n(), 1
          ),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(.data$publications))
      names(by_group)[1] <- by
    }
  }

  result <- list(overall = overall, by_group = by_group)
  class(result) <- "cris_describe"
  invisible(result)
}


#' @export
print.cris_describe <- function(x, ...) {
  cat("CRIS Data Summary\n")
  cat("=================\n")
  o <- x$overall
  cat("Publications:", o$n_publications, "\n")
  if (!is.na(o$n_authors)) cat("Authors:", o$n_authors, "\n")
  cat("Years:", o$year_range[1], "-", o$year_range[2],
      "(", o$n_years, "years )\n")
  cat("High impact (L2+3):", o$pct_high_impact, "%\n")
  cat("Mean pubs/year:", o$mean_pubs_per_year, "\n")
  cat("JUFO distribution:",
      paste(names(o$jufo_distribution), o$jufo_distribution,
            sep = "=", collapse = ", "), "\n")
  if (!is.null(x$by_group)) {
    cat("\nGroup breakdown:", nrow(x$by_group), "groups\n")
    print(utils::head(x$by_group, 10))
  }
  cat("\nAvailable: $overall, $by_group\n")
  invisible(x)
}


# ============================================================================
# Enriched wrapper functions
# ============================================================================

#' Analyze Individual Author
#'
#' @description
#' One-stop author analysis: publication trend, article list, descriptive
#' statistics, formatted gt table, and JUFO distribution plot.
#'
#' @param data A data frame from \code{\link{prepare_cris_data}} or
#'   \code{\link{process_data}}.
#' @param name Character string. Author name to search for (case-insensitive,
#'   partial match).
#' @param years Numeric vector. Years to include, e.g. \code{2020:2025}.
#'   If NULL (default), includes all years.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#'
#' @return A list of class \code{"cris_author_stats"} containing:
#'   \describe{
#'     \item{plot}{ggplot of the author's publication trend}
#'     \item{articles}{Data frame of the author's publications}
#'     \item{descriptives}{List of summary statistics}
#'     \item{gt_table}{Formatted gt table of articles}
#'     \item{jufo_plot}{Bar chart of JUFO level distribution}
#'   }
#'   Returns NULL if no articles are found.
#'
#' @examples
#' \dontrun{
#' data <- process_data("Data/")
#' me <- author_stats(data, "SMITH JOHN")
#' me
#' me$gt_table
#' me$jufo_plot
#' me$descriptives$coauthor_count
#' }
#'
#' @seealso \code{\link{analyze_author_publications}}, \code{\link{university_stats}}
#'
#' @export
author_stats <- function(data, name, years = NULL, jufo = 1:3) {
  # Pre-filter by JUFO
  filtered <- filter_by_criteria(data, years = NULL, jufo = jufo)

  # Smart name matching: try the name as-given first
  search_name <- name
  base_result <- suppressWarnings(
    analyze_author_publications(filtered, author_name = search_name,
                                year_range = years)
  )

  # If not found and name has no comma, try "LASTNAME, FIRSTNAME" format
  if (is.null(base_result)) {
    upper_name <- simplify_author_string(name)
    parts <- trimws(strsplit(upper_name, "\\s+")[[1]])

    if (length(parts) >= 2 && !grepl(",", name, fixed = TRUE)) {
      search_name <- paste0(parts[length(parts)], ", ",
                            paste(parts[-length(parts)], collapse = " "))
      base_result <- suppressWarnings(
        analyze_author_publications(filtered, author_name = search_name,
                                    year_range = years)
      )
    }

    # If still not found, try just the last name part
    if (is.null(base_result) && length(parts) >= 1) {
      search_name <- parts[length(parts)]
      base_result <- suppressWarnings(
        analyze_author_publications(filtered, author_name = search_name,
                                    year_range = years)
      )
    }
  }

  if (is.null(base_result)) {
    warning(paste("No articles found for author:", name,
                  "within the specified criteria."))
    return(NULL)
  }

  articles <- base_result$articles
  n_pubs <- nrow(articles)
  yr <- articles$publication_year
  jufo_vals <- as.numeric(articles$jufo_level_of_publication)

  # Coauthor count — use the matched search_name for filtering
  auth_col <- if ("Authorsclean" %in% names(data)) "Authorsclean" else NULL
  coauthor_count <- NA_integer_
  if (!is.null(auth_col)) {
    author_pub_ids <- articles$publication_id
    coauthors <- data %>%
      dplyr::filter(.data$publication_id %in% author_pub_ids,
                    !grepl(toupper(search_name), toupper(.data[[auth_col]]),
                           fixed = FALSE))
    coauthor_count <- dplyr::n_distinct(coauthors[[auth_col]])
  }

  # Most productive year
  yr_counts <- table(yr)
  most_productive_year <- if (length(yr_counts) > 0) {
    as.integer(names(which.max(yr_counts)))
  } else {
    NA_integer_
  }

  descriptives <- list(
    name = name,
    n_publications = n_pubs,
    n_years_active = dplyr::n_distinct(yr),
    year_range = if (length(yr) > 0) range(yr, na.rm = TRUE) else c(NA, NA),
    pubs_per_year = if (dplyr::n_distinct(yr) > 0) {
      round(n_pubs / dplyr::n_distinct(yr), 1)
    } else 0,
    jufo_distribution = as.integer(
      table(factor(articles$jufo_level_of_publication, levels = 0:3))
    ),
    pct_high_impact = if (n_pubs > 0) {
      round(100 * sum(jufo_vals %in% c(2, 3), na.rm = TRUE) / n_pubs, 1)
    } else 0,
    most_productive_year = most_productive_year,
    coauthor_count = coauthor_count
  )
  names(descriptives$jufo_distribution) <- paste0("jufo_", 0:3)

  # GT table of articles
  gt_cols <- intersect(
    c("publication_year", "title_of_publication", "jufo_level_of_publication"),
    names(articles)
  )
  gt_data <- articles[, gt_cols, drop = FALSE]

  labels_list <- list()
  if ("publication_year" %in% gt_cols) labels_list[["publication_year"]] <- "Year"
  if ("title_of_publication" %in% gt_cols) {
    labels_list[["title_of_publication"]] <- "Title"
  }
  if ("jufo_level_of_publication" %in% gt_cols) {
    labels_list[["jufo_level_of_publication"]] <- "JUFO"
  }

  gt_tbl <- gt_data %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste("Publications:", name),
      subtitle = paste(n_pubs, "articles")
    ) %>%
    gt::cols_label(.list = labels_list) %>%
    gt::cols_align(align = "center")

  # JUFO distribution plot
  jufo_df <- data.frame(
    jufo_level = factor(0:3),
    count = as.integer(
      table(factor(articles$jufo_level_of_publication, levels = 0:3))
    )
  )
  jufo_plot <- ggplot2::ggplot(
    jufo_df,
    ggplot2::aes(x = .data$jufo_level, y = .data$count)
  ) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::labs(
      title = paste("JUFO Distribution:", name),
      x = "JUFO Level", y = "Publications"
    ) +
    ggplot2::theme_minimal()

  result <- list(
    plot = base_result$plot,
    articles = articles,
    descriptives = descriptives,
    gt_table = gt_tbl,
    jufo_plot = jufo_plot
  )
  class(result) <- "cris_author_stats"
  result
}


#' @export
print.cris_author_stats <- function(x, ...) {
  d <- x$descriptives
  cat("Author Statistics:", d$name, "\n")
  cat("=================================\n")
  cat("Publications:", d$n_publications, "\n")
  cat("Years active:", d$n_years_active,
      "(", d$year_range[1], "-", d$year_range[2], ")\n")
  cat("High impact (L2+3):", d$pct_high_impact, "%\n")
  cat("Pubs/year:", d$pubs_per_year, "\n")
  if (!is.na(d$coauthor_count)) cat("Co-authors:", d$coauthor_count, "\n")
  cat("\nAvailable: $plot, $articles, $descriptives, $gt_table, $jufo_plot\n")
  invisible(x)
}


#' Faculty Publication Statistics
#'
#' @description
#' One-stop faculty analysis: comparison table, gt table, descriptive
#' statistics, plots, and top authors per faculty.
#'
#' @param data A data frame from \code{\link{process_data}}.
#' @param years Numeric vector. Years to include. If NULL, uses all years.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#' @param top_n Integer. Number of top authors per faculty. Default is 20.
#'
#' @return A list of class \code{"cris_faculty_stats"} with \code{table},
#'   \code{summary}, \code{gt_table}, \code{descriptives}, \code{plots},
#'   and \code{top_authors}.
#'
#' @examples
#' \dontrun{
#' data <- process_data("Data/")
#' fac <- faculty_stats(data)
#' fac
#' fac$gt_table
#' fac$plots$trends
#' fac$top_authors$"Health Sciences"$gt_table
#' }
#'
#' @seealso \code{\link{compare_faculties}}, \code{\link{university_stats}}
#'
#' @export
faculty_stats <- function(data, years = NULL, jufo = 1:3, top_n = 20) {
  base <- compare_faculties(data, years = years, jufo = jufo)

  # GT table
  gt_tbl <- build_stats_gt(
    base$table,
    title = "Faculty Comparison",
    subtitle = paste(base$summary$total_faculties, "faculties,",
                     base$summary$total_publications, "publications"),
    level_col = "faculty"
  )

  # Descriptives
  pubs_vec <- base$table$publications
  top_fac <- if (nrow(base$table) > 0) base$table$faculty[1] else NA
  descriptives <- list(
    n_publications = base$summary$total_publications,
    n_faculties = base$summary$total_faculties,
    pct_high_impact = if (sum(pubs_vec) > 0) {
      round(100 * sum(base$table$level_2_3) / sum(pubs_vec), 1)
    } else 0,
    top_faculty = top_fac,
    sd_publications = round(stats::sd(pubs_vec), 1),
    cv_publications = if (mean(pubs_vec) > 0) {
      round(stats::sd(pubs_vec) / mean(pubs_vec), 2)
    } else NA_real_,
    gini_coefficient = round(compute_gini(pubs_vec), 3)
  )

  # Plots
  plots <- list()
  plots$comparison <- tryCatch(
    plot_comparison(data, level = "faculty", years = years, jufo = jufo),
    error = function(e) NULL
  )
  plots$trends <- tryCatch(
    plot_trends(data, level = "faculty", years = years, jufo = jufo),
    error = function(e) NULL
  )
  plots$jufo_dist <- tryCatch(
    plot_jufo_distribution(data, by = "faculty", years = years),
    error = function(e) NULL
  )

  # Top authors per faculty
  top_auth <- list()
  has_authors <- "Authorsclean" %in% names(data) || "authors" %in% names(data)
  if (has_authors && "faculty" %in% names(data)) {
    for (fac_name in base$table$faculty) {
      fac_data <- data[data$faculty == fac_name, , drop = FALSE]
      if (nrow(fac_data) > 0) {
        top_auth[[fac_name]] <- tryCatch(
          compare_authors(fac_data, top_n = top_n, years = years, jufo = jufo),
          error = function(e) NULL
        )
      }
    }
  }

  result <- list(
    table = base$table,
    summary = base$summary,
    gt_table = gt_tbl,
    descriptives = descriptives,
    plots = plots,
    top_authors = top_auth
  )
  class(result) <- "cris_faculty_stats"
  result
}


#' @export
print.cris_faculty_stats <- function(x, ...) {
  d <- x$descriptives
  cat("Faculty Statistics\n")
  cat("==================\n")
  cat("Faculties:", d$n_faculties, "| Publications:", d$n_publications, "\n")
  cat("High impact:", d$pct_high_impact, "% | Gini:", d$gini_coefficient, "\n")
  if (!is.na(d$top_faculty)) cat("Top faculty:", d$top_faculty, "\n")
  cat("\nAvailable: $table, $summary, $gt_table, $descriptives,",
      "$plots, $top_authors\n")
  invisible(x)
}


#' Department Publication Statistics
#'
#' @description
#' One-stop department analysis: comparison table, gt table, descriptive
#' statistics, plots, and top authors per department.
#'
#' @param data A data frame from \code{\link{process_data}}.
#' @param pattern Character string. Wildcard pattern to filter departments.
#'   Default is \code{"*"} (all).
#' @param faculty Character string. Wildcard pattern to filter by faculty.
#'   Default is NULL (all faculties).
#' @param years Numeric vector. Years to include. If NULL, uses all years.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#' @param top_n Integer. Maximum departments to return. Default is 50.
#'
#' @return A list of class \code{"cris_department_stats"} with \code{table},
#'   \code{summary}, \code{pattern_used}, \code{gt_table}, \code{descriptives},
#'   \code{plots}, and \code{top_authors}.
#'
#' @examples
#' \dontrun{
#' data <- process_data("Data/")
#' dept <- department_stats(data)
#' dept
#' dept$gt_table
#' dept$plots$comparison
#' }
#'
#' @seealso \code{\link{compare_departments}}, \code{\link{university_stats}}
#'
#' @export
department_stats <- function(data, pattern = "*", faculty = NULL,
                             years = NULL, jufo = 1:3, top_n = 50) {
  base <- compare_departments(data, pattern = pattern, faculty = faculty,
                              years = years, jufo = jufo, top_n = top_n)

  # GT table
  gt_tbl <- build_stats_gt(
    base$table,
    title = "Department Comparison",
    subtitle = paste(base$summary$total_departments, "departments,",
                     base$summary$total_publications, "publications"),
    level_col = "department"
  )

  # Descriptives
  pubs_vec <- base$table$publications
  top_dept <- if (nrow(base$table) > 0) base$table$department[1] else NA

  by_faculty_summary <- NULL
  if ("faculty" %in% names(base$table)) {
    by_faculty_summary <- base$table %>%
      dplyr::group_by(.data$faculty) %>%
      dplyr::summarise(
        departments = dplyr::n(),
        publications = sum(.data$publications),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(.data$publications))
  }

  descriptives <- list(
    n_publications = base$summary$total_publications,
    n_departments = base$summary$total_departments,
    pct_high_impact = if (sum(pubs_vec) > 0) {
      round(100 * sum(base$table$level_2_3) / sum(pubs_vec), 1)
    } else 0,
    top_department = top_dept,
    sd_publications = round(stats::sd(pubs_vec), 1),
    cv_publications = if (mean(pubs_vec) > 0) {
      round(stats::sd(pubs_vec) / mean(pubs_vec), 2)
    } else NA_real_,
    by_faculty_summary = by_faculty_summary
  )

  # Plots
  plots <- list()
  plots$comparison <- tryCatch(
    plot_comparison(data, level = "department", years = years, jufo = jufo,
                    top_n = min(top_n, 20)),
    error = function(e) NULL
  )
  plots$trends <- tryCatch(
    plot_trends(data, level = "department", years = years, jufo = jufo),
    error = function(e) NULL
  )
  plots$jufo_dist <- tryCatch(
    plot_jufo_distribution(data, by = "department", years = years),
    error = function(e) NULL
  )

  # Top authors per department
  top_auth <- list()
  has_authors <- "Authorsclean" %in% names(data) || "authors" %in% names(data)
  if (has_authors && "department" %in% names(data)) {
    for (dept_name in base$table$department) {
      dept_data <- data[data$department == dept_name, , drop = FALSE]
      if (nrow(dept_data) > 0) {
        top_auth[[dept_name]] <- tryCatch(
          compare_authors(dept_data, top_n = 10, years = years, jufo = jufo),
          error = function(e) NULL
        )
      }
    }
  }

  result <- list(
    table = base$table,
    summary = base$summary,
    pattern_used = base$pattern_used,
    gt_table = gt_tbl,
    descriptives = descriptives,
    plots = plots,
    top_authors = top_auth
  )
  class(result) <- "cris_department_stats"
  result
}


#' @export
print.cris_department_stats <- function(x, ...) {
  d <- x$descriptives
  cat("Department Statistics\n")
  cat("=====================\n")
  cat("Departments:", d$n_departments, "| Publications:", d$n_publications, "\n")
  cat("High impact:", d$pct_high_impact, "%\n")
  if (!is.na(d$top_department)) cat("Top department:", d$top_department, "\n")
  cat("\nAvailable: $table, $summary, $pattern_used, $gt_table,",
      "$descriptives, $plots, $top_authors\n")
  invisible(x)
}


#' Unit Publication Statistics
#'
#' @description
#' One-stop unit analysis: comparison table, gt table, descriptive
#' statistics, plots, growth analysis, and top authors per unit.
#'
#' @param data A data frame from \code{\link{process_data}}.
#' @param pattern Character string. Wildcard pattern to filter units.
#'   Default is \code{"*"} (all).
#' @param years Numeric vector. Years to include. If NULL, uses all years.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#' @param top_n Integer. Maximum units to return. Default is 50.
#'
#' @return A list of class \code{"cris_unit_stats"} with \code{table},
#'   \code{summary}, \code{pattern_used}, \code{gt_table}, \code{descriptives},
#'   \code{plots}, \code{growth}, and \code{top_authors}.
#'
#' @examples
#' \dontrun{
#' data <- process_data("Data/")
#' units <- unit_stats(data)
#' units
#' units$growth$growth_plot
#' units$descriptives$gini_coefficient
#' }
#'
#' @seealso \code{\link{compare_units}}, \code{\link{university_stats}}
#'
#' @export
unit_stats <- function(data, pattern = "*", years = NULL,
                       jufo = 1:3, top_n = 50) {
  base <- compare_units(data, pattern = pattern, years = years, jufo = jufo,
                        top_n = top_n)

  # GT table
  gt_tbl <- build_stats_gt(
    base$table,
    title = "Unit Comparison",
    subtitle = paste(base$summary$total_units, "units,",
                     base$summary$total_publications, "publications"),
    level_col = "first_authors_unit"
  )

  # Descriptives
  pubs_vec <- base$table$publications
  pct_high <- base$table$pct_high_impact
  top_unit <- if (nrow(base$table) > 0) {
    base$table$first_authors_unit[1]
  } else NA

  descriptives <- list(
    n_publications = base$summary$total_publications,
    n_units = base$summary$total_units,
    pct_high_impact = if (sum(pubs_vec) > 0) {
      round(100 * sum(base$table$level_2_3) / sum(pubs_vec), 1)
    } else 0,
    top_unit = top_unit,
    sd_publications = round(stats::sd(pubs_vec), 1),
    cv_publications = if (mean(pubs_vec) > 0) {
      round(stats::sd(pubs_vec) / mean(pubs_vec), 2)
    } else NA_real_,
    mean_pct_high = round(mean(pct_high, na.rm = TRUE), 1),
    median_pct_high = round(stats::median(pct_high, na.rm = TRUE), 1),
    gini_coefficient = round(compute_gini(pubs_vec), 3)
  )

  # Plots
  plots <- list()
  plots$comparison <- tryCatch(
    plot_comparison(data, level = "unit", years = years, jufo = jufo,
                    top_n = min(top_n, 20)),
    error = function(e) NULL
  )
  plots$trends <- tryCatch(
    plot_trends(data, level = "unit", years = years, jufo = jufo),
    error = function(e) NULL
  )
  plots$jufo_dist <- tryCatch(
    plot_jufo_distribution(data, by = "unit", years = years),
    error = function(e) NULL
  )

  # Growth analysis
  growth <- tryCatch(
    plot_unit_growth(data, levels = jufo),
    error = function(e) NULL
  )

  # Top authors for top 5 units
  top_auth <- list()
  has_authors <- "Authorsclean" %in% names(data) || "authors" %in% names(data)
  if (has_authors && "first_authors_unit" %in% names(data)) {
    top_5_units <- utils::head(base$table$first_authors_unit, 5)
    for (unit_name in top_5_units) {
      unit_data <- data[data$first_authors_unit == unit_name, , drop = FALSE]
      if (nrow(unit_data) > 0) {
        top_auth[[unit_name]] <- tryCatch(
          compare_authors(unit_data, top_n = 10, years = years, jufo = jufo),
          error = function(e) NULL
        )
      }
    }
  }

  result <- list(
    table = base$table,
    summary = base$summary,
    pattern_used = base$pattern_used,
    gt_table = gt_tbl,
    descriptives = descriptives,
    plots = plots,
    growth = growth,
    top_authors = top_auth
  )
  class(result) <- "cris_unit_stats"
  result
}


#' @export
print.cris_unit_stats <- function(x, ...) {
  d <- x$descriptives
  cat("Unit Statistics\n")
  cat("===============\n")
  cat("Units:", d$n_units, "| Publications:", d$n_publications, "\n")
  cat("High impact:", d$pct_high_impact, "% | Gini:", d$gini_coefficient, "\n")
  cat("Mean % high:", d$mean_pct_high, "| Median % high:",
      d$median_pct_high, "\n")
  if (!is.na(d$top_unit)) cat("Top unit:", d$top_unit, "\n")
  cat("\nAvailable: $table, $summary, $pattern_used, $gt_table,",
      "$descriptives, $plots, $growth, $top_authors\n")
  invisible(x)
}


#' Author Productivity Analysis
#'
#' @description
#' Comprehensive author productivity analysis with fractional counting,
#' position analysis, JUFO level breakdowns, descriptive statistics, and
#' distribution plots.
#'
#' @param data A data frame from \code{\link{process_data}}.
#' @param years Numeric vector. Years to include. If NULL, uses the most
#'   recent year.
#' @param top_n Integer. Number of top authors. Default is 30.
#' @param levels Character vector of JUFO levels. Default is
#'   \code{c("1", "2", "3")}.
#'
#' @return A list of class \code{"cris_productivity_stats"} with all elements
#'   from \code{\link{generate_author_productivity_tables}} plus
#'   \code{descriptives} and \code{plots}.
#'
#' @examples
#' \dontrun{
#' data <- process_data("Data/")
#' prod <- productivity_stats(data, years = 2020:2025)
#' prod
#' prod$comprehensive_analysis
#' prod$plots$productivity_distribution
#' }
#'
#' @seealso \code{\link{generate_author_productivity_tables}},
#'   \code{\link{university_stats}}
#'
#' @export
productivity_stats <- function(data, years = NULL, top_n = 30,
                               levels = c("1", "2", "3")) {
  base <- generate_author_productivity_tables(data, years = years,
                                              top_n = top_n, levels = levels)

  # Descriptives from raw_data
  raw <- base$raw_data
  if (!is.null(raw) && nrow(raw) > 0) {
    # Column is 'Total' in generate_author_productivity_tables output
    pub_col <- if ("Total" %in% names(raw)) "Total"
               else if ("total_publications" %in% names(raw)) "total_publications"
               else NULL
    pub_counts <- if (!is.null(pub_col)) {
      as.numeric(raw[[pub_col]])
    } else {
      rep(NA_real_, nrow(raw))
    }
    top_author_idx <- which.max(pub_counts)
    auth_col <- if ("Author" %in% names(raw)) "Author"
                else if ("Authorsclean" %in% names(raw)) "Authorsclean"
                else if ("author" %in% names(raw)) "author"
                else names(raw)[1]

    descriptives <- list(
      n_authors = nrow(raw),
      n_publications = sum(pub_counts, na.rm = TRUE),
      mean_pubs_per_author = round(mean(pub_counts, na.rm = TRUE), 1),
      median_pubs_per_author = round(stats::median(pub_counts, na.rm = TRUE), 1),
      gini_productivity = round(compute_gini(pub_counts), 3),
      top_author = if (length(top_author_idx) > 0) {
        as.character(raw[[auth_col]][top_author_idx])
      } else NA_character_,
      top_author_count = if (length(top_author_idx) > 0) {
        pub_counts[top_author_idx]
      } else NA_integer_
    )

    # Plots
    prod_plots <- list()

    # Productivity distribution histogram
    hist_df <- data.frame(publications = pub_counts)
    prod_plots$productivity_distribution <- ggplot2::ggplot(
      hist_df,
      ggplot2::aes(x = .data$publications)
    ) +
      ggplot2::geom_histogram(binwidth = 1, fill = "steelblue",
                              color = "white") +
      ggplot2::labs(title = "Author Productivity Distribution",
                    x = "Publications", y = "Number of Authors") +
      ggplot2::theme_minimal()

    # Position distribution
    if (!is.null(base$position_counts) && nrow(base$position_counts) > 0 &&
        "Position" %in% names(base$position_counts) &&
        "Authorsclean" %in% names(base$position_counts)) {
      pos_agg <- base$position_counts %>%
        dplyr::group_by(.data$Authorsclean, .data$Position) %>%
        dplyr::summarise(count = dplyr::n(), .groups = "drop")

      # Get top authors by total count
      top_auth_pos <- pos_agg %>%
        dplyr::group_by(.data$Authorsclean) %>%
        dplyr::summarise(total = sum(.data$count), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(.data$total)) %>%
        utils::head(top_n)

      pos_plot_data <- pos_agg %>%
        dplyr::filter(.data$Authorsclean %in% top_auth_pos$Authorsclean)

      if (nrow(pos_plot_data) > 0) {
        prod_plots$position_distribution <- ggplot2::ggplot(
          pos_plot_data,
          ggplot2::aes(x = stats::reorder(.data$Authorsclean, .data$count),
                       y = .data$count, fill = .data$Position)
        ) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::coord_flip() +
          ggplot2::labs(title = "Author Position Distribution",
                        x = NULL, y = "Publications", fill = "Position") +
          ggplot2::theme_minimal()
      }
    }
  } else {
    descriptives <- list(
      n_authors = 0, n_publications = 0,
      mean_pubs_per_author = 0, median_pubs_per_author = 0,
      gini_productivity = NA_real_,
      top_author = NA_character_, top_author_count = NA_integer_
    )
    prod_plots <- list()
  }

  base$descriptives <- descriptives
  base$plots <- prod_plots
  class(base) <- "cris_productivity_stats"
  base
}


#' @export
print.cris_productivity_stats <- function(x, ...) {
  d <- x$descriptives
  cat("Productivity Statistics\n")
  cat("=======================\n")
  cat("Authors:", d$n_authors, "| Publications:", d$n_publications, "\n")
  cat("Mean pubs/author:", d$mean_pubs_per_author,
      "| Median:", d$median_pubs_per_author, "\n")
  if (!is.na(d$gini_productivity)) {
    cat("Gini coefficient:", d$gini_productivity, "\n")
  }
  if (!is.na(d$top_author)) {
    cat("Top author:", d$top_author, "(", d$top_author_count, "pubs )\n")
  }
  cat("\nAvailable: $comprehensive_analysis, $most_productive_overall,",
      "$detailed_productivity,\n")
  cat("  $raw_data, $descriptives, $plots\n")
  invisible(x)
}


# ============================================================================
# S3 plot() methods
# ============================================================================

#' Plot Faculty Statistics
#'
#' @param x A \code{cris_faculty_stats} object.
#' @param type Character. Which plot: \code{"comparison"} (default),
#'   \code{"jufo"}, or \code{"trends"}.
#' @param ... Ignored.
#'
#' @export
plot.cris_faculty_stats <- function(x, type = c("comparison", "jufo", "trends"), ...) {
  type <- match.arg(type)
  p <- x$plots[[switch(type, comparison = "comparison",
                        jufo = "jufo_dist", trends = "trends")]]
  if (is.null(p)) message("Plot '", type, "' not available.")
  else print(p)
  invisible(x)
}


#' Plot Department Statistics
#'
#' @param x A \code{cris_department_stats} object.
#' @param type Character. Which plot: \code{"comparison"} (default),
#'   \code{"jufo"}, or \code{"trends"}.
#' @param ... Ignored.
#'
#' @export
plot.cris_department_stats <- function(x, type = c("comparison", "jufo", "trends"), ...) {
  type <- match.arg(type)
  p <- x$plots[[switch(type, comparison = "comparison",
                        jufo = "jufo_dist", trends = "trends")]]
  if (is.null(p)) message("Plot '", type, "' not available.")
  else print(p)
  invisible(x)
}


#' Plot Unit Statistics
#'
#' @param x A \code{cris_unit_stats} object.
#' @param type Character. Which plot: \code{"comparison"} (default),
#'   \code{"jufo"}, \code{"trends"}, \code{"growth"}, or \code{"counts"}.
#' @param ... Ignored.
#'
#' @export
plot.cris_unit_stats <- function(x, type = c("comparison", "jufo", "trends",
                                              "growth", "counts"), ...) {
  type <- match.arg(type)
  if (type %in% c("growth", "counts")) {
    pname <- if (type == "growth") "growth_plot" else "counts_plot"
    p <- if (!is.null(x$growth)) x$growth[[pname]] else NULL
  } else {
    p <- x$plots[[switch(type, comparison = "comparison",
                          jufo = "jufo_dist", trends = "trends")]]
  }
  if (is.null(p)) message("Plot '", type, "' not available.")
  else print(p)
  invisible(x)
}


#' Plot Author Statistics
#'
#' @param x A \code{cris_author_stats} object.
#' @param type Character. Which plot: \code{"trend"} (default) or \code{"jufo"}.
#' @param ... Ignored.
#'
#' @export
plot.cris_author_stats <- function(x, type = c("trend", "jufo"), ...) {
  type <- match.arg(type)
  p <- switch(type, trend = x$plot, jufo = x$jufo_plot)
  if (is.null(p)) message("Plot '", type, "' not available.")
  else print(p)
  invisible(x)
}


#' Plot Productivity Statistics
#'
#' @param x A \code{cris_productivity_stats} object.
#' @param type Character. Which plot: \code{"distribution"} (default) or
#'   \code{"positions"}.
#' @param ... Ignored.
#'
#' @export
plot.cris_productivity_stats <- function(x, type = c("distribution", "positions"), ...) {
  type <- match.arg(type)
  p <- x$plots[[switch(type, distribution = "productivity_distribution",
                        positions = "position_distribution")]]
  if (is.null(p)) message("Plot '", type, "' not available.")
  else print(p)
  invisible(x)
}


# ============================================================================
# S3 summary() methods
# ============================================================================

#' @export
summary.cris_faculty_stats <- function(object, ...) object$descriptives

#' @export
summary.cris_department_stats <- function(object, ...) object$descriptives

#' @export
summary.cris_unit_stats <- function(object, ...) object$descriptives

#' @export
summary.cris_author_stats <- function(object, ...) object$descriptives

#' @export
summary.cris_productivity_stats <- function(object, ...) object$descriptives

#' @export
summary.cris_describe <- function(object, ...) object$overall


#' Detect Type Column
#'
#' @description
#' Internal helper to detect a publication type column by name.
#'
#' @param col_names Character vector of column names to search.
#'
#' @return The matching column name, or \code{NULL} if none found.
#'
#' @keywords internal
detect_type_column <- function(col_names) {
  candidates <- c("type_of_publication", "publication_type",
                   "type", "publication_type_code")
  lower_names <- tolower(col_names)
  for (cand in candidates) {
    idx <- which(lower_names == cand)
    if (length(idx) > 0) return(col_names[idx[1]])
  }
  NULL
}


# ============================================================================
# Aliases — consistent stats_* and top_* naming
# ============================================================================

#' @rdname author_stats
#' @export
stats_author <- author_stats

#' @rdname faculty_stats
#' @export
stats_faculty <- faculty_stats

#' @rdname department_stats
#' @export
stats_department <- department_stats

#' @rdname unit_stats
#' @export
stats_unit <- unit_stats

#' @rdname productivity_stats
#' @export
stats_productivity <- productivity_stats

#' @rdname university_stats
#' @export
stats_university <- university_stats

#' @rdname describe
#' @export
stats_describe <- describe


# ============================================================================
# compare_years() — Multi-Year Comparison
# ============================================================================

#' Compare Performance Across Years
#'
#' @description
#' Shows how authors, departments, or faculties perform across years.
#' Returns an Entity x Year table with heatmap and line plots.
#'
#' @param data A data frame from \code{\link{process_data}}.
#' @param by Character. Grouping level: \code{"author"}, \code{"department"},
#'   or \code{"faculty"}. Default is \code{"author"}.
#' @param n Integer. Number of top entities to show. Default is 20.
#' @param years Numeric vector. Years to include. If NULL, uses all years.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#'
#' @return A list of class \code{"cris_yearly_comparison"} containing:
#'   \describe{
#'     \item{table}{Wide data frame: entity | year columns | total | change}
#'     \item{long_data}{Long format: entity, year, publications}
#'     \item{gt_table}{gt table with color-coded year columns}
#'     \item{plots}{List with \code{heatmap} and \code{lines} plots}
#'     \item{by}{Grouping level used}
#'     \item{years}{Sorted year vector}
#'     \item{n}{Number of entities shown}
#'   }
#'
#' @examples
#' \dontrun{
#' data <- process_data("Data/", years = 2020:2025)
#'
#' # Faculty trends
#' yf <- compare_years(data, "faculty")
#' yf$gt_table
#' plot(yf, "lines")
#'
#' # Top 15 authors
#' ya <- compare_years(data, "author", n = 15)
#' plot(ya)  # heatmap
#'
#' # Department trends
#' yd <- compare_years(data, "department", n = 15)
#' }
#'
#' @seealso \code{\link{university_stats}}, \code{\link{plot_trends}}
#'
#' @export
compare_years <- function(data, by = c("author", "department", "faculty"),
                          n = 20, years = NULL, jufo = 1:3) {

  by <- match.arg(by)

  validate_cris_data(
    data,
    required_cols = c("publication_id", "publication_year"),
    func_name = "compare_years"
  )

  # Determine grouping column
  grp_col <- switch(by,
    author = {
      if ("Authorsclean" %in% names(data)) "Authorsclean"
      else if ("authors" %in% names(data)) "authors"
      else stop("compare_years: No author column found (Authorsclean or authors).")
    },
    department = {
      if (!("department" %in% names(data)))
        stop("compare_years: No 'department' column found.")
      "department"
    },
    faculty = {
      if (!("faculty" %in% names(data)))
        stop("compare_years: No 'faculty' column found.")
      "faculty"
    }
  )

  # Filter by criteria
  filtered <- filter_by_criteria(data, years = years, jufo = jufo)

  # Deduplicate: one row per publication × entity
  deduped <- filtered %>%
    dplyr::distinct(.data$publication_id, .data[[grp_col]],
                    .keep_all = TRUE)

  # Find top N entities by total publications
  top_entities <- deduped %>%
    dplyr::count(.data[[grp_col]], name = "total") %>%
    dplyr::arrange(dplyr::desc(.data$total)) %>%
    utils::head(n)

  entity_names <- top_entities[[grp_col]]

  # Build long format
  long_data <- deduped %>%
    dplyr::filter(.data[[grp_col]] %in% entity_names) %>%
    dplyr::group_by(
      entity = .data[[grp_col]],
      year = .data$publication_year
    ) %>%
    dplyr::summarise(publications = dplyr::n(), .groups = "drop")

  # Complete all entity × year combos with 0 fill
  all_years <- sort(unique(deduped$publication_year))
  grid <- expand.grid(
    entity = entity_names,
    year = all_years,
    stringsAsFactors = FALSE
  )
  long_data <- dplyr::left_join(grid, long_data, by = c("entity", "year"))
  long_data$publications[is.na(long_data$publications)] <- 0L

  # Pivot to wide format
  wide_data <- long_data %>%
    tidyr::pivot_wider(
      names_from = "year",
      values_from = "publications",
      values_fill = 0L
    )

  # Add total and change columns
  year_cols <- as.character(all_years)
  wide_data$total <- rowSums(wide_data[, year_cols, drop = FALSE])

  first_yr <- as.character(min(all_years))
  last_yr <- as.character(max(all_years))
  wide_data$change <- wide_data[[last_yr]] - wide_data[[first_yr]]

  # Order by total descending
  wide_data <- wide_data[order(-wide_data$total), ]

  # Build gt table
  gt_tbl <- wide_data %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste("Yearly Comparison:", tools::toTitleCase(by)),
      subtitle = paste(length(all_years), "years,",
                       sum(wide_data$total), "publications,",
                       "top", nrow(wide_data))
    ) %>%
    gt::cols_label(entity = tools::toTitleCase(by),
                   total = "Total", change = "Change") %>%
    gt::cols_align(align = "center", columns = c(year_cols, "total", "change")) %>%
    gt::cols_align(align = "left", columns = "entity") %>%
    gt::data_color(
      columns = dplyr::all_of(year_cols),
      palette = c("white", "steelblue")
    )

  # Build heatmap plot
  long_data$entity <- factor(long_data$entity,
                             levels = rev(wide_data$entity))

  p_heatmap <- ggplot2::ggplot(
    long_data,
    ggplot2::aes(x = factor(.data$year), y = .data$entity,
                 fill = .data$publications)
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$publications),
      size = 3
    ) +
    ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
    ggplot2::labs(
      title = paste("Publications by", tools::toTitleCase(by), "and Year"),
      x = "Year", y = NULL, fill = "Publications"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())

  # Build line plot
  long_data$entity_lines <- factor(long_data$entity,
                                   levels = wide_data$entity)

  p_lines <- ggplot2::ggplot(
    long_data,
    ggplot2::aes(x = .data$year, y = .data$publications,
                 color = .data$entity_lines, group = .data$entity_lines)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::labs(
      title = paste(tools::toTitleCase(by), "Publication Trends"),
      x = "Year", y = "Publications", color = tools::toTitleCase(by)
    ) +
    ggplot2::theme_minimal()

  result <- list(
    table = wide_data,
    long_data = long_data,
    gt_table = gt_tbl,
    plots = list(heatmap = p_heatmap, lines = p_lines),
    by = by,
    years = all_years,
    n = nrow(wide_data)
  )
  class(result) <- "cris_yearly_comparison"
  result
}


#' @rdname compare_years
#' @export
yearly <- compare_years


#' @export
print.cris_yearly_comparison <- function(x, ...) {
  cat("Yearly Comparison:", tools::toTitleCase(x$by), "\n")
  cat("===================================\n")
  cat("Entities:", x$n, "| Years:", min(x$years), "-", max(x$years),
      "(", length(x$years), ")\n")
  cat("Total publications:", sum(x$table$total), "\n")
  top <- x$table$entity[1]
  cat("Top", paste0(x$by, ":"), top, "(", x$table$total[1], "pubs )\n")
  cat("\nAvailable: $table, $long_data, $gt_table, $plots\n")
  cat("Plot types: plot(x) for heatmap, plot(x, 'lines') for line chart\n")
  invisible(x)
}


#' Plot Yearly Comparison
#'
#' @param x A \code{cris_yearly_comparison} object.
#' @param type Character. Which plot: \code{"heatmap"} (default) or
#'   \code{"lines"}.
#' @param ... Ignored.
#'
#' @export
plot.cris_yearly_comparison <- function(x, type = c("heatmap", "lines"), ...) {
  type <- match.arg(type)
  p <- x$plots[[type]]
  if (is.null(p)) message("Plot '", type, "' not available.")
  else print(p)
  invisible(x)
}


#' @export
summary.cris_yearly_comparison <- function(object, ...) {
  tbl <- object$table
  list(
    n_entities = object$n,
    n_years = length(object$years),
    total_pubs = sum(tbl$total),
    top_entity = tbl$entity[1],
    growth_summary = data.frame(
      entity = tbl$entity,
      total = tbl$total,
      change = tbl$change,
      stringsAsFactors = FALSE
    )
  )
}
