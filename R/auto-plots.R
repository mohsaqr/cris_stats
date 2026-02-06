#' Plot Comparison
#'
#' @description
#' Creates automatic bar chart visualizations comparing publication counts
#' at different organizational levels.
#'
#' @param data A data frame from \code{\link{prepare_cris_data}}.
#' @param level Character string. Organizational level to compare:
#'   \code{"faculty"}, \code{"department"}, or \code{"unit"}.
#' @param years Numeric vector. Years to include. If NULL, uses all years.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#' @param top_n Integer. Maximum number of items to display. Default is 20.
#' @param fill_by Character string. How to color bars: \code{"total"} (single color),
#'   \code{"jufo"} (by JUFO level), or \code{"faculty"} (by faculty, for units/departments).
#'   Default is \code{"total"}.
#' @param title Character string. Custom plot title. If NULL, auto-generated.
#' @param horizontal Logical. If TRUE, creates horizontal bar chart.
#'   Default is TRUE for better label readability.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' data <- prepare_cris_data("Data/")
#'
#' # Faculty comparison
#' plot_comparison(data, level = "faculty")
#'
#' # Top 15 departments colored by faculty
#' plot_comparison(data, level = "department", top_n = 15, fill_by = "faculty")
#'
#' # Unit comparison for specific years
#' plot_comparison(data, level = "unit", years = 2022:2024)
#' }
#'
#' @export
plot_comparison <- function(data,
                            level = c("faculty", "department", "unit"),
                            years = NULL,
                            jufo = 1:3,
                            top_n = 20,
                            fill_by = c("total", "jufo", "faculty"),
                            title = NULL,
                            horizontal = TRUE) {

  level <- match.arg(level)
  fill_by <- match.arg(fill_by)

  # Validate level column exists
  level_col <- switch(level,
                      faculty = "faculty",
                      department = "department",
                      unit = "first_authors_unit")

  if (!(level_col %in% names(data))) {
    if (level %in% c("faculty", "department")) {
      stop("Column '", level_col, "' not found. Use prepare_cris_data() with add_hierarchy = TRUE")
    } else {
      stop("Column '", level_col, "' not found in data")
    }
  }

  # Apply filters
  filtered <- filter_by_criteria(data, years = years, jufo = jufo)

  # Remove NA values in level column
  filtered <- filtered %>%
    dplyr::filter(!is.na(.data[[level_col]]))

  # Use abbreviation for units if available
  if (level == "unit" && "unit_abbrev" %in% names(filtered)) {
    filtered <- filtered %>%
      dplyr::mutate(plot_label = .data$unit_abbrev)
    label_col <- "plot_label"
  } else {
    label_col <- level_col
  }

  # Aggregate data
  if (fill_by == "jufo") {
    plot_data <- filtered %>%
      dplyr::group_by(.data[[label_col]], .data$jufo_level_of_publication) %>%
      dplyr::summarise(
        count = dplyr::n_distinct(.data$publication_id),
        .groups = "drop"
      ) %>%
      dplyr::rename(item = dplyr::all_of(label_col))

    # Get top items by total
    top_items <- plot_data %>%
      dplyr::group_by(.data$item) %>%
      dplyr::summarise(total = sum(.data$count), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(.data$total)) %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::pull(.data$item)

    plot_data <- plot_data %>%
      dplyr::filter(.data$item %in% top_items) %>%
      dplyr::mutate(
        item = factor(.data$item, levels = rev(top_items)),
        jufo_level_of_publication = factor(.data$jufo_level_of_publication)
      )

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(
      x = if (horizontal) .data$count else .data$item,
      y = if (horizontal) .data$item else .data$count,
      fill = .data$jufo_level_of_publication
    )) +
      ggplot2::geom_col(position = "stack") +
      ggplot2::scale_fill_brewer(palette = "Blues", name = "JUFO Level")

  } else if (fill_by == "faculty" && "faculty" %in% names(filtered)) {
    plot_data <- filtered %>%
      dplyr::group_by(.data[[label_col]], .data$faculty) %>%
      dplyr::summarise(
        count = dplyr::n_distinct(.data$publication_id),
        .groups = "drop"
      ) %>%
      dplyr::rename(item = dplyr::all_of(label_col))

    # Get top items by total
    top_items <- plot_data %>%
      dplyr::group_by(.data$item) %>%
      dplyr::summarise(total = sum(.data$count), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(.data$total)) %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::pull(.data$item)

    plot_data <- plot_data %>%
      dplyr::filter(.data$item %in% top_items) %>%
      dplyr::mutate(item = factor(.data$item, levels = rev(top_items)))

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(
      x = if (horizontal) .data$count else .data$item,
      y = if (horizontal) .data$item else .data$count,
      fill = .data$faculty
    )) +
      ggplot2::geom_col(position = "stack") +
      ggplot2::scale_fill_brewer(palette = "Set2", name = "Faculty")

  } else {
    # Single color (total)
    plot_data <- filtered %>%
      dplyr::group_by(.data[[label_col]]) %>%
      dplyr::summarise(
        count = dplyr::n_distinct(.data$publication_id),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(.data$count)) %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::rename(item = dplyr::all_of(label_col)) %>%
      dplyr::mutate(item = factor(.data$item, levels = rev(.data$item)))

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(
      x = if (horizontal) .data$count else .data$item,
      y = if (horizontal) .data$item else .data$count
    )) +
      ggplot2::geom_col(fill = "steelblue")
  }

  # Build title
  if (is.null(title)) {
    year_str <- if (!is.null(years)) {
      paste0(" (", min(years), "-", max(years), ")")
    } else {
      ""
    }
    title <- paste0("Publications by ", tools::toTitleCase(level), year_str)
  }

  # Add labels and theme
  p <- p +
    ggplot2::labs(
      title = title,
      x = if (horizontal) "Number of Publications" else tools::toTitleCase(level),
      y = if (horizontal) tools::toTitleCase(level) else "Number of Publications"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text = ggplot2::element_text(size = 10)
    )

  if (!horizontal) {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  }

  return(p)
}

#' Plot Trends
#'
#' @description
#' Creates line plots showing publication trends over time at different
#' organizational levels.
#'
#' @param data A data frame from \code{\link{prepare_cris_data}}.
#' @param level Character string. Organizational level to compare:
#'   \code{"faculty"}, \code{"department"}, or \code{"unit"}.
#' @param years Numeric vector. Years to include. If NULL, uses all years.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#' @param top_n Integer. Maximum number of items to display. Default is 8.
#' @param title Character string. Custom plot title. If NULL, auto-generated.
#' @param show_points Logical. If TRUE, adds points to lines. Default is TRUE.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' data <- prepare_cris_data("Data/")
#'
#' # Faculty trends
#' plot_trends(data, level = "faculty")
#'
#' # Top 5 department trends
#' plot_trends(data, level = "department", top_n = 5)
#'
#' # Unit trends for recent years
#' plot_trends(data, level = "unit", years = 2020:2024)
#' }
#'
#' @export
plot_trends <- function(data,
                        level = c("faculty", "department", "unit"),
                        years = NULL,
                        jufo = 1:3,
                        top_n = 8,
                        title = NULL,
                        show_points = TRUE) {

  level <- match.arg(level)

  # Validate level column exists
  level_col <- switch(level,
                      faculty = "faculty",
                      department = "department",
                      unit = "first_authors_unit")

  if (!(level_col %in% names(data))) {
    if (level %in% c("faculty", "department")) {
      stop("Column '", level_col, "' not found. Use prepare_cris_data() with add_hierarchy = TRUE")
    } else {
      stop("Column '", level_col, "' not found in data")
    }
  }

  if (!("publication_year" %in% names(data))) {
    stop("Column 'publication_year' not found in data")
  }

  # Apply filters
  filtered <- filter_by_criteria(data, years = years, jufo = jufo)

  # Remove NA values
  filtered <- filtered %>%
    dplyr::filter(!is.na(.data[[level_col]]),
                  !is.na(.data$publication_year))

  # Use abbreviation for units if available
  if (level == "unit" && "unit_abbrev" %in% names(filtered)) {
    filtered <- filtered %>%
      dplyr::mutate(plot_label = .data$unit_abbrev)
    label_col <- "plot_label"
  } else {
    label_col <- level_col
  }

  # Get top items by total publications
  top_items <- filtered %>%
    dplyr::group_by(.data[[label_col]]) %>%
    dplyr::summarise(total = dplyr::n_distinct(.data$publication_id), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$total)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::pull(.data[[label_col]])

  # Calculate yearly counts for top items
  plot_data <- filtered %>%
    dplyr::filter(.data[[label_col]] %in% top_items) %>%
    dplyr::group_by(.data[[label_col]], .data$publication_year) %>%
    dplyr::summarise(
      count = dplyr::n_distinct(.data$publication_id),
      .groups = "drop"
    ) %>%
    dplyr::rename(item = dplyr::all_of(label_col))

  # Build title
  if (is.null(title)) {
    year_str <- if (!is.null(years)) {
      paste0(" (", min(years), "-", max(years), ")")
    } else {
      ""
    }
    title <- paste0(tools::toTitleCase(level), " Publication Trends", year_str)
  }

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(
    x = .data$publication_year,
    y = .data$count,
    color = .data$item,
    group = .data$item
  )) +
    ggplot2::geom_line(linewidth = 1)

  if (show_points) {
    p <- p + ggplot2::geom_point(size = 2)
  }

  p <- p +
    ggplot2::labs(
      title = title,
      x = "Year",
      y = "Number of Publications",
      color = tools::toTitleCase(level)
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty()) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      legend.position = "right",
      axis.text = ggplot2::element_text(size = 10)
    )

  return(p)
}

#' Plot JUFO Distribution
#'
#' @description
#' Creates a stacked bar chart showing JUFO level distribution over time
#' or across organizational units.
#'
#' @param data A data frame from \code{\link{prepare_cris_data}}.
#' @param by Character string. What to show on x-axis: \code{"year"},
#'   \code{"faculty"}, \code{"department"}, or \code{"unit"}.
#' @param years Numeric vector. Years to include. If NULL, uses all years.
#' @param top_n Integer. For organizational levels, maximum items to show.
#'   Default is 15.
#' @param proportional Logical. If TRUE, shows proportions instead of counts.
#'   Default is FALSE.
#' @param title Character string. Custom plot title. If NULL, auto-generated.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' data <- prepare_cris_data("Data/")
#'
#' # JUFO distribution by year
#' plot_jufo_distribution(data, by = "year")
#'
#' # JUFO distribution by faculty (proportional)
#' plot_jufo_distribution(data, by = "faculty", proportional = TRUE)
#' }
#'
#' @export
plot_jufo_distribution <- function(data,
                                   by = c("year", "faculty", "department", "unit"),
                                   years = NULL,
                                   top_n = 15,
                                   proportional = FALSE,
                                   title = NULL) {

  by <- match.arg(by)

  # Determine the grouping column
  group_col <- switch(by,
                      year = "publication_year",
                      faculty = "faculty",
                      department = "department",
                      unit = "first_authors_unit")

  if (!(group_col %in% names(data))) {
    stop("Column '", group_col, "' not found in data")
  }

  # Apply year filter
  if (!is.null(years) && "publication_year" %in% names(data)) {
    data <- data %>%
      dplyr::filter(.data$publication_year %in% years)
  }

  # Use abbreviation for units if available
  if (by == "unit" && "unit_abbrev" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(plot_group = .data$unit_abbrev)
    label_col <- "plot_group"
  } else {
    label_col <- group_col
  }

  # Remove NA values
  data <- data %>%
    dplyr::filter(!is.na(.data[[label_col]]),
                  !is.na(.data$jufo_level_of_publication))

  # For non-year groupings, limit to top_n
  if (by != "year") {
    top_items <- data %>%
      dplyr::group_by(.data[[label_col]]) %>%
      dplyr::summarise(total = dplyr::n_distinct(.data$publication_id), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(.data$total)) %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::pull(.data[[label_col]])

    data <- data %>%
      dplyr::filter(.data[[label_col]] %in% top_items)
  }

  # Aggregate data
  plot_data <- data %>%
    dplyr::group_by(.data[[label_col]], .data$jufo_level_of_publication) %>%
    dplyr::summarise(
      count = dplyr::n_distinct(.data$publication_id),
      .groups = "drop"
    ) %>%
    dplyr::rename(group = dplyr::all_of(label_col)) %>%
    dplyr::mutate(
      jufo_level_of_publication = factor(.data$jufo_level_of_publication,
                                         levels = c("0", "1", "2", "3"))
    )

  # Calculate proportions if requested
  if (proportional) {
    plot_data <- plot_data %>%
      dplyr::group_by(.data$group) %>%
      dplyr::mutate(
        proportion = .data$count / sum(.data$count)
      ) %>%
      dplyr::ungroup()
    y_var <- "proportion"
    y_label <- "Proportion"
  } else {
    y_var <- "count"
    y_label <- "Number of Publications"
  }

  # Order groups
  if (by == "year") {
    plot_data <- plot_data %>%
      dplyr::mutate(group = as.numeric(as.character(.data$group)))
  } else {
    group_order <- plot_data %>%
      dplyr::group_by(.data$group) %>%
      dplyr::summarise(total = sum(.data$count), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(.data$total)) %>%
      dplyr::pull(.data$group)
    plot_data <- plot_data %>%
      dplyr::mutate(group = factor(.data$group, levels = group_order))
  }

  # Build title
  if (is.null(title)) {
    title <- paste0("JUFO Level Distribution by ", tools::toTitleCase(by))
  }

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(
    x = .data$group,
    y = .data[[y_var]],
    fill = .data$jufo_level_of_publication
  )) +
    ggplot2::geom_col(position = if (proportional) "fill" else "stack") +
    ggplot2::scale_fill_brewer(palette = "Blues", name = "JUFO Level") +
    ggplot2::labs(
      title = title,
      x = tools::toTitleCase(by),
      y = y_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  if (proportional) {
    p <- p + ggplot2::scale_y_continuous(labels = scales::percent)
  }

  return(p)
}

#' Create All Standard Plots
#'
#' @description
#' Generates a comprehensive set of standard visualization for quick analysis.
#'
#' @param data A data frame from \code{\link{prepare_cris_data}}.
#' @param years Numeric vector. Years to include. If NULL, uses all years.
#' @param jufo Numeric vector. JUFO levels to include. Default is \code{1:3}.
#'
#' @return A named list of ggplot2 objects:
#'   \describe{
#'     \item{faculty_comparison}{Bar chart comparing faculties}
#'     \item{department_comparison}{Bar chart comparing departments}
#'     \item{unit_comparison}{Bar chart comparing units}
#'     \item{faculty_trends}{Line plot of faculty trends}
#'     \item{jufo_by_year}{JUFO distribution by year}
#'     \item{jufo_by_faculty}{JUFO distribution by faculty}
#'   }
#'
#' @examples
#' \dontrun{
#' data <- prepare_cris_data("Data/")
#' plots <- create_all_plots(data, years = 2020:2024)
#'
#' # View individual plots
#' plots$faculty_comparison
#' plots$faculty_trends
#' }
#'
#' @export
create_all_plots <- function(data,
                             years = NULL,
                             jufo = 1:3) {

  plots <- list()

  # Faculty comparison
  if ("faculty" %in% names(data)) {
    plots$faculty_comparison <- plot_comparison(data, level = "faculty",
                                                 years = years, jufo = jufo)
    plots$faculty_trends <- plot_trends(data, level = "faculty",
                                         years = years, jufo = jufo)
    plots$jufo_by_faculty <- plot_jufo_distribution(data, by = "faculty",
                                                     years = years,
                                                     proportional = TRUE)
  }

  # Department comparison
  if ("department" %in% names(data)) {
    plots$department_comparison <- plot_comparison(data, level = "department",
                                                    years = years, jufo = jufo,
                                                    fill_by = "faculty")
  }

  # Unit comparison
  if ("first_authors_unit" %in% names(data)) {
    plots$unit_comparison <- plot_comparison(data, level = "unit",
                                              years = years, jufo = jufo,
                                              top_n = 20)
  }

  # JUFO by year
  if ("publication_year" %in% names(data)) {
    plots$jufo_by_year <- plot_jufo_distribution(data, by = "year",
                                                  years = years)
  }

  plots
}
