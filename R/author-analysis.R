#' Analyze Individual Author Publications
#'
#' @description
#' Analyzes a single author's publication history over a specified time period.
#' Generates a publication trend plot and returns a list of the author's
#' publications.
#'
#' @param data A data frame containing publication data with columns:
#'   \code{Authorsclean}, \code{publication_year}, \code{publication_id},
#'   \code{title_of_publication}, and \code{jufo_level_of_publication}.
#' @param author_name Character string. The author's name to search for.
#'   The search is case-insensitive and matches partial names.
#' @param year_range Numeric vector. The range of years to analyze,
#'   e.g., \code{2019:2024}. If NULL (default), includes all years.
#'
#' @return A list containing:
#'   \describe{
#'     \item{plot}{ggplot object showing the author's publication trend}
#'     \item{articles}{Data frame listing the author's publications}
#'   }
#'   Returns NULL if no articles are found for the author.
#'
#' @examples
#' \dontrun{
#' # Analyze an author's publications
#' result <- analyze_author_publications(cleaned_data, "SMITH JOHN")
#'
#' # View the plot
#' print(result$plot)
#'
#' # View the article list
#' View(result$articles)
#'
#' # Analyze for specific years
#' result <- analyze_author_publications(
#'   cleaned_data,
#'   author_name = "SMITH JOHN",
#'   year_range = 2020:2024
#' )
#' }
#'
#' @export
analyze_author_publications <- function(data, author_name, year_range = NULL) {

  # Input Validation
  required_cols <- c("Authorsclean", "publication_year", "publication_id",
                     "title_of_publication", "jufo_level_of_publication")
  validate_cris_data(data, required_cols, "analyze_author_publications")

  if (!is.character(author_name) || length(author_name) != 1 ||
      nchar(trimws(author_name)) == 0) {
    stop("'author_name' must be a single, non-empty string.")
  }

  if (!is.null(year_range) && !is.numeric(year_range)) {
    stop("'year_range' must be a numeric vector (e.g., 2019:2024).")
  }

  # Filter for the Author's Articles
  author_articles <- data %>%
    dplyr::filter(stringr::str_detect(
      .data$Authorsclean,
      stringr::regex(paste0("\\b", author_name, "\\b"), ignore_case = TRUE)
    ))

  # Apply year range filter if specified
  if (!is.null(year_range)) {
    author_articles <- author_articles %>%
      dplyr::filter(.data$publication_year %in% year_range)
  }

  if (nrow(author_articles) == 0) {
    warning(paste("No articles found for author:", author_name,
                  "within the specified criteria."))
    return(NULL)
  }

  # Extract and Prepare the List of Articles
  article_list <- author_articles %>%
    dplyr::select(dplyr::all_of(required_cols)) %>%
    dplyr::arrange(dplyr::desc(.data$publication_year))

  # Create the Publication Plot
  yearly_counts <- author_articles %>%
    dplyr::group_by(.data$publication_year) %>%
    dplyr::summarise(n_articles = dplyr::n(), .groups = "drop")

  # Define the full sequence of years for the plot's x-axis
  plot_years <- if (!is.null(year_range)) {
    min(year_range):max(year_range)
  } else {
    min(yearly_counts$publication_year):max(yearly_counts$publication_year)
  }
  all_years_df <- data.frame(publication_year = plot_years)

  plot_data <- dplyr::left_join(all_years_df, yearly_counts,
                                 by = "publication_year") %>%
    dplyr::mutate(n_articles = tidyr::replace_na(.data$n_articles, 0))

  # Create dynamic subtitle
  subtitle_text <- if (!is.null(year_range)) {
    paste("Period:", min(year_range), "-", max(year_range),
          "| Total publications:", nrow(author_articles))
  } else {
    paste("Total publications found:", nrow(author_articles))
  }

  publication_plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$publication_year, y = .data$n_articles)
  ) +
    ggplot2::geom_line(color = "#0072B2", linewidth = 1) +
    ggplot2::geom_point(color = "#0072B2", size = 3) +
    ggplot2::geom_text(
      data = dplyr::filter(plot_data, .data$n_articles > 0),
      ggplot2::aes(label = .data$n_articles),
      vjust = -1.5, size = 3.5
    ) +
    ggplot2::labs(
      title = paste("Annual Publication Count for", author_name),
      subtitle = subtitle_text,
      x = "Year",
      y = "Number of Articles"
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(1)) +
    ggplot2::scale_y_continuous(
      limits = c(0, max(plot_data$n_articles) * 1.2),
      breaks = scales::breaks_pretty()
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      panel.grid.minor.x = ggplot2::element_blank()
    )

  return(list(
    plot = publication_plot,
    articles = article_list
  ))
}
