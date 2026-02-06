#' Generate Publication Statistics
#'
#' @description
#' Calculates basic publication statistics and generates visualizations
#' including articles per year and JUFO level distributions over time.
#'
#' @param data A data frame containing publication data with columns:
#'   \code{publication_id}, \code{Authorsclean}, \code{publication_year},
#'   and \code{jufo_level_of_publication}.
#'
#' @return A list containing:
#'   \describe{
#'     \item{n_articles}{Total number of unique articles}
#'     \item{avg_articles_per_author}{Average articles per author}
#'     \item{articles_per_year_barplot}{ggplot bar chart of articles per year}
#'     \item{jufo_level_per_year_plot}{ggplot line chart of JUFO levels by year}
#'   }
#'
#' @examples
#' \dontrun{
#' stats <- generate_publication_stats(cleaned_data)
#'
#' # View number of articles
#' stats$n_articles
#'
#' # Display the bar plot
#' print(stats$articles_per_year_barplot)
#'
#' # Display JUFO level trends
#' print(stats$jufo_level_per_year_plot)
#' }
#'
#' @export
generate_publication_stats <- function(data) {

  # Validate input
  required_cols <- c("publication_id", "Authorsclean", "publication_year",
                     "jufo_level_of_publication")
  validate_cris_data(data, required_cols, "generate_publication_stats")

  # Create unique publications dataset
  temp_data <- data %>%
    dplyr::distinct(.data$publication_id, .keep_all = TRUE)

  # 1. Number of Articles (Unique)
  n_articles <- nrow(temp_data)

  # 2. Average Number of Articles Per Author
  author_counts <- temp_data %>%
    tidyr::separate_rows(.data$Authorsclean, sep = ";\\s*") %>%
    dplyr::mutate(Authorsclean = trimws(.data$Authorsclean)) %>%
    dplyr::group_by(.data$Authorsclean) %>%
    dplyr::summarise(n_articles = dplyr::n(), .groups = "drop")

  avg_articles_per_author <- mean(author_counts$n_articles)

  # 3. Bar Plot of Articles Published Per Year
  articles_per_year <- temp_data %>%
    dplyr::group_by(.data$publication_year) %>%
    dplyr::summarise(n_articles = dplyr::n(), .groups = "drop")

  articles_per_year_barplot <- ggplot2::ggplot(
    articles_per_year,
    ggplot2::aes(x = as.factor(.data$publication_year), y = .data$n_articles)
  ) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::labs(
      title = "Articles Published Per Year",
      x = "Publication Year",
      y = "Number of Articles"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # 4. Plot JUFO Level Per Year
  jufo_level_per_year <- temp_data %>%
    dplyr::group_by(.data$publication_year, .data$jufo_level_of_publication) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  jufo_level_per_year_plot <- ggplot2::ggplot(
    jufo_level_per_year,
    ggplot2::aes(
      x = as.factor(.data$publication_year),
      y = .data$n,
      color = as.factor(.data$jufo_level_of_publication),
      group = .data$jufo_level_of_publication
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = "JUFO Level of Publication Per Year",
      x = "Publication Year",
      y = "Number of Articles",
      color = "JUFO Level"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  list(
    n_articles = n_articles,
    avg_articles_per_author = avg_articles_per_author,
    articles_per_year_barplot = articles_per_year_barplot,
    jufo_level_per_year_plot = jufo_level_per_year_plot
  )
}
