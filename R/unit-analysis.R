#' Plot Unit Productivity Growth
#'
#' @description
#' Analyzes and visualizes the publication productivity growth of university
#' units over time. Can filter by keyword to focus on specific units or
#' exclude them.
#'
#' @param data A data frame containing publication data with columns:
#'   \code{first_authors_unit}, \code{publication_year}, and
#'   \code{jufo_level_of_publication}.
#' @param n_years Integer. Number of recent years to analyze. Default is 5.
#' @param levels Numeric or character vector. JUFO levels to include.
#'   Default is \code{c(1, 2, 3)}.
#' @param top_n Integer. Number of top units to display. Default is 10.
#' @param keyword Character string or NULL. If provided, filters units by
#'   this keyword. Default is NULL (all units).
#' @param keep_keyword_units Logical. If TRUE (default) and keyword is
#'   provided, keeps only units containing the keyword. If FALSE, excludes
#'   units containing the keyword.
#'
#' @return A list containing:
#'   \describe{
#'     \item{growth_plot}{ggplot bar chart of average growth rates}
#'     \item{counts_plot}{ggplot grouped bar chart of publication counts}
#'     \item{growth_line_plot}{ggplot line chart of yearly growth rates}
#'   }
#'
#' @details
#' Growth rate is calculated as the percentage change from the previous year:
#' \deqn{growth = \frac{current - previous}{previous} \times 100}
#'
#' Units with zero publications in the previous year have their growth rate
#' set to NA for that transition.
#'
#' @examples
#' \dontrun{
#' # Basic analysis of top 10 units
#' plots <- plot_unit_growth(cleaned_data)
#' print(plots$counts_plot)
#'
#' # Focus on education-related units
#' plots <- plot_unit_growth(
#'   cleaned_data,
#'   keyword = "Education",
#'   n_years = 4,
#'   top_n = 5
#' )
#'
#' # Exclude administration units
#' plots <- plot_unit_growth(
#'   cleaned_data,
#'   keyword = "Admin",
#'   keep_keyword_units = FALSE
#' )
#' }
#'
#' @export
plot_unit_growth <- function(data,
                              n_years = 5,
                              levels = c(1, 2, 3),
                              top_n = 10,
                              keyword = NULL,
                              keep_keyword_units = TRUE) {

  # Input Validation
  required_cols <- c("first_authors_unit", "publication_year", "jufo_level_of_publication")
  validate_cris_data(data, required_cols, "plot_unit_growth")

  if (n_years <= 0) {
    stop("'n_years' must be a positive integer.")
  }
  if (!is.null(keyword) && !is.character(keyword)) {
    stop("'keyword' must be a character string.")
  }

  # Data Preparation
  # Determine the last n years present in the data
  recent_years <- sort(unique(data$publication_year), decreasing = TRUE)[
    1:min(n_years, length(unique(data$publication_year)))
  ]
  if (length(recent_years) < 2) {
    stop("Not enough years of data to calculate growth. Need at least 2 years.")
  }

  # Filter by levels and recent years
  filtered_data <- data %>%
    dplyr::filter(
      .data$jufo_level_of_publication %in% levels,
      .data$publication_year %in% recent_years
    )

  # Split the 'first_authors_unit' string
  unit_data <- filtered_data %>%
    dplyr::mutate(unit_list = stringr::str_split(.data$first_authors_unit, ";\\s*")) %>%
    tidyr::unnest(.data$unit_list) %>%
    dplyr::mutate(unit_list = trimws(.data$unit_list))

  # Keyword-Based Filtering/Aggregation
  if (!is.null(keyword)) {
    if (keep_keyword_units) {
      # Keep only units containing the keyword, and group them
      unit_data <- unit_data %>%
        dplyr::mutate(
          unit_group = ifelse(
            stringr::str_detect(.data$unit_list, stringr::regex(keyword, ignore_case = TRUE)),
            paste0("Group_", keyword),
            NA_character_
          )
        ) %>%
        dplyr::filter(!is.na(.data$unit_group))
    } else {
      # Keep only units NOT containing the keyword
      unit_data <- unit_data %>%
        dplyr::filter(!stringr::str_detect(
          .data$unit_list,
          stringr::regex(keyword, ignore_case = TRUE)
        )) %>%
        dplyr::mutate(unit_group = .data$unit_list)
    }
  } else {
    unit_data <- unit_data %>%
      dplyr::mutate(unit_group = .data$unit_list)
  }

  if (nrow(unit_data) == 0) {
    stop("No data after filtering on the keyword.")
  }

  # Calculate Yearly Publication Counts
  unit_counts <- unit_data %>%
    dplyr::group_by(.data$publication_year, .data$unit_group) %>%
    dplyr::summarise(publication_count = dplyr::n(), .groups = "drop")

  # Identify Top N Units Overall
  top_units <- unit_counts %>%
    dplyr::group_by(.data$unit_group) %>%
    dplyr::summarise(total_publications = sum(.data$publication_count), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$total_publications)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::pull(.data$unit_group)

  # Filter to include only top units
  top_unit_counts <- unit_counts %>%
    dplyr::filter(.data$unit_group %in% top_units)

  # Create Publication Counts Plot
  counts_plot <- ggplot2::ggplot(
    top_unit_counts,
    ggplot2::aes(
      x = as.factor(.data$publication_year),
      y = .data$publication_count,
      fill = .data$unit_group
    )
  ) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(
      title = paste("Publication Counts of Top", top_n, "Units (Last", n_years, "Years)"),
      x = "Year",
      y = "Number of Publications",
      fill = "Unit"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = ceiling(top_n / 3)))

  # Get all combinations of top unit and year
  all_combinations <- expand.grid(
    unit_group = top_units,
    publication_year = recent_years,
    stringsAsFactors = FALSE
  )

  # Merge with actual data, filling missing with 0
  top_unit_counts_complete <- dplyr::left_join(
    all_combinations, top_unit_counts,
    by = c("unit_group", "publication_year")
  ) %>%
    dplyr::mutate(
      publication_count = ifelse(is.na(.data$publication_count), 0, .data$publication_count)
    )

  # Calculate growth rates
  growth_rates <- top_unit_counts_complete %>%
    dplyr::group_by(.data$unit_group) %>%
    dplyr::arrange(.data$publication_year) %>%
    dplyr::mutate(
      previous_count = dplyr::lag(.data$publication_count),
      growth_rate = ifelse(
        .data$previous_count > 0,
        (.data$publication_count - .data$previous_count) / .data$previous_count * 100,
        NA_real_
      )
    ) %>%
    dplyr::filter(!is.na(.data$growth_rate)) %>%
    dplyr::summarise(avg_growth_rate = mean(.data$growth_rate, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$avg_growth_rate))

  # Create Growth Rate Plot
  growth_plot <- ggplot2::ggplot(
    growth_rates,
    ggplot2::aes(x = stats::reorder(.data$unit_group, .data$avg_growth_rate),
                 y = .data$avg_growth_rate)
  ) +
    ggplot2::geom_bar(stat = "identity", fill = "seagreen") +
    ggplot2::labs(
      title = paste("Top Growing of Top", top_n, "Units by Average Growth Rate (Last", n_years, "Years)"),
      x = "Unit",
      y = "Average Growth Rate (%)"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::coord_flip()

  # Create Growth Line Plot
  top_unit_growth <- top_unit_counts_complete %>%
    dplyr::group_by(.data$unit_group, .data$publication_year) %>%
    dplyr::summarise(publication_count = sum(.data$publication_count), .groups = "drop") %>%
    dplyr::group_by(.data$unit_group) %>%
    dplyr::arrange(.data$publication_year) %>%
    dplyr::mutate(
      previous_count = dplyr::lag(.data$publication_count),
      growth_rate = ifelse(
        .data$previous_count > 0,
        (.data$publication_count - .data$previous_count) / .data$previous_count * 100,
        NA_real_
      )
    ) %>%
    dplyr::filter(!is.na(.data$growth_rate))

  growth_line_plot <- ggplot2::ggplot(
    top_unit_growth,
    ggplot2::aes(x = .data$publication_year, y = .data$growth_rate, color = .data$unit_group)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = paste("Yearly Growth Rates of Top", top_n, "Units (Last", n_years, "Years)"),
      x = "Year",
      y = "Growth Rate (%)",
      color = "Unit"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom"
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = ceiling(top_n / 3)))

  return(list(
    growth_plot = growth_plot,
    counts_plot = counts_plot,
    growth_line_plot = growth_line_plot
  ))
}
