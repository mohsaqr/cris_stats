#' Compute Fractional Author Counts
#'
#' @description
#' Calculates fractional author contributions using various counting methods.
#' These methods weight author contributions differently based on position,
#' number of authors, or other criteria.
#'
#' @param data A data frame with publication data containing \code{publication_id}
#'   and \code{Authorsclean} columns.
#' @param counting_methods Character vector specifying which counting methods
#'   to use. Options are:
#'   \describe{
#'     \item{"standard"}{Equal fractional share (1/n for n authors)}
#'     \item{"harmonic"}{Weights based on 1/position, normalized}
#'     \item{"proportional"}{Weights decrease linearly by position}
#'     \item{"first_last"}{Extra weight for first and last authors}
#'     \item{"geometric"}{Weights decay exponentially (0.5^position)}
#'     \item{"position_weighted"}{Custom weights for specific positions}
#'   }
#'   Default includes all methods.
#' @param first_last_weight Numeric. Weight multiplier for first and last
#'   authors when using the "first_last" method. Default is 2.
#' @param position_weights Numeric vector. Weights for positions 1, 2, 3, etc.
#'   when using "position_weighted" method. Authors beyond the specified
#'   positions receive the last weight. Default is \code{c(1, 0.8, 0.6, 0.4)}.
#'
#' @return A data frame with one row per author containing the fractional
#'   counts for each requested method:
#'   \describe{
#'     \item{Authorsclean}{Author name}
#'     \item{Standard_Fractional}{Standard fractional count (if requested)}
#'     \item{Harmonic_Fractional}{Harmonic fractional count (if requested)}
#'     \item{Proportional_Fractional}{Proportional count (if requested)}
#'     \item{FirstLast_Fractional}{First-last weighted count (if requested)}
#'     \item{Geometric_Fractional}{Geometric weighted count (if requested)}
#'     \item{PositionWeighted_Fractional}{Position weighted count (if requested)}
#'   }
#'
#' @details
#' ## Counting Methods
#'
#' **Standard**: Each author receives 1/n credit for a paper with n authors.
#'
#' **Harmonic**: Authors are weighted by 1/position (first author gets 1,
#' second gets 1/2, etc.), then normalized to sum to 1.
#'
#' **Proportional**: Authors are weighted by (n - position + 1) / sum(1:n),
#' giving more credit to earlier positions.
#'
#' **First-Last**: First and last authors receive \code{first_last_weight / n}
#' credit, while middle authors receive proportional fractional credit.
#'
#' **Geometric**: Weights decay as 0.5^(position-1), then normalized.
#'
#' **Position Weighted**: Custom weights specified by \code{position_weights},
#' normalized per paper.
#'
#' @examples
#' \dontrun{
#' # Compute all fractional counts
#' frac <- compute_fractional_counts(cleaned_data)
#'
#' # Compute only standard and harmonic
#' frac <- compute_fractional_counts(
#'   cleaned_data,
#'   counting_methods = c("standard", "harmonic")
#' )
#'
#' # Custom position weights
#' frac <- compute_fractional_counts(
#'   cleaned_data,
#'   counting_methods = "position_weighted",
#'   position_weights = c(1.0, 0.9, 0.7, 0.5, 0.3)
#' )
#' }
#'
#' @export
compute_fractional_counts <- function(data,
                                       counting_methods = c("standard", "harmonic", "proportional",
                                                            "first_last", "geometric", "position_weighted"),
                                       first_last_weight = 2,
                                       position_weights = c(1, 0.8, 0.6, 0.4)) {
  # Validate input
  validate_cris_data(
    data,
    required_cols = c("publication_id", "Authorsclean"),
    func_name = "compute_fractional_counts"
  )

  # Prepare base data with author positions
  base_data <- data %>%
    dplyr::select(.data$publication_id, .data$Authorsclean) %>%
    tidyr::separate_rows(.data$Authorsclean, sep = ";\\s*") %>%
    dplyr::mutate(Authorsclean = trimws(.data$Authorsclean)) %>%
    dplyr::group_by(.data$publication_id) %>%
    dplyr::mutate(
      total_authors = dplyr::n(),
      position = dplyr::row_number(),
      is_first = .data$position == 1,
      is_last = .data$position == .data$total_authors
    ) %>%
    dplyr::ungroup()

  fractional_counts <- list()

  # Standard fractional counting (1/n)
  if ("standard" %in% counting_methods) {
    standard_counts <- base_data %>%
      dplyr::group_by(.data$Authorsclean) %>%
      dplyr::summarise(
        Standard_Fractional = sum(1 / .data$total_authors),
        .groups = "drop"
      )
    fractional_counts$standard <- standard_counts
  }

  # Harmonic counting (1/position, normalized)
  if ("harmonic" %in% counting_methods) {
    harmonic_counts <- base_data %>%
      dplyr::group_by(.data$publication_id) %>%
      dplyr::mutate(
        harmonic_weight = 1 / .data$position,
        normalized_weight = .data$harmonic_weight / sum(.data$harmonic_weight)
      ) %>%
      dplyr::group_by(.data$Authorsclean) %>%
      dplyr::summarise(
        Harmonic_Fractional = sum(.data$normalized_weight),
        .groups = "drop"
      )
    fractional_counts$harmonic <- harmonic_counts
  }

  # Proportional counting (linear decrease by position)
  if ("proportional" %in% counting_methods) {
    proportional_counts <- base_data %>%
      dplyr::group_by(.data$publication_id) %>%
      dplyr::mutate(
        prop_weight = (.data$total_authors - .data$position + 1) / sum(1:.data$total_authors[1])
      ) %>%
      dplyr::group_by(.data$Authorsclean) %>%
      dplyr::summarise(
        Proportional_Fractional = sum(.data$prop_weight),
        .groups = "drop"
      )
    fractional_counts$proportional <- proportional_counts
  }

  # First-last weighted counting
  if ("first_last" %in% counting_methods) {
    first_last_counts <- base_data %>%
      dplyr::group_by(.data$publication_id) %>%
      dplyr::mutate(
        prop_weight = (.data$total_authors - .data$position + 1) / sum(1:.data$total_authors[1]),
        weight = dplyr::case_when(
          .data$is_first | .data$is_last ~ first_last_weight / .data$total_authors,
          TRUE ~ .data$prop_weight / .data$total_authors
        )
      ) %>%
      dplyr::group_by(.data$Authorsclean) %>%
      dplyr::summarise(
        FirstLast_Fractional = sum(.data$weight),
        .groups = "drop"
      )
    fractional_counts$first_last <- first_last_counts
  }

  # Geometric counting (0.5^position, normalized)
  if ("geometric" %in% counting_methods) {
    geometric_counts <- base_data %>%
      dplyr::group_by(.data$publication_id) %>%
      dplyr::mutate(
        geo_weight = 0.5^(.data$position - 1),
        normalized_geo_weight = .data$geo_weight / sum(.data$geo_weight)
      ) %>%
      dplyr::group_by(.data$Authorsclean) %>%
      dplyr::summarise(
        Geometric_Fractional = sum(.data$normalized_geo_weight),
        .groups = "drop"
      )
    fractional_counts$geometric <- geometric_counts
  }

  # Position weighted counting (custom weights)
  if ("position_weighted" %in% counting_methods) {
    position_weighted_counts <- base_data %>%
      dplyr::group_by(.data$publication_id) %>%
      dplyr::mutate(
        pos_weight = dplyr::case_when(
          .data$position <= length(position_weights) ~ position_weights[.data$position],
          TRUE ~ position_weights[length(position_weights)]
        ),
        normalized_pos_weight = .data$pos_weight / sum(.data$pos_weight)
      ) %>%
      dplyr::group_by(.data$Authorsclean) %>%
      dplyr::summarise(
        PositionWeighted_Fractional = sum(.data$normalized_pos_weight),
        .groups = "drop"
      )
    fractional_counts$position_weighted <- position_weighted_counts
  }

  # Combine all computed methods
  if (length(fractional_counts) > 0) {
    result <- fractional_counts[[1]]
    if (length(fractional_counts) > 1) {
      for (i in 2:length(fractional_counts)) {
        result <- result %>%
          dplyr::full_join(fractional_counts[[i]], by = "Authorsclean")
      }
    }
    return(result)
  } else {
    return(data.frame(Authorsclean = character(0)))
  }
}
