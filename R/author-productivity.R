#' Generate Author Productivity Tables
#'
#' @description
#' Comprehensive analysis of author productivity including multiple fractional
#' counting methods, position analysis, and publication level breakdowns.
#' Generates formatted gt tables for each analysis type.
#'
#' @param data A data frame containing cleaned publication data with columns:
#'   \code{publication_id}, \code{Authorsclean}, \code{publication_year},
#'   \code{jufo_level_of_publication}, \code{first_author}.
#' @param years Numeric vector or single year. If NULL (default), uses the
#'   most recent year in the data.
#' @param top_n Integer. Number of top authors to display in tables.
#'   Default is 30.
#' @param exclude_et_al Logical. If TRUE, excludes entries where author name
#'   is "ET AL". Default is TRUE.
#' @param levels Character vector of JUFO levels to include. Default is
#'   \code{c("1", "2", "3")}.
#' @param counting_methods Character vector of fractional counting methods.
#'   See \code{\link{compute_fractional_counts}} for options.
#' @param first_last_weight Numeric. Weight for first/last author method.
#'   Default is 2.
#' @param position_weights Numeric vector. Weights for position-weighted method.
#'   Default is \code{c(1, 0.8, 0.6, 0.4)}.
#'
#' @return A list containing:
#'   \describe{
#'     \item{most_productive_overall}{gt table of most productive authors}
#'     \item{most_productive_by_level}{List of gt tables by JUFO level}
#'     \item{detailed_productivity}{gt table with level breakdown}
#'     \item{author_positions}{gt table of author position distribution}
#'     \item{fractional_contributions}{gt table of fractional counts}
#'     \item{comprehensive_analysis}{gt table combining all metrics}
#'     \item{raw_data}{Data frame with all computed metrics}
#'     \item{counting_methods_used}{Methods used in analysis}
#'     \item{data_filtered}{Filtered input data}
#'     \item{fractional_counts}{Fractional count data}
#'     \item{position_counts}{Position analysis data}
#'     \item{first_author_counts}{First author counts}
#'     \item{authors_by_level}{Author counts by level}
#'     \item{first_authors_unit_counts}{First author unit counts}
#'     \item{active_years_counts}{Publication years per author}
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic usage with current year
#' results <- generate_author_productivity_tables(cleaned_data)
#'
#' # Analyze multiple years
#' results <- generate_author_productivity_tables(
#'   cleaned_data,
#'   years = 2020:2024,
#'   top_n = 50
#' )
#'
#' # Use specific counting methods
#' results <- generate_author_productivity_tables(
#'   cleaned_data,
#'   counting_methods = c("standard", "harmonic")
#' )
#'
#' # View comprehensive table
#' results$comprehensive_analysis
#'
#' # Access raw data for further analysis
#' head(results$raw_data)
#' }
#'
#' @export
generate_author_productivity_tables <- function(data,
                                                 years = NULL,
                                                 top_n = 30,
                                                 exclude_et_al = TRUE,
                                                 levels = c("1", "2", "3"),
                                                 counting_methods = c("standard", "harmonic", "proportional",
                                                                      "first_last", "geometric", "position_weighted"),
                                                 first_last_weight = 2,
                                                 position_weights = c(1, 0.8, 0.6, 0.4)) {

  # Input data validation
  required_cols <- c("publication_id", "Authorsclean", "publication_year",
                     "jufo_level_of_publication", "first_author")
  validate_cris_data(data, required_cols, "generate_author_productivity_tables")

  # Data type conversion for numeric columns
  numeric_cols <- c("Level1", "Level2", "Level3", "Total_2n3",
                    "Total_1n2n3", "First_Author_Count")
  data <- convert_to_numeric(data, numeric_cols)

  if ("FirstLast_Fractional" %in% names(data) &&
      is.character(data$FirstLast_Fractional)) {
    data$FirstLast_Fractional <- as.numeric(data$FirstLast_Fractional)
  }

  # Convert counting_methods to vector if single string
  if (is.character(counting_methods) && length(counting_methods) == 1) {
    counting_methods <- c(counting_methods)
  }

  # Main data preparation
  if (is.null(years)) {
    years <- max(data$publication_year)
  }

  # Filter data based on years
  if (length(years) == 1) {
    filtered_data <- data %>% dplyr::filter(.data$publication_year == years)
    subtitle_suffix <- paste("Year", years)
  } else {
    filtered_data <- data %>% dplyr::filter(.data$publication_year %in% years)
    subtitle_suffix <- paste("Years", min(years), "to", max(years))
  }

  # Exclude 'ET AL' if specified
  if (exclude_et_al) {
    filtered_data <- filtered_data %>% dplyr::filter(.data$Authorsclean != "ET AL")
  }

  # Filter by JUFO levels
  filtered_data <- filtered_data %>%
    dplyr::mutate(jufo_level_of_publication = as.character(.data$jufo_level_of_publication)) %>%
    dplyr::filter(.data$jufo_level_of_publication %in% levels)

  # Calculate all metrics using exported function
  fractional_counts <- compute_fractional_counts(
    filtered_data, counting_methods, first_last_weight, position_weights
  )

  # Calculate positions and author counts
  position_counts <- filtered_data %>%
    dplyr::select(.data$publication_id, .data$Authorsclean) %>%
    tidyr::separate_rows(.data$Authorsclean, sep = ";\\s*") %>%
    dplyr::mutate(Authorsclean = trimws(.data$Authorsclean)) %>%
    dplyr::group_by(.data$publication_id) %>%
    dplyr::mutate(
      total_authors = dplyr::n(),
      Rank = dplyr::row_number(),
      Position = dplyr::case_when(
        .data$Rank == 1 ~ "First",
        .data$Rank == 2 ~ "Second",
        .data$Rank == .data$total_authors & .data$Rank > 2 ~ "Last",
        TRUE ~ "Other"
      )
    ) %>%
    dplyr::ungroup()

  first_author_counts <- position_counts %>%
    dplyr::filter(.data$Position == "First") %>%
    dplyr::group_by(.data$Authorsclean) %>%
    dplyr::summarise(
      First_Author_Count = dplyr::n(),
      .groups = "drop"
    )

  # Calculate first_authors_unit count per unique publication_id
  first_authors_unit_counts <- filtered_data %>%
    dplyr::group_by(.data$publication_id, .data$first_author) %>%
    dplyr::summarise(Count = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(.data$first_author) %>%
    dplyr::summarise(
      First_Authors_Unit_Count = sum(.data$Count),
      .groups = "drop"
    ) %>%
    dplyr::rename(Authorsclean = .data$first_author)

  # Calculate level counts
  authors_by_level <- filtered_data %>%
    janitor::tabyl(.data$Authorsclean, .data$jufo_level_of_publication) %>%
    datawizard::data_addprefix("Level") %>%
    dplyr::rename(Author = .data$LevelAuthorsclean)

  # Initialize columns if they don't exist
  for (level in levels) {
    col_name <- paste0("Level", level)
    if (!(col_name %in% names(authors_by_level))) {
      authors_by_level[[col_name]] <- 0
    }
  }

  # Calculate totals
  authors_by_level <- authors_by_level %>%
    dplyr::mutate(
      Total = rowSums(dplyr::across(dplyr::starts_with("Level")), na.rm = TRUE),
      Total_2n3 = rowSums(dplyr::select(., dplyr::any_of(c("Level2", "Level3"))), na.rm = TRUE),
      Total_1n2n3 = rowSums(dplyr::select(., dplyr::any_of(c("Level1", "Level2", "Level3"))), na.rm = TRUE)
    )

  # Calculate number of active years for each author
  active_years_counts <- filtered_data %>%
    dplyr::select(.data$Authorsclean, .data$publication_year) %>%
    tidyr::separate_rows(.data$Authorsclean, sep = ";\\s*") %>%
    dplyr::mutate(Authorsclean = trimws(.data$Authorsclean)) %>%
    dplyr::group_by(.data$Authorsclean) %>%
    dplyr::summarise(
      active_years = dplyr::n_distinct(.data$publication_year),
      .groups = "drop"
    )

  # Create comprehensive dataset
  comprehensive_data <- authors_by_level %>%
    dplyr::left_join(fractional_counts, by = c("Author" = "Authorsclean")) %>%
    dplyr::left_join(first_author_counts, by = c("Author" = "Authorsclean")) %>%
    dplyr::left_join(first_authors_unit_counts, by = c("Author" = "Authorsclean")) %>%
    dplyr::left_join(active_years_counts, by = c("Author" = "Authorsclean")) %>%
    tidyr::replace_na(list(First_Author_Count = 0, First_Authors_Unit_Count = 0, active_years = 0)) %>%
    dplyr::mutate(
      Articles_Per_Year_Active = ifelse(.data$active_years > 0,
                                        .data$Total_1n2n3 / .data$active_years, 0)
    )

  # Data type correction
  numeric_cols <- c("Level1", "Level2", "Level3", "Total_2n3", "Total_1n2n3",
                    "First_Author_Count", "First_Authors_Unit_Count", "Articles_Per_Year_Active")
  comprehensive_data <- convert_to_numeric(comprehensive_data, numeric_cols)

  if ("FirstLast_Fractional" %in% names(comprehensive_data) &&
      is.character(comprehensive_data$FirstLast_Fractional)) {
    comprehensive_data$FirstLast_Fractional <- as.numeric(comprehensive_data$FirstLast_Fractional)
  }

  # Sort the data
  has_firstlast <- "FirstLast_Fractional" %in% names(comprehensive_data)
  comprehensive_data <- comprehensive_data %>%
    dplyr::arrange(dplyr::desc(.data$Total_1n2n3),
                   dplyr::desc(if (has_firstlast) .data$FirstLast_Fractional else .data$Total_1n2n3))

  # Create labels mapping
  labels_map <- list(
    Standard_Fractional = "Standard Frac.",
    Harmonic_Fractional = "Harmonic Frac.",
    Proportional_Fractional = "Prop. Frac.",
    FirstLast_Fractional = "First-Last Frac.",
    Geometric_Fractional = "Geometric Frac.",
    PositionWeighted_Fractional = "Pos-Weight Frac."
  )

  selected_labels <- labels_map[intersect(names(labels_map),
                                          paste0(tools::toTitleCase(counting_methods), "_Fractional"))]

  # Create tables
  select_cols <- names(comprehensive_data)
  select_cols <- select_cols[select_cols != "Total"]

  # Adjust column order
  base_order <- c("Author", "Total_1n2n3", "Articles_Per_Year_Active",
                  "First_Author_Count", "First_Authors_Unit_Count")
  other_cols <- setdiff(select_cols, c(base_order, "active_years"))
  select_cols <- c(base_order, other_cols)

  # Comprehensive analysis table
  p7 <- comprehensive_data %>%
    dplyr::select(dplyr::all_of(select_cols)) %>%
    dplyr::slice_head(n = top_n) %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::html("Comprehensive Author Productivity Analysis"),
      subtitle = gt::html(paste("Period:", subtitle_suffix))
    ) %>%
    gt::fmt_number(
      columns = c(dplyr::ends_with("_Fractional"), .data$Articles_Per_Year_Active),
      decimals = 2
    ) %>%
    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%
    gt::cols_label(
      First_Author_Count = "First Author",
      First_Authors_Unit_Count = "First Authors Unit",
      Articles_Per_Year_Active = gt::html("Articles /<br>Active Year"),
      !!!selected_labels
    )

  # Most Productive Authors Overall
  authors_overall <- filtered_data %>%
    janitor::tabyl(.data$Authorsclean) %>%
    dplyr::arrange(dplyr::desc(.data$n)) %>%
    dplyr::mutate(Order = dplyr::row_number(), .before = 1L)

  p1 <- authors_overall[1:top_n, ] %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::html("Most Productive Authors"),
      subtitle = gt::html(paste("Period:", subtitle_suffix))
    )

  # Level-Specific Productivity
  level_tables <- lapply(levels, function(level) {
    authors_level <- filtered_data %>%
      dplyr::filter(.data$jufo_level_of_publication == level) %>%
      janitor::tabyl(.data$Authorsclean) %>%
      dplyr::arrange(dplyr::desc(.data$n))

    authors_level[1:top_n, ] %>%
      gt::gt() %>%
      gt::tab_header(
        title = gt::html(paste("Level", level, "Authors")),
        subtitle = gt::html(paste("Period:", subtitle_suffix))
      )
  })

  # Detailed Productivity by Levels
  authors_itemized <- filtered_data %>%
    janitor::tabyl(.data$Authorsclean, .data$jufo_level_of_publication) %>%
    datawizard::data_addprefix("Level") %>%
    dplyr::rename(Author = .data$LevelAuthorsclean) %>%
    dplyr::mutate(
      Total = rowSums(dplyr::across(dplyr::starts_with("Level")), na.rm = TRUE),
      Total_2n3 = rowSums(dplyr::select(., dplyr::any_of(c("Level2", "Level3"))), na.rm = TRUE),
      Total_1n2n3 = rowSums(dplyr::select(., dplyr::any_of(c("Level1", "Level2", "Level3"))), na.rm = TRUE)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$Total_1n2n3))

  p4 <- authors_itemized[1:top_n, ] %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::html("Detailed Productivity by Levels"),
      subtitle = gt::html(paste("Period:", subtitle_suffix))
    )

  # Author Position Analysis
  process_positions <- function(df) {
    df %>%
      dplyr::select(.data$publication_id, .data$Authorsclean) %>%
      tidyr::separate_rows(.data$Authorsclean, sep = ";\\s*") %>%
      dplyr::mutate(Authorsclean = trimws(.data$Authorsclean)) %>%
      dplyr::group_by(.data$publication_id) %>%
      dplyr::mutate(
        total_authors = dplyr::n(),
        Rank = dplyr::row_number(),
        Position = dplyr::case_when(
          .data$Rank == 1 ~ "First",
          .data$Rank == 2 ~ "Second",
          .data$Rank == .data$total_authors & .data$Rank > 2 ~ "Last",
          TRUE ~ "Other"
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data$Authorsclean, .data$Position) %>%
      dplyr::summarise(freq = dplyr::n(), .groups = "drop") %>%
      tidyr::complete(.data$Authorsclean, .data$Position, fill = list(freq = 0)) %>%
      tidyr::pivot_wider(
        names_from = .data$Position,
        values_from = .data$freq,
        values_fill = 0
      ) %>%
      dplyr::mutate(
        `All Others` = .data$Other,
        Total = .data$First + .data$Second + .data$Last + .data$`All Others`
      ) %>%
      dplyr::select(-.data$Other) %>%
      dplyr::arrange(dplyr::desc(.data$Total)) %>%
      dplyr::slice_head(n = top_n)
  }

  p5 <- process_positions(filtered_data) %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::html("Author Position Distribution"),
      subtitle = subtitle_suffix
    ) %>%
    gt::fmt_number(decimals = 0)

  # Overall Fractional Contributions
  author_fractional <- filtered_data %>%
    dplyr::select(.data$publication_id, .data$Authorsclean) %>%
    tidyr::separate_rows(.data$Authorsclean, sep = ";\\s*") %>%
    dplyr::mutate(Authorsclean = trimws(.data$Authorsclean)) %>%
    dplyr::group_by(.data$publication_id) %>%
    dplyr::mutate(total_authors = dplyr::n()) %>%
    dplyr::group_by(.data$Authorsclean) %>%
    dplyr::summarise(
      `Fractional Count` = sum(1 / .data$total_authors),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$`Fractional Count`)) %>%
    dplyr::slice(1:top_n)

  p6 <- author_fractional %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::html("Overall Fractional Contributions"),
      subtitle = subtitle_suffix
    ) %>%
    gt::fmt_number(decimals = 2) %>%
    gt::cols_align(align = "center")

  # Return all results
  list(
    most_productive_overall = p1,
    most_productive_by_level = level_tables,
    detailed_productivity = p4,
    author_positions = p5,
    fractional_contributions = p6,
    comprehensive_analysis = p7,
    raw_data = comprehensive_data,
    counting_methods_used = counting_methods,
    data_filtered = filtered_data,
    fractional_counts = fractional_counts,
    position_counts = position_counts,
    first_author_counts = first_author_counts,
    authors_by_level = authors_by_level,
    first_authors_unit_counts = first_authors_unit_counts,
    active_years_counts = active_years_counts
  )
}
