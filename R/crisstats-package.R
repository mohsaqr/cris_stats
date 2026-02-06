#' crisstats: University Publication Productivity Analysis
#'
#' @description
#' The crisstats package provides tools for analyzing university publication
#' productivity from CRIS (Current Research Information System) data exports.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{clean_author_data}}: Clean and standardize author names
#'   \item \code{\link{generate_author_productivity_tables}}: Comprehensive productivity analysis
#'   \item \code{\link{generate_publication_stats}}: Publication statistics and visualizations
#'   \item \code{\link{analyze_author_publications}}: Individual author analysis
#'   \item \code{\link{plot_unit_growth}}: Unit-level productivity visualization
#' }
#'
#' @section Data Functions:
#' \itemize{
#'   \item \code{\link{read_cris_data}}: Read CRIS Excel exports
#'   \item \code{\link{compute_fractional_counts}}: Calculate fractional author contributions
#' }
#'
#' @section Unit Functions:
#' \itemize{
#'   \item \code{\link{abbreviate_units}}: Shorten unit names for plotting
#'   \item \code{\link{get_unit_abbreviations}}: Get default UEF unit abbreviations
#'   \item \code{\link{add_unit_column}}: Add abbreviated unit column to data
#' }
#'
#' @docType package
#' @name crisstats-package
#' @aliases crisstats
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import dplyr
#' @import ggplot2
#' @importFrom datawizard data_addprefix remove_empty_columns
#' @importFrom gt gt html tab_header fmt_number cols_align cols_label
#' @importFrom janitor clean_names tabyl
#' @importFrom purrr map_dfr
#' @importFrom readxl read_xlsx
#' @importFrom rlang sym .data `:=`
#' @importFrom scales breaks_pretty breaks_width
#' @importFrom splitstackshape cSplit
#' @importFrom stats na.omit reorder
#' @importFrom stringr regex str_count str_detect str_remove str_split str_to_upper
#' @importFrom tidyr complete pivot_wider replace_na separate_rows unnest
#' @importFrom utils head glob2rx
## usethis namespace: end
NULL

# Global variables to avoid R CMD check NOTEs
utils::globalVariables(c(".", "where"))
