#' UEF Unit Translations and Hierarchy
#'
#' @description
#' A data frame mapping UEF (University of Eastern Finland) unit names
#' (both Finnish and English) to standardized English names, abbreviations,
#' parent departments, and faculties. Includes 179 unit name variants found
#' in CRIS data.
#'
#' @format A data frame with 179 rows and 5 columns:
#' \describe{
#'   \item{unit}{Original unit name as appears in CRIS data (Finnish or English)}
#'   \item{unit_en}{Standardized English unit name}
#'   \item{unit_abbrev}{Short abbreviation for plotting}
#'   \item{department}{Parent department name}
#'   \item{faculty}{Faculty grouping: Health Sciences, Science and Forestry,
#'     Social Sciences and Business, Philosophical, or Administration}
#' }
#'
#' @source University of Eastern Finland CRIS system
#'
#' @seealso \code{\link{get_unit_translations}}, \code{\link{add_hierarchy_columns}}
#'
#' @examples
#' data(unit_translations)
#' head(unit_translations)
#'
#' # View units by faculty
#' table(unit_translations$faculty)
#'
"unit_translations"

#' UEF Unit Names and Abbreviations (Legacy)
#'
#' @description
#' A data frame containing the 34 unique university unit names found in
#' UEF (University of Eastern Finland) CRIS data, along with their
#' short abbreviations suitable for plotting.
#'
#' Note: For new code, consider using \code{\link{unit_translations}} which
#' includes department information and handles variant unit names.
#'
#' @format A data frame with 34 rows and 3 columns:
#' \describe{
#'   \item{unit_name}{Full unit name as appears in CRIS data}
#'   \item{unit_abbrev}{Short abbreviation for plotting}
#'   \item{faculty}{Faculty grouping}
#' }
#'
#' @source University of Eastern Finland CRIS system
#'
#' @seealso \code{\link{unit_translations}} for the enhanced version
#'
#' @examples
#' data(uef_units)
#' head(uef_units)
#'
"uef_units"

#' Sample Publication Data
#'
#' @description
#' A sample dataset containing anonymized publication records from a
#' university CRIS system. Includes JUFO-ranked publications with
#' author information, publication years, and unit affiliations.
#'
#' @format A data frame with 100 publication records containing:
#' \describe{
#'   \item{publication_id}{Unique identifier for each publication}
#'   \item{authors}{Semicolon-separated list of author names}
#'   \item{first_author}{Name of the first author}
#'   \item{publication_year}{Year of publication (2019-2024)}
#'   \item{jufo_level_of_publication}{JUFO ranking level (0, 1, 2, or 3)}
#'   \item{title_of_publication}{Publication title}
#'   \item{first_authors_unit}{Unit affiliation of the first author}
#' }
#'
#' @examples
#' data(sample_publications)
#' head(sample_publications)
#'
#' # Use with prepare_cris_data workflow
#' \dontrun{
#' # For real data:
#' data <- prepare_cris_data("Data/")
#' }
#'
"sample_publications"
