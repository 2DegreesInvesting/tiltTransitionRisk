#' tiltTransitionRisk options
#'
#' @description
#' These options are meant to be used mainly by developers or analysts while
#' testing the code or creating data:
#' * `tiltIndicatorAfter.output_co2_footprint`:
#'     * At product level it outputs licensed column `co2_footprint`.
#'     * At company level it outputs the column `co2_avg` (average `co2_footprint`
#'     by `companies_id`).
#' * `tiltIndicatorAfter.verbose`: Controls verbosity.
#'
#' @keywords internal
#' @name tiltIndicatorAfter_options
#'
#' @examples
#' library(readr, warn.conflicts = FALSE)
#' library(dplyr, warn.conflicts = FALSE)
#' library(withr)
#' library(tiltToyData)
#' library(tiltIndicator)
#' library(tiltIndicatorAfter)
#'
#' set.seed(1)
#'
#' restore <- options(list(
#'   readr.show_col_types = FALSE,
#'   tiltIndicatorAfter.verbose = TRUE,
#'   tiltIndicatorAfter.output_co2_footprint = TRUE
#' ))
#'
#' companies <- read_csv(toy_emissions_profile_any_companies())
#' products <- read_csv(toy_emissions_profile_products_ecoinvent())
#' europages_companies <- read_csv(toy_europages_companies())
#' ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
#' ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
#' isic_name <- read_csv(toy_isic_name())
#'
#' result <- profile_emissions(
#'   companies,
#'   products,
#'   europages_companies = europages_companies,
#'   ecoinvent_activities = ecoinvent_activities,
#'   ecoinvent_europages = ecoinvent_europages,
#'   isic = isic_name
#' )
#'
#' result |>
#'   unnest_product() |>
#'   select(matches(c("co2")))
#'
#' result |>
#'   unnest_company() |>
#'   select(matches(c("co2")))
NULL

option_output_co2_footprint <- function() {
  getOption("tiltIndicatorAfter.output_co2_footprint", default = FALSE)
}

option_verbose <- function() {
  getOption("tiltIndicatorAfter.verbose", default = TRUE)
}
