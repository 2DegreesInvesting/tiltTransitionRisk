#' Adds transition risk categories for transition risk scores
#'
#' @param data A dataframe
#'
#' @keywords internal
#' @return A dataframe.
#'
#' @export
#'
#' @examples
#' library(tiltToyData)
#' library(readr)
#' library(dplyr)
#' options(readr.show_col_types = FALSE)
#'
#' emissions_profile_products <- read_csv(toy_emissions_profile_products_ecoinvent())
#' all_uuids_scenario_sectors <- read_csv(toy_all_uuids_scenario_sectors())
#' scenarios <- read_csv(toy_sector_profile_any_scenarios())
#'
#' transition_risk_thresholds <- add_thresholds_transition_risk(
#'   emissions_profile_products,
#'   all_uuids_scenario_sectors,
#'   scenarios
#' )
#'
#' output <- add_transition_risk_category(transition_risk_thresholds)
#' output
add_transition_risk_category <- function(data) {
  check_crucial_cols(data, c(
    col_transition_risk_score(), col_tr_low_threshold(),
    col_tr_high_threshold()
  ))

  mutate(data, transition_risk_category = ifelse(
    is.na(.data[[col_transition_risk_score()]]),
    NA,
    categorize_risk(
      .data[[col_transition_risk_score()]],
      .data[[col_tr_low_threshold()]],
      .data[[col_tr_high_threshold()]]
    )
  ))
}
