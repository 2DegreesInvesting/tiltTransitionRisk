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
#' co2 <- read_csv(toy_emissions_profile_products_ecoinvent())
#' all_activities_scenario_sectors <- read_csv(toy_all_activities_scenario_sectors())
#' scenarios <- read_csv(toy_sector_profile_any_scenarios())
#'
#' transition_risk_thresholds <- add_thresholds_transition_risk(
#'   co2,
#'   all_activities_scenario_sectors,
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
    NA_character_,
    categorize_risk(
      .data[[col_transition_risk_score()]],
      .data[[col_tr_low_threshold()]],
      .data[[col_tr_high_threshold()]]
    )
  ))
}
