#' Calculates best case and worst case for tranistion risk profile at product level
#'
#' @param data Dataframe. Transition risk profile product level output
#'
#' @return A dataframe
#' @export
#' @keywords internal
best_case_worst_case_transition_risk_profile <- function(data) {
  crucial_cols <- c(
    col_companies_id(), col_europages_product(),
    col_transition_risk_grouped_by(), col_transition_risk_category()
  )
  check_crucial_cols(data, crucial_cols)

  best_case_worst_case_impl(data,
    col_group_by = col_transition_risk_grouped_by(),
    col_risk = col_transition_risk_category(),
    col_rank = "transition_risk_score"

  )
}

polish_best_case_worst_case_transition_risk_profile <- function(data) {
  data |>
    rename_with_prefix("transition_risk_profile_", match = c(
      "^best_case$",
      "^worst_case$",
      "^equal_weight$"
    ))
}
