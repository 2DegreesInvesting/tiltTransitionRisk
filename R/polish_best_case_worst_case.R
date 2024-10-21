polish_best_case_worst_case <- function(data) {
  data |>
    select(-all_of(c(
      "min_rank_per_company_benchmark",
      "max_rank_per_company_benchmark"
    ))) |>
    rename(amount_of_distinct_products = "n_distinct_products",
           amount_of_distinct_products_matched = "n_distinct_products_matched") |>
    distinct()
}

rename_with_prefix <- function(data, prefix, match = ".") {
  rename_with(data, ~ paste0(prefix, .x), .cols = matches(match))
}

polish_best_case_worst_case_emissions_profile <- function(data) {
  data |>
    rename_with_prefix("emissions_profile_", match = c(
      "best_case",
      "worst_case",
      "equal_weight"
    ))
}

polish_best_case_worst_case_transition_risk_profile <- function(data) {
  data |>
    rename_with_prefix("transition_risk_profile_", match = c(
      "^best_case$",
      "^worst_case$",
      "^equal_weight$"
    ))
}

polish_best_case_worst_case_sector_profile <- function(data) {
  data |>
    rename_with_prefix("sector_profile_", match = c(
      "^best_case$",
      "^worst_case$",
      "^equal_weight$"
    ))
}
