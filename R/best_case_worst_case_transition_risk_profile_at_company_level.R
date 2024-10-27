best_case_worst_case_transition_risk_profile_at_company_level <- function(data) {
  product <- data |>
    unnest_product()

  avg_best_case_worst_case_at_product_level <- product |>
    prepare_for_join_at_company_level_transition_risk()

  avg_best_case <- prepare_avg_best_case_join_table_transition_risk(
    avg_best_case_worst_case_at_product_level
  )
  avg_worst_case <- prepare_avg_worst_case_join_table_transition_risk(
    avg_best_case_worst_case_at_product_level
  )

  company <- data |>
    unnest_company() |>
    left_join(avg_best_case, by = c(col_companies_id(),
      "benchmark_tr_score_avg" = col_transition_risk_grouped_by()
    )) |>
    left_join(avg_worst_case, by = c(col_companies_id(),
      "benchmark_tr_score_avg" = col_transition_risk_grouped_by()
    )) |>
    polish_transition_risk_best_case_worst_case_at_company_level()

  tilt_profile(nest_levels(product, company))
}

prepare_for_join_at_company_level_transition_risk <- function(data) {
  data |>
    select(all_of(c(
      col_companies_id(),
      col_transition_risk_grouped_by(),
      "transition_risk_profile_best_case",
      "transition_risk_profile_worst_case"
    ))) |>
    distinct() |>
    rename("avg_transition_risk_best_case" = "transition_risk_profile_best_case",
           "avg_transition_risk_worst_case" = "transition_risk_profile_worst_case")
}

prepare_avg_worst_case_join_table_transition_risk <- function(data) {
  data |>
    prepare_avg_best_case_join_table(
      "avg_transition_risk_best_case",
      "avg_transition_risk_worst_case"
    )
}

prepare_avg_best_case_join_table_transition_risk <- function(data) {
  data |>
    prepare_avg_best_case_join_table(
      "avg_transition_risk_worst_case",
      "avg_transition_risk_best_case"
    )
}

polish_transition_risk_best_case_worst_case_at_company_level <- function(data) {
  rename(data, avg_transition_risk_equal_weight = "transition_risk_score_avg")
}

prepare_avg_best_case_join_table <- function(data, case1_col, case2_col) {
  data |>
    select(-all_of(c(case1_col))) |>
    filter(!is.na(.data[[case2_col]]))
}
