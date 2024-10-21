best_case_worst_case_avg_reduction_targets <- function(data) {
  product <- data |>
    unnest_product()

  avg_best_case_worst_case_at_product_level <- product |>
    prepare_for_join_at_company_level_reduction_targets()

  avg_best_case <- prepare_avg_best_case_join_table_reduction_targets(
    avg_best_case_worst_case_at_product_level
  )
  avg_worst_case <- prepare_avg_worst_case_join_table_reduction_targets(
    avg_best_case_worst_case_at_product_level
  )

  company <- data |>
    unnest_company() |>
    left_join(avg_best_case, by = c(
      col_companies_id(),
      "scenario",
      "year"
    )) |>
    left_join(avg_worst_case, by = c(
      col_companies_id(),
      "scenario",
      "year"
    ))

  tilt_profile(nest_levels(product, company))
}

prepare_for_join_at_company_level_reduction_targets <- function(data) {
  data |>
    select(all_of(c(
      col_companies_id(),
      col_scenario(),
      col_year(),
      "sector_profile_best_case",
      "sector_profile_worst_case"
    ))) |>
    distinct() |>
    rename("avg_reduction_targets_best_case" = "sector_profile_best_case",
           "avg_reduction_targets_worst_case" = "sector_profile_worst_case")
}

prepare_avg_worst_case_join_table_reduction_targets <- function(data) {
  data |>
    prepare_avg_best_case_join_table(
      "avg_reduction_targets_best_case",
      "avg_reduction_targets_worst_case"
    )
}

prepare_avg_best_case_join_table_reduction_targets <- function(data) {
  data |>
    prepare_avg_best_case_join_table(
      "avg_reduction_targets_worst_case",
      "avg_reduction_targets_best_case"
    )
}
