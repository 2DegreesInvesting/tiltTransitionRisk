best_case_worst_case_avg_profile_ranking <- function(data) {
  product <- data |>
    unnest_product()

  avg_best_case_worst_case_at_product_level <- product |>
    prepare_for_join_at_company_level_profile_ranking()

  avg_best_case <- prepare_avg_best_case_join_table_profile_ranking(
    avg_best_case_worst_case_at_product_level
  )
  avg_worst_case <- prepare_avg_worst_case_join_table_profile_ranking(
    avg_best_case_worst_case_at_product_level
  )

  company <- data |>
    unnest_company() |>
    left_join(avg_best_case, by = c(
      col_companies_id(),
      col_emission_grouped_by()
    )) |>
    left_join(avg_worst_case, by = c(
      col_companies_id(),
      col_emission_grouped_by()
    ))

  tilt_profile(nest_levels(product, company))
}

prepare_for_join_at_company_level_profile_ranking <- function(data) {
  data |>
    select(all_of(c(
      col_companies_id(),
      col_emission_grouped_by(),
      "emissions_profile_best_case",
      "emissions_profile_worst_case"
    ))) |>
    distinct() |>
    rename("avg_profile_ranking_best_case" = "emissions_profile_best_case",
           "avg_profile_ranking_worst_case" = "emissions_profile_worst_case")
}

prepare_avg_worst_case_join_table_profile_ranking <- function(data) {
  data |>
    prepare_avg_best_case_join_table(
      "avg_profile_ranking_best_case",
      "avg_profile_ranking_worst_case"
    )
}

prepare_avg_best_case_join_table_profile_ranking <- function(data) {
  data |>
    prepare_avg_best_case_join_table(
      "avg_profile_ranking_worst_case",
      "avg_profile_ranking_best_case"
    )
}
