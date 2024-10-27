add_transition_risk_category_at_company_level <- function(data) {
  product <- data |>
    unnest_product()

  risk_categories <- product |>
    create_risk_categories_at_company_level()

  company <- data |>
    unnest_company() |>
    join_risk_categories_at_company_level(risk_categories)

  tilt_profile(nest_levels(product, company))
}

create_risk_categories_at_company_level <- function(data) {
  data |>
    adapt_tr_product_cols_to_tiltIndicator_cols() |>
    epa_at_company_level() |>
    insert_row_with_na_in_risk_category() |>
    adapt_tiltIndicator_cols_to_tr_company_cols()
}

join_risk_categories_at_company_level <- function(data, risk_categories) {
  data |>
    create_transition_risk_category_col_at_company_level() |>
    left_join(risk_categories, by = c(
      "companies_id",
      "benchmark_tr_score_avg",
      "transition_risk_category"
    ))
}

adapt_tr_product_cols_to_tiltIndicator_cols <- function(data) {
  rename(data,
    grouped_by = "benchmark_tr_score",
    risk_category = "transition_risk_category"
  )
}

adapt_tiltIndicator_cols_to_tr_company_cols <- function(data) {
  rename(data,
    benchmark_tr_score_avg = "grouped_by",
    transition_risk_category = "risk_category",
    transition_risk_category_share = "value"
  )
}

create_transition_risk_category_col_at_company_level <- function(data) {
  mutate(data, transition_risk_category = coalesce(
    .data$emission_profile,
    .data$sector_profile
  ))
}
