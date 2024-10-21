add_transition_risk_NA_share <- function(data) {
  product <- data |>
    unnest_product() |>
    add_transition_risk_NA_share_at_product_level()

  company <- data |>
    unnest_company() |>
    select_and_join_transition_risk_NA_share_at_company_level(product)

  tilt_profile(nest_levels(product, company))
}

add_transition_risk_NA_share_at_product_level <- function(data) {
  data |>
    fill_benchmark_tr_score() |>
    transition_risk_NA_amount_all() |>
    transition_risk_NA_amount_benchmarks() |>
    transition_risk_NA_total() |>
    transition_risk_NA_share() |>
    polish_transition_risk_NA_share()
}

select_and_join_transition_risk_NA_share_at_company_level <- function(data, product) {
  join_table <- product |>
    select(all_of(c(
      "companies_id",
      "benchmark_tr_score",
      "transition_risk_NA_share"
    ))) |>
    distinct()

  data |>
    left_join(
      join_table,
      by = c("companies_id",
        "benchmark_tr_score_avg" = "benchmark_tr_score"
      )
    )
}

fill_benchmark_tr_score <- function(data) {
  mutate(data, benchmark_tr_score = ifelse(
    is.na(.data[[col_transition_risk_grouped_by()]]),
    paste(.data[[col_scenario()]],
      .data[[col_year()]],
      .data[[col_emission_grouped_by()]],
      sep = "_"
    ),
    .data[[col_transition_risk_grouped_by()]]
  ))
}

transition_risk_NA_amount_all <- function(data) {
  mutate(data,
    transition_risk_NA_amount_all = n_distinct(
      .data[[col_europages_product()]][is.na(.data$matched_activity_name) | is.na(.data$reduction_targets)]
    ),
    .by = col_companies_id()
  )
}

transition_risk_NA_amount_benchmarks <- function(data) {
  mutate(data,
    transition_risk_NA_amount_benchmarks = n_distinct(
      .data[[col_europages_product()]][is.na(.data$transition_risk_score)]
    ),
    .by = all_of(c(col_companies_id(), col_transition_risk_grouped_by()))
  )
}

transition_risk_NA_total <- function(data) {
  mutate(data,
    transition_risk_NA_total = ifelse(
      is.na(.data$matched_activity_name) | is.na(.data$reduction_targets),
      .data$transition_risk_NA_amount_all,
      .data$transition_risk_NA_amount_all + .data$transition_risk_NA_amount_benchmarks
    ),
    .by = all_of(c(col_companies_id(), col_transition_risk_grouped_by()))
  )
}

transition_risk_NA_share <- function(data) {
  mutate(data,
    transition_risk_NA_share = ifelse(
      .data$amount_of_distinct_products == 0,
      NA,
      .data$transition_risk_NA_total / .data$amount_of_distinct_products
    ),
    .by = all_of(c(col_companies_id(), col_transition_risk_grouped_by()))
  )
}

polish_transition_risk_NA_share <- function(data) {
  select(data, -all_of(c(
    "transition_risk_NA_amount_all",
    "transition_risk_NA_amount_benchmarks"
  )))
}
