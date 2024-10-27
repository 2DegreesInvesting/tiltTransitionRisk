compute_n_distinct_products <- function(data) {
  data |>
    mutate(
      n_distinct_products = n_distinct(.data[[col_europages_product()]], na.rm = TRUE),
      .by = col_companies_id()
    )
}

compute_n_distinct_products_matched <- function(data, col_risk, col_group_by) {
  data |>
    mutate(
      n_distinct_products_matched = n_distinct(.data[[col_europages_product()]][!is.na(.data[[col_risk]])], na.rm = TRUE),
      .by = all_of(c(col_companies_id(), col_group_by))
    )
}

compute_equal_weight <- function(data) {
  mutate(data,
    equal_weight = ifelse(.data$n_distinct_products_matched == 0, NA,
      1 / .data$n_distinct_products_matched
    )
  )
}

add_min_or_max_rank_per_company_benchmark <- function(data,
                                                      rank_per_company_benchmark,
                                                      col_group_by,
                                                      col_rank,
                                                      agg_func = min) {
  mutate(data,
    {{ rank_per_company_benchmark }} := ifelse(is.na(.data[[col_rank]]),
      NA_real_,
      agg_func(.data[[col_rank]], na.rm = TRUE)
    ),
    .by = all_of(c(col_companies_id(), col_group_by))
  )
}

add_case_col_if_rank_col_match <- function(data,
                                           case_col,
                                           col_rank,
                                           min_max_rank_col) {
  mutate(data, {{ case_col }} :=
    ifelse(.data[[col_rank]] == .data[[min_max_rank_col]],
      .data[[min_max_rank_col]],
      NA_real_
    )) |>
    mutate({{ case_col }} := ifelse(is.na(.data[[as_name(ensym(case_col))]]),
      NA_real_,
      .data[[as_name(ensym(case_col))]]
    ))
}
