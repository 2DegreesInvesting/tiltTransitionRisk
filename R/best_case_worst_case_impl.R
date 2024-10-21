best_case_worst_case_impl <- function(data, col_group_by, col_risk, col_rank) {
  data |>
    compute_n_distinct_products() |>
    compute_n_distinct_products_matched(col_risk, col_group_by) |>
    compute_equal_weight() |>
    add_min_or_max_rank_per_company_benchmark("min_rank_per_company_benchmark", col_group_by, col_rank, agg_func = min) |>
    add_min_or_max_rank_per_company_benchmark("max_rank_per_company_benchmark", col_group_by, col_rank, agg_func = max) |>
    add_case_col_if_rank_col_match("best_case", col_rank, "min_rank_per_company_benchmark") |>
    add_case_col_if_rank_col_match("worst_case", col_rank, "max_rank_per_company_benchmark")
}
