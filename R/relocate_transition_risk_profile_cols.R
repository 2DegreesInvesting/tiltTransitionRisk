relocate_transition_risk_profile_cols <- function(
    data,
    include_co2 = FALSE) {
  product <- data |>
    unnest_product() |>
    relocate_transition_risk_profile_cols_at_product_level(
      include_co2 = include_co2
    )

  company <- data |>
    unnest_company() |>
    relocate_transition_risk_profile_cols_at_company_level(
      include_co2 = include_co2
    )

  tilt_profile(nest_levels(product, company))
}

relocate_transition_risk_profile_cols_at_product_level <- function(
    data,
    include_co2 = FALSE) {
  data |>
    relocate(
      "companies_id",
      "company_name",
      "country",
      "postcode",
      "address",
      "main_activity",
      "ep_product",
      "matched_activity_name",
      "matched_reference_product",
      "matching_certainty",
      if (include_co2) "co2_footprint",
      "unit",
      "isic_4digit",
      "tilt_sector",
      "tilt_subsector",
      "benchmark",
      "co2e_lower",
      "co2e_upper",
      "profile_ranking",
      "emission_profile",
      "emissions_profile_equal_weight",
      "emissions_profile_best_case",
      "emissions_profile_worst_case",
      "scenario",
      "year",
      "reduction_targets",
      "sector_profile",
      "sector_profile_equal_weight",
      "sector_profile_best_case",
      "sector_profile_worst_case",
      "benchmark_tr_score",
      "transition_risk_score",
      "transition_risk_low_threshold",
      "transition_risk_high_threshold",
      "transition_risk_category",
      "transition_risk_profile_equal_weight",
      "transition_risk_profile_best_case",
      "transition_risk_profile_worst_case",
      "amount_of_distinct_products",
      "amount_of_distinct_products_matched",
      "transition_risk_NA_total",
      "transition_risk_NA_share",
      "min_headcount",
      "max_headcount"
    )
}

relocate_transition_risk_profile_cols_at_company_level <- function(
    data,
    include_co2 = FALSE) {
  data |>
    relocate(
      "companies_id",
      "company_name",
      "country",
      if (include_co2) "co2_avg",
      "benchmark",
      "profile_ranking_avg",
      "avg_profile_ranking_best_case",
      "avg_profile_ranking_worst_case",
      "emission_category_low",
      "emission_category_medium",
      "emission_category_high",
      "emission_category_NA",
      "scenario",
      "year",
      "reduction_targets_avg",
      "avg_reduction_targets_best_case",
      "avg_reduction_targets_worst_case",
      "sector_category_low",
      "sector_category_medium",
      "sector_category_high",
      "sector_category_NA",
      "benchmark_tr_score_avg",
      "avg_transition_risk_equal_weight",
      "avg_transition_risk_best_case",
      "avg_transition_risk_worst_case",
      "transition_risk_NA_share",
      "postcode",
      "address",
      "main_activity",
      "min_headcount",
      "max_headcount"
    )
}
