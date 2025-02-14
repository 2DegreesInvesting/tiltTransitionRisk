prepare_trs_emissions <- function(data, include_co2 = FALSE) {
  select(
    data,
    c(
      "companies_id",
      "benchmark",
      "profile_ranking",
      "ep_product",
      "activity_uuid_product_uuid",
      "co2e_lower",
      "co2e_upper",
      "emission_profile",
      "emissions_profile_best_case",
      "emissions_profile_worst_case",
      "emissions_profile_equal_weight",
      if (include_co2) "co2_footprint"
    )
  )
}

prepare_trs_sector <- function(data) {
  data |>
    select(
      c(
        "companies_id",
        "scenario",
        "year",
        "reduction_targets",
        "ep_product",
        "activity_uuid_product_uuid",
        "sector_profile",
        "sector_profile_best_case",
        "sector_profile_worst_case",
        "sector_profile_equal_weight"
      )
    )
}

full_join_emmissions_sector <- function(emissions, sector) {
  full_join(
    emissions,
    sector,
    by = c("companies_id", "ep_product", "activity_uuid_product_uuid"),
    relationship = "many-to-many"
  )
}

get_rows_union_for_common_cols <-
  function(emissions_at_product_level,
           sector_at_product_level) {
    emission_common_columns <- emissions_at_product_level |>
      select(common_columns_emissions_sector_at_product_level()) |>
      distinct()

    sector_common_columns <- sector_at_product_level |>
      select(common_columns_emissions_sector_at_product_level()) |>
      distinct()

    distinct(bind_rows(emission_common_columns, sector_common_columns))
  }

relocate_trs_columns_product <- function(include_co2 = FALSE) {
  c(
    "companies_id",
    "country",
    "main_activity",
    "ep_product",
    "postcode",
    "address",
    "activity_uuid_product_uuid",
    "matched_activity_name",
    "matched_reference_product",
    "unit",
    "co2e_lower",
    "co2e_upper",
    "emission_profile",
    "benchmark",
    "profile_ranking",
    "tilt_sector",
    "tilt_subsector",
    "min_headcount",
    "max_headcount",
    "emissions_profile_best_case",
    "emissions_profile_worst_case",
    "isic_4digit",
    "matching_certainty",
    "company_name",
    "emissions_profile_equal_weight",
    if (include_co2) "co2_footprint",
    "sector_profile",
    "scenario",
    "year",
    "reduction_targets",
    "sector_profile_best_case",
    "sector_profile_worst_case",
    "sector_profile_equal_weight",
    "benchmark_tr_score",
    "transition_risk_score"
  )
}

relocate_trs_columns_company <- function(include_co2 = FALSE) {
  c(
    "companies_id",
    "company_name",
    "country",
    "main_activity",
    "postcode",
    "address",
    "benchmark",
    "min_headcount",
    "max_headcount",
    "emission_profile",
    "emission_profile_share",
    "profile_ranking_avg",
    if (include_co2) "co2_avg",
    "sector_profile",
    "sector_profile_share",
    "scenario",
    "year",
    "reduction_targets_avg",
    "benchmark_tr_score_avg",
    "transition_risk_score_avg"
  )
}

product_level_trs_column <- function() {
  c("transition_risk_score")
}

company_level_trs_avg_column <- function() {
  c("transition_risk_score_avg")
}

trs_company_columns <- function() {
  c(
    common_columns_emissions_sector_at_company_level(),
    "benchmark_tr_score_avg"
  )
}

trs_product_output_columns <- function() {
  c(
    common_columns_emissions_sector_at_product_level(),
    product_level_trs_column(),
    "profile_ranking",
    "reduction_targets",
    "benchmark_tr_score"
  )
}

trs_company_output_columns <- function() {
  c(trs_company_columns(), company_level_trs_avg_column())
}

common_columns_emissions_sector_at_product_level <- function() {
  c(
    "companies_id",
    "company_name",
    "country",
    "ep_product",
    "matched_activity_name",
    "matched_reference_product",
    "unit",
    "matching_certainty",
    "postcode",
    "address",
    "main_activity",
    "activity_uuid_product_uuid",
    "tilt_sector",
    "tilt_subsector",
    "isic_4digit",
    "min_headcount",
    "max_headcount"
  )
}

common_columns_emissions_sector_at_company_level <- function() {
  c(
    "companies_id",
    "company_name",
    "country",
    "postcode",
    "address",
    "main_activity",
    "min_headcount",
    "max_headcount"
  )
}
