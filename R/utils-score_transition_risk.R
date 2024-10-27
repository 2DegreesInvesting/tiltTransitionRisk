prepare_trs_emissions <- function(data) {
  select(
    data,
    c(
      "companies_id",
      "benchmark",
      "profile_ranking",
      "ep_product",
      "activity_uuid_product_uuid"
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
        "activity_uuid_product_uuid"
      )
    ) |>
    mutate(scenario_year = paste(.data$scenario, .data$year, sep = "_")) |>
    select(-c("scenario", "year"))
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

relocate_trs_columns <- function(columns) {
  c(
    "companies_id",
    "company_name",
    "country",
    "benchmark_tr_score",
    columns
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
    "multi_match",
    "matching_certainty",
    "matching_certainty_company_average",
    "company_city",
    "postcode",
    "address",
    "main_activity",
    "activity_uuid_product_uuid",
    "tilt_sector",
    "tilt_subsector",
    "isic_4digit",
    "isic_4digit_name",
    "ei_geography"
  )
}

common_columns_emissions_sector_at_company_level <- function() {
  c(
    "companies_id",
    "company_name",
    "country",
    "company_city",
    "postcode",
    "address",
    "main_activity"
  )
}
