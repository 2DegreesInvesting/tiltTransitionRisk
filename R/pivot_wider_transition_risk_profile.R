#' Pivot company-level columns to wide format for indicator "transition risk profile"
#'
#' @param include_co2 Logical. Include `co2_*` columns ?
#'
#' @return A Dataframe
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' library(readr, warn.conflicts = FALSE)
#' library(dplyr, warn.conflicts = FALSE)
#' library(tiltToyData, warn.conflicts = FALSE)
#' library(tiltIndicator)
#' library(tiltIndicatorAfter)
#'
#' restore <- options(list(
#'   readr.show_col_types = FALSE,
#'   tiltIndicatorAfter.output_co2_footprint = TRUE
#' ))
#'
#' toy_emissions_profile_products_ecoinvent <- read_csv(toy_emissions_profile_products_ecoinvent())
#' toy_emissions_profile_any_companies <- read_csv(toy_emissions_profile_any_companies())
#' toy_sector_profile_any_scenarios <- read_csv(toy_sector_profile_any_scenarios())
#' toy_sector_profile_companies <- read_csv(toy_sector_profile_companies())
#' toy_europages_companies <- read_csv(toy_europages_companies())
#' toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
#' toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
#' toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
#' toy_isic_name <- read_csv(toy_isic_name())
#' toy_all_activities_scenario_sectors <- read_csv(toy_all_activities_scenario_sectors())
#'
#' toy_emissions_profile <- profile_emissions(
#'   companies = toy_emissions_profile_any_companies,
#'   co2 = toy_emissions_profile_products_ecoinvent,
#'   europages_companies = toy_europages_companies,
#'   ecoinvent_activities = toy_ecoinvent_activities,
#'   ecoinvent_europages = toy_ecoinvent_europages,
#'   isic = toy_isic_name
#' )
#'
#' toy_sector_profile <- profile_sector(
#'   companies = toy_sector_profile_companies,
#'   scenarios = toy_sector_profile_any_scenarios,
#'   europages_companies = toy_europages_companies,
#'   ecoinvent_activities = toy_ecoinvent_activities,
#'   ecoinvent_europages = toy_ecoinvent_europages,
#'   isic = toy_isic_name
#' )
#'
#' wide_format <- transition_risk_profile_impl(
#'   emissions_profile,
#'   sector_profile,
#'   co2,
#'   all_activities_scenario_sectors,
#'   scenarios
#' ) |>
#'   add_transition_risk_category_at_company_level() |>
#'   best_case_worst_case_transition_risk_profile_at_company_level() |>
#'   pivot_wider_transition_risk_profile(include_co2 = TRUE) |>
#'   unnest_company()
#' wide_format
#'
#' # Cleanup
#' options(restore)
#' }
pivot_wider_transition_risk_profile <- function(data, include_co2 = FALSE) {
  product <- data |>
    unnest_product()

  company <- data |>
    unnest_company()

  emission_profile_company <- company |>
    select_emissions_profile_pivot_cols(include_co2 = include_co2) |>
    exclude_subset_cols_then_pivot_wider(
      subset_cols = select_subset_emissions_profile_id_cols(
        include_co2 = include_co2
      ),
      names_from = "emission_profile",
      names_prefix = "emission_category_",
      values_from = "emission_profile_share"
    )

  sector_profile_company <- company |>
    select_sector_profile_pivot_cols() |>
    exclude_subset_cols_then_pivot_wider(
      subset_cols = select_subset_sector_profile_id_cols(),
      names_from = "sector_profile",
      names_prefix = "sector_category_",
      values_from = "sector_profile_share"
    )

  transition_risk_profile_company <- company |>
    select_transition_risk_profile_pivot_cols() |>
    exclude_subset_cols_then_pivot_wider(
      subset_cols = select_subset_transition_risk_profile_id_cols(),
      names_from = "transition_risk_category",
      names_prefix = "transition_risk_category_",
      values_from = "transition_risk_category_share"
    )

  company <- full_join(emission_profile_company,
    sector_profile_company,
    by = c(
      "companies_id",
      "country",
      "main_activity"
    ),
    relationship = "many-to-many"
  ) |>
    add_benchmark_tr_score_avg() |>
    full_join(transition_risk_profile_company,
      by = c(
        "companies_id",
        "country",
        "main_activity",
        "benchmark_tr_score_avg"
      ),
      relationship = "many-to-many"
    ) |>
    distinct()

  tilt_profile(nest_levels(product, company))
}

exclude_subset_cols_then_pivot_wider <- function(data,
                                                 subset_cols,
                                                 names_from,
                                                 names_prefix,
                                                 values_from) {
  data |>
    exclude_cols_then_pivot_wider(
      exclude_cols = "co2e",
      avoid_list_cols = TRUE,
      id_cols = subset_cols,
      names_from = names_from,
      names_prefix = names_prefix,
      values_from = values_from
    )
}

select_subset_emissions_profile_id_cols <- function(data, include_co2 = FALSE) {
  c(
    "companies_id",
    "company_name",
    "country",
    "main_activity",
    "benchmark",
    "profile_ranking_avg",
    "postcode",
    "address",
    if (include_co2) "co2_avg",
    "min_headcount",
    "max_headcount"
  )
}

select_emissions_profile_pivot_cols <- function(data, include_co2 = FALSE) {
  select(data, all_of(c(
    select_subset_emissions_profile_id_cols(include_co2 = include_co2),
    "emission_profile",
    "emission_profile_share"
  )))
}

select_subset_sector_profile_id_cols <- function(data) {
  c(
    "companies_id",
    "country",
    "main_activity",
    "scenario",
    "year",
    "reduction_targets_avg"
  )
}

select_sector_profile_pivot_cols <- function(data) {
  select(data, all_of(c(
    select_subset_sector_profile_id_cols(),
    "sector_profile",
    "sector_profile_share"
  )))
}

select_subset_transition_risk_profile_id_cols <- function(data) {
  c(
    "companies_id",
    "country",
    "main_activity",
    "benchmark_tr_score_avg",
    "avg_transition_risk_equal_weight",
    "avg_transition_risk_best_case",
    "avg_transition_risk_worst_case"
  )
}

select_transition_risk_profile_pivot_cols <- function(data) {
  select(data, all_of(c(
    select_subset_transition_risk_profile_id_cols(),
    "transition_risk_category",
    "transition_risk_category_share"
  )))
}
