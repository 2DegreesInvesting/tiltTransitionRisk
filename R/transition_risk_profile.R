#' Calculate the indicator "transition risk profile"
#'
#' Adds the risk classification to calculated transition risk scores from
#' emission profile and sector profile indicator.
#'
#' @param emissions_profile Nested data frame. The output of
#'   `profile_emissions()`.
#' @param sector_profile Nested data frame. The output of `profile_sector()`.
#' @param co2 A dataframe
#' @param all_activities_scenario_sectors A dataframe
#' @param scenarios A dataframe
#' @param for_webtool Logical. Is it output for webtool or not?
#'
#' @return A data frame with the column `companies_id`, and the nested
#'   columns`product` and `company` holding the outputs at product and company
#'   level.
#' @export
#'
#' @family top-level functions
#' @family profile functions
#'
#' @examples
#' library(readr, warn.conflicts = FALSE)
#' library(dplyr, warn.conflicts = FALSE)
#' library(tiltToyData, warn.conflicts = FALSE)
#' library(tiltIndicator)
#' library(tiltIndicatorAfter)
#'
#' set.seed(123)
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
#' output <- transition_risk_profile(
#'   emissions_profile = toy_emissions_profile,
#'   sector_profile = toy_sector_profile,
#'   co2 = toy_emissions_profile_products_ecoinvent,
#'   all_activities_scenario_sectors = toy_all_activities_scenario_sectors,
#'   scenarios = toy_sector_profile_any_scenarios,
#'   for_webtool = FALSE
#' )
#'
#' output |> unnest_product()
#'
#' output |> unnest_company()
transition_risk_profile <- function(emissions_profile,
                                    sector_profile,
                                    co2,
                                    all_activities_scenario_sectors,
                                    scenarios,
                                    for_webtool = FALSE) {
  transition_risk_profile_impl(
    emissions_profile,
    sector_profile,
    co2,
    all_activities_scenario_sectors,
    scenarios
  ) |>
    add_transition_risk_category_at_company_level() |>
    best_case_worst_case_transition_risk_profile_at_company_level() |>
    pivot_wider_transition_risk_profile(
      include_co2 = option_output_co2_footprint()
    ) |>
    best_case_worst_case_avg_profile_ranking() |>
    best_case_worst_case_avg_reduction_targets() |>
    add_transition_risk_NA_share() |>
    relocate_transition_risk_profile_cols(
      include_co2 = option_output_co2_footprint()
    ) |>
    round_off_to_4_decimal_places() |>
    coefficient_of_variation_transition_risk_profile() |>
    polish_transition_risk_profile() |>
    prepare_webtool_output(
      for_webtool = for_webtool,
      include_co2 = option_output_co2_footprint()
    ) |>
    remove_case3_companies()
}

transition_risk_profile_impl <- function(emissions_profile,
                                         sector_profile,
                                         co2,
                                         all_activities_scenario_sectors,
                                         scenarios) {
  transition_risk_scores <- score_transition_risk_and_polish(emissions_profile,
    sector_profile,
    include_co2 = option_output_co2_footprint()
  )
  transition_risk_thresholds <- add_thresholds_transition_risk(
    co2,
    all_activities_scenario_sectors,
    scenarios
  ) |>
    select_crucial_threshold_cols()

  product <- transition_risk_scores |>
    unnest_product() |>
    left_join(transition_risk_thresholds, by = c(
      "benchmark_tr_score",
      "activity_uuid_product_uuid"
    )) |>
    add_transition_risk_category_at_product_level() |>
    best_case_worst_case_transition_risk_profile() |>
    polish_best_case_worst_case() |>
    polish_best_case_worst_case_transition_risk_profile()

  company <- transition_risk_scores |>
    unnest_company() |>
    arrange_transition_risk_at_company_level()

  tilt_profile(nest_levels(product, company))
}

arrange_transition_risk_at_company_level <- function(data) {
  arrange(data, .data$companies_id, .data$benchmark_tr_score_avg)
}

select_crucial_threshold_cols <- function(data) {
  select(data, all_of(c(
    "activity_uuid_product_uuid", "benchmark_tr_score",
    "transition_risk_low_threshold", "transition_risk_high_threshold"
  )))
}

remove_case3_companies <- function(data) {
  product <- data |>
    unnest_product()

  company <- data |>
    unnest_company()

  case_3_companies <- identify_case3_companies(product)

  final_product <- product |>
    filter(!(.data$companies_id %in% unique(case_3_companies$companies_id))) |>
    distinct()

  final_company <- company |>
    filter(!(.data$companies_id %in% unique(case_3_companies$companies_id))) |>
    distinct()

  tilt_profile(nest_levels(final_product, final_company))
}

identify_case3_companies <- function(data) {
  # To identify which companies belong to Case 3, please follow this link:
  # https://github.com/2DegreesInvesting/TiltDevProjectMGMT/issues/169#issuecomment-2284344632
  data |>
    mutate(
      check =
        all(is.na(.data$sector_target) & is.na(.data$matched_activity_name)),
      .by = col_companies_id()
    ) |>
    filter(.data$check)
}
