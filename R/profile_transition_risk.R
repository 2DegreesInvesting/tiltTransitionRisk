#' Calculate the indicator "transition risk profile"
#'
#' Adds the risk classification to calculated transition risk scores from
#' emission profile and sector profile indicator.
#'
#' @param emissions_profile Nested data frame. The output of
#'   `profile_emissions()`.
#' @param sector_profile Nested data frame. The output of `profile_sector()`.
#' @param pivot_wider Logical. Pivot the output at company level to a wide
#'   format?
#' @param emissions_profile_products A dataframe
#' @param all_uuids_scenario_sectors A dataframe
#' @param scenarios A dataframe
#'
#' @return A data frame with the column `companies_id`, and the nested
#'   columns`product` and `company` holding the outputs at product and company
#'   level.
#' @export
#'
#' @examples
#' library(readr, warn.conflicts = FALSE)
#' library(dplyr, warn.conflicts = FALSE)
#' library(tiltToyData, warn.conflicts = FALSE)
#' library(tiltIndicatorAfter, warn.conflicts = FALSE)
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
#'
#' emissions_profile <- profile_emissions(
#'   companies = toy_emissions_profile_any_companies,
#'   co2 = toy_emissions_profile_products_ecoinvent,
#'   europages_companies = toy_europages_companies,
#'   ecoinvent_activities = toy_ecoinvent_activities,
#'   ecoinvent_europages = toy_ecoinvent_europages,
#'   isic = toy_isic_name
#' )
#'
#' sector_profile <- profile_sector(
#'   companies = toy_sector_profile_companies,
#'   scenarios = toy_sector_profile_any_scenarios,
#'   europages_companies = toy_europages_companies,
#'   ecoinvent_activities = toy_ecoinvent_activities,
#'   ecoinvent_europages = toy_ecoinvent_europages,
#'   isic = toy_isic_name
#' )
#'
#' emissions_profile_products <- read_csv(toy_emissions_profile_products_ecoinvent())
#' all_uuids_scenario_sectors <- read_csv(toy_all_uuids_scenario_sectors())
#' scenarios <- read_csv(toy_sector_profile_any_scenarios())
#'
#' output <- profile_transition_risk(emissions_profile,
#'   sector_profile,
#'   pivot_wider = FALSE,
#'   emissions_profile_products,
#'   all_uuids_scenario_sectors,
#'   scenarios
#' )
#'
#' output |> unnest_product()
#'
#' output |> unnest_company()
profile_transition_risk <- function(emissions_profile,
                                    sector_profile,
                                    pivot_wider = FALSE,
                                    emissions_profile_products,
                                    all_uuids_scenario_sectors,
                                    scenarios) {
  transition_risk_scores <- score_transition_risk_and_polish(emissions_profile,
    sector_profile,
    pivot_wider = FALSE
  )
  transition_risk_thresholds <- add_thresholds_transition_risk(
    emissions_profile_products,
    all_uuids_scenario_sectors,
    scenarios
  ) |>
    select_crucial_threshold_cols()

  product <- transition_risk_scores |>
    unnest_product() |>
    left_join(transition_risk_thresholds, by = c("benchmark_tr_score", col_uuid())) |>
    add_transition_risk_category() |>
    polish_transition_risk_at_product_level()

  company <- transition_risk_scores |>
    unnest_company()

  nest_levels(product, company)
}

add_transition_risk_category <- function(data) {
  mutate(data, transition_risk_category = ifelse(
    is.na(.data$transition_risk_score),
    NA,
    categorize_risk(
      .data$transition_risk_score,
      .data$transition_risk_low_threshold,
      .data$transition_risk_high_threshold
    )
  ))
}

polish_transition_risk_at_product_level <- function(data) {
  data |>
    select(-c("co2_footprint", "unit")) |>
    relocate(c(
      col_uuid(), "transition_risk_score", "transition_risk_low_threshold",
      "transition_risk_high_threshold", "transition_risk_category", "benchmark_tr_score",
      col_ranking(), "reduction_targets"
    ))
}

select_crucial_threshold_cols <- function(data) {
  select(data, all_of(c(
    col_uuid(), "benchmark_tr_score", "transition_risk_low_threshold",
    "transition_risk_high_threshold"
  )))
}
