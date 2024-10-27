#' Example input datasets for Transition Risk Score
#'
#' @return A dataframe.
#' @export
#' @keywords internal
#'
#' @examples
#' example_emissions_profile_at_product_level()
#' example_sector_profile_at_product_level()
example_emissions_profile_at_product_level <- function() {
  local_options(readr.show_col_types = FALSE)
  toy_emissions_profile_products_ecoinvent <-
    read_csv(toy_emissions_profile_products_ecoinvent())
  toy_emissions_profile_any_companies <-
    read_csv(toy_emissions_profile_any_companies())
  toy_europages_companies <- read_csv(toy_europages_companies())
  toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
  toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
  toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
  toy_isic_name <- read_csv(toy_isic_name())

  emissions_profile_at_product_level <- profile_emissions(
    companies = toy_emissions_profile_any_companies,
    co2 = toy_emissions_profile_products_ecoinvent,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  ) |>
    unnest_product()
  emissions_profile_at_product_level
}

#' @export
#' @rdname example_emissions_profile_at_product_level
example_sector_profile_at_product_level <- function() {
  local_options(readr.show_col_types = FALSE)
  toy_sector_profile_any_scenarios <-
    read_csv(toy_sector_profile_any_scenarios())
  toy_sector_profile_companies <-
    read_csv(toy_sector_profile_companies())
  toy_europages_companies <- read_csv(toy_europages_companies())
  toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
  toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
  toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
  toy_isic_name <- read_csv(toy_isic_name())

  sector_profile_at_product_level <- profile_sector(
    companies = toy_sector_profile_companies,
    scenarios = toy_sector_profile_any_scenarios,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  ) |>
    unnest_product()
  sector_profile_at_product_level
}


example_best_case_worst_case_transition_risk_profile_product_level <- example_data_factory(
  # styler: off
  tribble(
    ~companies_id, ~ep_product, ~benchmark_tr_score, ~transition_risk_category, ~transition_risk_score,
            "any",       "one", "1.5C RPS_2030_all",                     "low",                    1.0,
            "any",       "two", "1.5C RPS_2030_all",                  "medium",                    2.0,
            "any",     "three", "1.5C RPS_2030_all",                    "high",                    3.0,
  )
  # styler: on
)

example_best_case_worst_case_transition_risk_profile_company_level <- example_data_factory(
  # styler: off
  tribble(
    ~companies_id, ~benchmark_tr_score_avg, ~transition_risk_score_avg,
            "any",     "1.5C RPS_2030_all",                        5.0,
            "any",    "1.5C RPS_2030_unit",                        6.0,
            "any",     "1.5C RPS_2050_all",                        7.0
  )
  # styler: on
)


example_best_case_worst_case_profile_ranking_product_level <- example_data_factory(
  # styler: off
  tribble(
    ~companies_id, ~ep_product, ~benchmark, ~emission_profile, ~profile_ranking,
            "any",       "one",      "all",             "low",              1.0,
            "any",       "two",      "all",          "medium",              2.0,
            "any",     "three",      "all",            "high",              3.0
  )
  # styler: on
)

example_best_case_worst_case_profile_ranking_company_level <- example_data_factory(
  # styler: off
  tribble(
    ~companies_id,    ~benchmark, ~profile_ranking_avg,
            "any",         "all",                  5.0,
            "any",        "unit",                  6.0,
            "any", "tilt_sector",                  7.0
  )
  # styler: on
)

example_best_case_worst_case_reduction_targets_product_level <- example_data_factory(
  # styler: off
  tribble(
    ~companies_id, ~ep_product,  ~scenario_year, ~sector_profile, ~reduction_targets,
            "any",       "one", "1.5C RPS_2030",           "low",                1.0,
            "any",       "two", "1.5C RPS_2030",        "medium",                2.0,
            "any",     "three", "1.5C RPS_2030",          "high",                3.0
  )
  # styler: on
)

example_best_case_worst_case_reduction_targets_company_level <- example_data_factory(
  # styler: off
  tribble(
    ~companies_id,  ~scenario, ~year, ~reduction_targets,
            "any", "1.5C RPS",  2030,                1.0,
            "any", "1.5C RPS",  2050,                2.0,
            "any",  "NZ 2050",  2030,                3.0
  )
  # styler: on
)

example_risk_categories_at_product_level <- example_data_factory(
  # styler: off
  tribble(
    ~companies_id, ~ep_product, ~benchmark_tr_score, ~transition_risk_category,
            "any",         "a",               "all",                     "low",
            "any",         "a",               "all",                  "medium",
            "any",         "a",               "all",                    "high",
            "any",         "b",              "unit",                     "low",
            "any",         "b",              "unit",                  "medium",
            "any",         "b",              "unit",                    "high",
  )
  # styler: on
)
