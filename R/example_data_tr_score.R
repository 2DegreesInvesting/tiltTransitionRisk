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
