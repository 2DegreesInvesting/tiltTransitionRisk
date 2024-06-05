test_that("outputs `NA` tranistion risk thresholds for `NA` transition risk score", {
  emissions_profile_products <- read_csv(toy_emissions_profile_products_ecoinvent()) |>
    filter(activity_uuid_product_uuid != "76269c17-78d6-420b-991a-aa38c51b45b7")
  all_uuids_scenario_sectors <- read_csv(toy_sector_profile_companies()) |>
    select(-c("companies_id", "company_name", "clustered")) |>
    distinct() |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  scenarios <- read_csv(toy_sector_profile_any_scenarios())

  output <- add_thresholds_transition_risk(
    emissions_profile_products,
    all_uuids_scenario_sectors,
    scenarios
  ) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")

  # Transition risk score is `NA` for uuid "76269c17-78d6-420b-991a-aa38c51b45b7"
  expect_true(is.na(unique(output$transition_risk_score)))
  # low and high thresholds are `NA` for transition risk score `NA`
  expect_true(is.na(unique(output$transition_risk_low_threshold)))
  expect_true(is.na(unique(output$transition_risk_high_threshold)))
})

test_that("low and high tranistion risk thresholds distribute all activities in three equal parts for each benchmark", {
  emissions_profile_products <- read_csv(toy_emissions_profile_products_ecoinvent())
  all_uuids_scenario_sectors <- read_csv(toy_sector_profile_companies()) |>
    select(-c("companies_id", "company_name", "clustered")) |>
    distinct()
  scenarios <- read_csv(toy_sector_profile_any_scenarios())

  output <- add_thresholds_transition_risk(
    emissions_profile_products,
    all_uuids_scenario_sectors,
    scenarios
  ) |>
    filter(benchmark_tr_score == "1.5C RPS_2030_isic_4digit")

  low_threshold <- unique(output$transition_risk_low_threshold)
  high_threshold <- unique(output$transition_risk_high_threshold)

  # For benchmark `1.5C RPS_2030_isic_4digit`
  one_third_number_of_activities <- nrow(output) / 3
  # Number of activities below low threshold is 1/3rd the number of total activities
  expect_equal(nrow(filter(output, transition_risk_score <= low_threshold)), one_third_number_of_activities)
  # Number of activities above high threshold is 1/3rd the total number of activities
  expect_equal(nrow(filter(output, transition_risk_score > high_threshold)), one_third_number_of_activities)
})

test_that("if `emissions_profile_products` lacks crucial columns, errors gracefully", {
  emissions_profile_products <- read_csv(toy_emissions_profile_products_ecoinvent())
  all_uuids_scenario_sectors <- read_csv(toy_sector_profile_companies()) |>
    select(-c("companies_id", "company_name", "clustered")) |>
    distinct()
  scenarios <- read_csv(toy_sector_profile_any_scenarios())

  crucial <- col_uuid()
  bad <- select(emissions_profile_products, -all_of(crucial))
  expect_error(add_thresholds_transition_risk(bad, all_uuids_scenario_sectors, scenarios), crucial)

  crucial <- col_co2_footprint()
  bad <- select(emissions_profile_products, -all_of(crucial))
  expect_error(add_thresholds_transition_risk(bad, all_uuids_scenario_sectors, scenarios), crucial)
})

test_that("if `all_uuids_scenario_sectors` lacks crucial columns, errors gracefully", {
  emissions_profile_products <- read_csv(toy_emissions_profile_products_ecoinvent())
  all_uuids_scenario_sectors <- read_csv(toy_sector_profile_companies()) |>
    select(-c("companies_id", "company_name", "clustered")) |>
    distinct()
  scenarios <- read_csv(toy_sector_profile_any_scenarios())

  crucial <- col_uuid()
  bad <- select(all_uuids_scenario_sectors, -all_of(crucial))
  expect_error(add_thresholds_transition_risk(emissions_profile_products, bad, scenarios), crucial)

  crucial <- col_type()
  bad <- select(all_uuids_scenario_sectors, -all_of(crucial))
  expect_error(add_thresholds_transition_risk(emissions_profile_products, bad, scenarios), crucial)

  crucial <- col_sector()
  bad <- select(all_uuids_scenario_sectors, -all_of(crucial))
  expect_error(add_thresholds_transition_risk(emissions_profile_products, bad, scenarios), crucial)

  crucial <- col_subsector()
  bad <- select(all_uuids_scenario_sectors, -all_of(crucial))
  expect_error(add_thresholds_transition_risk(emissions_profile_products, bad, scenarios), crucial)
})

test_that("if `scenarios` lacks crucial columns, errors gracefully", {
  emissions_profile_products <- read_csv(toy_emissions_profile_products_ecoinvent())
  all_uuids_scenario_sectors <- read_csv(toy_sector_profile_companies()) |>
    select(-c("companies_id", "company_name", "clustered")) |>
    distinct()
  scenarios <- read_csv(toy_sector_profile_any_scenarios())

  crucial <- col_type()
  bad <- select(scenarios, -all_of(crucial))
  expect_error(add_thresholds_transition_risk(emissions_profile_products, all_uuids_scenario_sectors, bad), crucial)

  crucial <- col_sector()
  bad <- select(scenarios, -all_of(crucial))
  expect_error(add_thresholds_transition_risk(emissions_profile_products, all_uuids_scenario_sectors, bad), crucial)

  crucial <- col_subsector()
  bad <- select(scenarios, -all_of(crucial))
  expect_error(add_thresholds_transition_risk(emissions_profile_products, all_uuids_scenario_sectors, bad), crucial)

  crucial <- col_year()
  bad <- select(scenarios, -all_of(crucial))
  expect_error(add_thresholds_transition_risk(emissions_profile_products, all_uuids_scenario_sectors, bad), crucial)

  crucial <- col_scenario()
  bad <- select(scenarios, -all_of(crucial))
  expect_error(add_thresholds_transition_risk(emissions_profile_products, all_uuids_scenario_sectors, bad), crucial)

  crucial <- col_targets()
  bad <- select(scenarios, -all_of(crucial))
  expect_error(add_thresholds_transition_risk(emissions_profile_products, all_uuids_scenario_sectors, bad), crucial)
})
