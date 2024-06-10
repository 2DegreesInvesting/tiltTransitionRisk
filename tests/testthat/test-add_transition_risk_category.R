test_that("if input data lacks crucial columns, errors gracefully", {
  emissions_profile_products <- read_csv(toy_emissions_profile_products_ecoinvent())
  all_uuids_scenario_sectors <- read_csv(toy_all_uuids_scenario_sectors())
  scenarios <- read_csv(toy_sector_profile_any_scenarios())

  input_data <- add_thresholds_transition_risk(
    emissions_profile_products,
    all_uuids_scenario_sectors,
    scenarios
  )

  crucial <- col_transition_risk_score()
  bad <- select(input_data, -all_of(crucial))
  expect_error(add_transition_risk_category(bad), crucial)

  crucial <- col_tr_low_threshold()
  bad <- select(input_data, -all_of(crucial))
  expect_error(add_transition_risk_category(bad), crucial)

  crucial <- col_tr_high_threshold()
  bad <- select(input_data, -all_of(crucial))
  expect_error(add_transition_risk_category(bad), crucial)
})
