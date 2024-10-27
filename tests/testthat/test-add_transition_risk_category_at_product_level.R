test_that("if input data lacks crucial columns, errors gracefully", {
  co2 <- read_csv(toy_emissions_profile_products_ecoinvent())
  all_activities_scenario_sectors <- read_csv(toy_all_activities_scenario_sectors())
  scenarios <- read_csv(toy_sector_profile_any_scenarios())

  input_data <- add_thresholds_transition_risk(
    co2,
    all_activities_scenario_sectors,
    scenarios
  )

  crucial <- col_transition_risk_score()
  bad <- select(input_data, -all_of(crucial))
  expect_error(add_transition_risk_category_at_product_level(bad), crucial)

  crucial <- col_tr_low_threshold()
  bad <- select(input_data, -all_of(crucial))
  expect_error(add_transition_risk_category_at_product_level(bad), crucial)

  crucial <- col_tr_high_threshold()
  bad <- select(input_data, -all_of(crucial))
  expect_error(add_transition_risk_category_at_product_level(bad), crucial)
})

test_that("if `transition_risk_category` column has only NAs, then class of the column is `character`", {
  co2 <- read_csv(toy_emissions_profile_products_ecoinvent()) |>
    filter(activity_uuid_product_uuid != "76269c17-78d6-420b-991a-aa38c51b45b7")
  all_activities_scenario_sectors <- read_csv(toy_all_activities_scenario_sectors()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  scenarios <- read_csv(toy_sector_profile_any_scenarios())

  output <- add_thresholds_transition_risk(
    co2,
    all_activities_scenario_sectors,
    scenarios
  ) |>
    add_transition_risk_category_at_product_level()

  expected_class <- "character"
  expect_equal(class(output$transition_risk_category), expected_class)
})
