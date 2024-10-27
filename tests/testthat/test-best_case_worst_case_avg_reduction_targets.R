test_that("NA best case and worst case for a `scenario-year` at produuct level gives NA to `avg_reduction_targets_best_case` and `avg_reduction_targets_worst_case` at company level", {
  example_data_product <- example_best_case_worst_case_reduction_targets_product_level(
    scenario = c("1.5C RPS", "1.5C RPS", "1.5C RPS"),
    year = c(2030, 2030, 2050),
    reduction_targets = c(1.0, 2.0, NA)
  )

  example_data_company <- example_best_case_worst_case_reduction_targets_company_level()
  input <- tilt_profile(nest_levels(example_data_product, example_data_company))
  out <- input |>
    best_case_worst_case_sector_profile() |>
    best_case_worst_case_avg_reduction_targets()

  case_product <- out |> unnest_product()
  case_company <- out |> unnest_company()

  case_product_benchmark <- filter(case_product, scenario == "1.5C RPS", year == 2050)
  # `sector_profile_best_case` is NA at product level for scenario `1.5C RPS` and year 2050
  expect_true(is.na(case_product_benchmark$sector_profile_best_case))
  # `sector_profile_worst_case` is NA at product level for scenario `1.5C RPS` and year 2050`
  expect_true(is.na(case_product_benchmark$sector_profile_worst_case))

  case_company_benchmark <- filter(case_company, scenario == "1.5C RPS", year == 2050)
  # `avg_reduction_targets_best_case` is NA at company level for scenario `1.5C RPS` and year 2050
  expect_true(is.na(case_company_benchmark$avg_reduction_targets_best_case))
  # `avg_reduction_targets_worst_case` is NA at company level for scenario `1.5C RPS` and year 2050
  expect_true(is.na(case_company_benchmark$avg_reduction_targets_worst_case))
})

test_that("same best case and worst case for two products at product level gives only one row of that value to `avg_reduction_targets_best_case` and `avg_reduction_targets_worst_case` at company level", {
  example_data_product <- example_best_case_worst_case_reduction_targets_product_level(
    scenario = c("1.5C RPS", "1.5C RPS", "1.5C RPS"),
    year = c(2030, 2030, 2050),
    reduction_targets = c(1.0, 1.0, NA)
  )

  example_data_company <- example_best_case_worst_case_reduction_targets_company_level()
  input <- tilt_profile(nest_levels(example_data_product, example_data_company))
  out <- input |>
    best_case_worst_case_sector_profile() |>
    best_case_worst_case_avg_reduction_targets()

  case_product <- out |> unnest_product()
  case_company <- out |> unnest_company()

  # `sector_profile_best_case` has same value for products `one` and `two` for scenario `1.5C RPS` and year 2030
  case_product_benchmark <- distinct(filter(case_product, scenario == "1.5C RPS", year == 2030))
  expected_same_value <- 1
  expect_equal(filter(case_product_benchmark, ep_product == "one")$sector_profile_best_case, expected_same_value)
  expect_equal(filter(case_product_benchmark, ep_product == "two")$sector_profile_best_case, expected_same_value)
  # `sector_profile_worst_case` has same value for products `one` and `two` for scenario `1.5C RPS` and year 2030
  expect_equal(filter(case_product_benchmark, ep_product == "one")$sector_profile_worst_case, expected_same_value)
  expect_equal(filter(case_product_benchmark, ep_product == "two")$sector_profile_worst_case, expected_same_value)

  # `avg_reduction_targets_best_case` at company level is assigned one value for two similar `sector_profile_best_case` of different products
  expect_true(nrow(filter(case_company, scenario == "1.5C RPS", year == 2030)) == 1)
})

test_that("if input to `best_case_worst_case_avg_profile_ranking` lacks crucial columns, errors gracefully", {
  example_data_product <- example_best_case_worst_case_reduction_targets_product_level(
    scenario = c("1.5C RPS", "1.5C RPS", "1.5C RPS"),
    year = c(2030, 2030, 2050)
  )
  example_data_company <- example_best_case_worst_case_reduction_targets_company_level()
  case_example_data_product <- tilt_profile(nest_levels(example_data_product, example_data_company)) |>
    best_case_worst_case_sector_profile() |>
    unnest_product()

  crucial <- col_companies_id()
  bad <- select(example_data_product, -all_of(crucial))
  expect_error(
    tilt_profile(nest_levels(bad, example_data_company)),
    crucial
  )

  crucial <- col_scenario()
  bad <- select(case_example_data_product, -all_of(crucial))
  bad_product <- tilt_profile(nest_levels(bad, example_data_company))
  expect_error(
    best_case_worst_case_avg_reduction_targets(bad_product),
    crucial
  )

  crucial <- col_year()
  bad <- select(case_example_data_product, -all_of(crucial))
  bad_product <- tilt_profile(nest_levels(bad, example_data_company))
  expect_error(
    best_case_worst_case_avg_reduction_targets(bad_product),
    crucial
  )

  crucial <- "sector_profile_best_case"
  bad <- select(case_example_data_product, -all_of(crucial))
  bad_product <- tilt_profile(nest_levels(bad, example_data_company))
  expect_error(
    best_case_worst_case_avg_reduction_targets(bad_product),
    crucial
  )

  crucial <- "sector_profile_worst_case"
  bad <- select(case_example_data_product, -all_of(crucial))
  bad_product <- tilt_profile(nest_levels(bad, example_data_company))
  expect_error(
    best_case_worst_case_avg_reduction_targets(bad_product),
    crucial
  )
})
