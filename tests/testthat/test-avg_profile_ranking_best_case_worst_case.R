test_that("NA best case and worst case for a `benchmark` at produuct level gives NA to `avg_profile_ranking_best_case` and `avg_profile_ranking_worst_case` at company level", {
  example_data_product <- example_best_case_worst_case_profile_ranking_product_level(
    benchmark = c("all", "all", "unit"),
    profile_ranking = c(1.0, 2.0, NA)
  )

  example_data_company <- example_best_case_worst_case_profile_ranking_company_level()
  input <- tilt_profile(nest_levels(example_data_product, example_data_company))
  out <- input |>
    best_case_worst_case_emission_profile() |>
    best_case_worst_case_avg_profile_ranking()

  case_product <- out |> unnest_product()
  case_company <- out |> unnest_company()

  case_product_benchmark <- filter(case_product, benchmark == "unit")
  # `emissions_profile_best_case` is NA at product level for benchmark `unit`
  expect_true(is.na(case_product_benchmark$emissions_profile_best_case))
  # `emissions_profile_worst_case` is NA at product level for benchmark `unit`
  expect_true(is.na(case_product_benchmark$emissions_profile_worst_case))

  case_company_benchmark <- filter(case_company, benchmark == "unit")
  # `avg_profile_ranking_best_case` is NA at company level for benchmark `unit`
  expect_true(is.na(case_company_benchmark$avg_profile_ranking_best_case))
  # `avg_profile_ranking_worst_case` is NA at company level for benchmark `unit`
  expect_true(is.na(case_company_benchmark$avg_profile_ranking_worst_case))
})

test_that("same best case and worst case for two products at produuct level gives only one row of that value to `avg_profile_ranking_best_case` and `avg_profile_ranking_worst_case` at company level", {
  example_data_product <- example_best_case_worst_case_profile_ranking_product_level(
    benchmark = c("all", "all", "unit"),
    profile_ranking = c(1.0, 1.0, NA)
  )

  example_data_company <- example_best_case_worst_case_profile_ranking_company_level()
  input <- tilt_profile(nest_levels(example_data_product, example_data_company))
  out <- input |>
    best_case_worst_case_emission_profile() |>
    best_case_worst_case_avg_profile_ranking()

  case_product <- out |> unnest_product()
  case_company <- out |> unnest_company()

  # `emissions_profile_best_case` has same value for products `one` and `two` for benchmark `all`
  case_product_benchmark <- distinct(filter(case_product, benchmark == "all"))
  expected_same_value <- 1
  expect_equal(filter(case_product_benchmark, ep_product == "one")$emissions_profile_best_case, expected_same_value)
  expect_equal(filter(case_product_benchmark, ep_product == "two")$emissions_profile_best_case, expected_same_value)
  # `emissions_profile_worst_case` has same value for products `one` and `two` for benchmark `all`
  expect_equal(filter(case_product_benchmark, ep_product == "one")$emissions_profile_worst_case, expected_same_value)
  expect_equal(filter(case_product_benchmark, ep_product == "two")$emissions_profile_worst_case, expected_same_value)

  # `avg_profile_ranking_best_case` at company level is assigned one value for two similar `emissions_profile_best_case` of different products
  expect_true(nrow(filter(case_company, benchmark == "all")) == 1)
})

test_that("if input to `best_case_worst_case_avg_profile_ranking` lacks crucial columns, errors gracefully", {
  example_data_product <- example_best_case_worst_case_profile_ranking_product_level()
  example_data_company <- example_best_case_worst_case_profile_ranking_company_level()
  case_example_data_product <- tilt_profile(nest_levels(example_data_product, example_data_company)) |>
    best_case_worst_case_emission_profile() |>
    unnest_product()

  crucial <- col_companies_id()
  bad <- select(example_data_product, -all_of(crucial))
  expect_error(
    tilt_profile(nest_levels(bad, example_data_company)),
    crucial
  )

  crucial <- col_emission_grouped_by()
  bad <- select(case_example_data_product, -all_of(crucial))
  bad_product <- tilt_profile(nest_levels(bad, example_data_company))
  expect_error(
    best_case_worst_case_avg_profile_ranking(bad_product),
    crucial
  )

  crucial <- "emissions_profile_best_case"
  bad <- select(case_example_data_product, -all_of(crucial))
  bad_product <- tilt_profile(nest_levels(bad, example_data_company))
  expect_error(
    best_case_worst_case_avg_profile_ranking(bad_product),
    crucial
  )

  crucial <- "emissions_profile_worst_case"
  bad <- select(case_example_data_product, -all_of(crucial))
  bad_product <- tilt_profile(nest_levels(bad, example_data_company))
  expect_error(
    best_case_worst_case_avg_profile_ranking(bad_product),
    crucial
  )
})
