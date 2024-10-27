test_that("NA best case and worst case for a `benchmark_tr_score` at produuct level gives NA to `avg_transition_risk_best_case` and `avg_transition_risk_worst_case` at company level", {
  example_data_product <- example_best_case_worst_case_transition_risk_profile_product_level(
    benchmark_tr_score = c("1.5C RPS_2030_all", "1.5C RPS_2030_all", "1.5C RPS_2050_all"),
    transition_risk_score = c(1.0, 2.0, NA)
  ) |>
    best_case_worst_case_transition_risk_profile() |>
    polish_best_case_worst_case() |>
    polish_best_case_worst_case_transition_risk_profile()

  example_data_company <- example_best_case_worst_case_transition_risk_profile_company_level()
  input <- tilt_profile(nest_levels(example_data_product, example_data_company))
  out <- best_case_worst_case_transition_risk_profile_at_company_level(input)

  case_product <- out |> unnest_product()
  case_company <- out |> unnest_company()

  case_product_benchmark <- filter(case_product, benchmark_tr_score == "1.5C RPS_2050_all")
  # `transition_risk_profile_best_case` is NA at product level for benchmark_tr_score `1.5C RPS_2050_all`
  expect_true(is.na(case_product_benchmark$transition_risk_profile_best_case))
  # `transition_risk_profile_worst_case` is NA at product level for benchmark_tr_score `1.5C RPS_2050_all`
  expect_true(is.na(case_product_benchmark$transition_risk_profile_worst_case))

  case_company_benchmark <- filter(case_company, benchmark_tr_score_avg == "1.5C RPS_2050_all")
  # `avg_transition_risk_best_case` is NA at company level for benchmark_tr_score `1.5C RPS_2050_all`
  expect_true(is.na(case_company_benchmark$avg_transition_risk_best_case))
  # `avg_transition_risk_worst_case` is NA at company level for benchmark_tr_score `1.5C RPS_2050_all`
  expect_true(is.na(case_company_benchmark$avg_transition_risk_worst_case))
})

test_that("same best case and worst case for two products at produuct level gives only one row of that value to `avg_transition_risk_best_case` and `avg_transition_risk_worst_case` at company level", {
  example_data_product <- example_best_case_worst_case_transition_risk_profile_product_level(
    benchmark_tr_score = c("1.5C RPS_2030_all", "1.5C RPS_2030_all", "1.5C RPS_2050_all"),
    transition_risk_score = c(1.0, 1.0, NA)
  ) |>
    best_case_worst_case_transition_risk_profile() |>
    polish_best_case_worst_case() |>
    polish_best_case_worst_case_transition_risk_profile()

  example_data_company <- example_best_case_worst_case_transition_risk_profile_company_level()
  input <- tilt_profile(nest_levels(example_data_product, example_data_company))
  out <- best_case_worst_case_transition_risk_profile_at_company_level(input)

  case_product <- out |> unnest_product()
  case_company <- out |> unnest_company()

  # `transition_risk_profile_best_case` has same value for products `one` and `two` for benchmark_tr_score `1.5C RPS_2030_all`
  case_product_benchmark <- distinct(filter(case_product, benchmark_tr_score == "1.5C RPS_2030_all"))
  expected_same_value <- 1
  expect_equal(filter(case_product_benchmark, ep_product == "one")$transition_risk_profile_best_case, expected_same_value)
  expect_equal(filter(case_product_benchmark, ep_product == "two")$transition_risk_profile_best_case, expected_same_value)
  # `transition_risk_profile_worst_case` has same value for products `one` and `two` for benchmark_tr_score `1.5C RPS_2030_all`
  expect_equal(filter(case_product_benchmark, ep_product == "one")$transition_risk_profile_worst_case, expected_same_value)
  expect_equal(filter(case_product_benchmark, ep_product == "two")$transition_risk_profile_worst_case, expected_same_value)

  # `avg_transition_risk_best_case` at company level is assigned one value for two similar `transition_risk_profile_best_case` of different products
  expect_true(nrow(filter(case_company, benchmark_tr_score_avg == "1.5C RPS_2030_all")) == 1)
})

test_that("if input to `best_case_worst_case_transition_risk_profile_at_company_level` lacks crucial columns, errors gracefully", {
  example_data_product <- example_best_case_worst_case_transition_risk_profile_product_level() |>
    best_case_worst_case_transition_risk_profile() |>
    polish_best_case_worst_case() |>
    polish_best_case_worst_case_transition_risk_profile()

  example_data_company <- example_best_case_worst_case_transition_risk_profile_company_level()

  crucial <- col_companies_id()
  bad <- select(example_data_product, -all_of(crucial))
  expect_error(
    tilt_profile(nest_levels(bad, example_data_company)),
    crucial
  )

  crucial <- col_transition_risk_grouped_by()
  bad <- select(example_data_product, -all_of(crucial))
  bad_product <- tilt_profile(nest_levels(bad, example_data_company))
  expect_error(
    best_case_worst_case_transition_risk_profile_at_company_level(bad_product),
    crucial
  )

  crucial <- "transition_risk_profile_best_case"
  bad <- select(example_data_product, -all_of(crucial))
  bad_product <- tilt_profile(nest_levels(bad, example_data_company))
  expect_error(
    best_case_worst_case_transition_risk_profile_at_company_level(bad_product),
    crucial
  )

  crucial <- "transition_risk_profile_worst_case"
  bad <- select(example_data_product, -all_of(crucial))
  bad_product <- tilt_profile(nest_levels(bad, example_data_company))
  expect_error(
    best_case_worst_case_transition_risk_profile_at_company_level(bad_product),
    crucial
  )
})
