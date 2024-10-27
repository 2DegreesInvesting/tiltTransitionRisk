test_that("Three `ep_products` with the same `benchmark_tr_score` but with different `transition_risk_score` will have `best_case` only for the lowest-scored product and have `worst_case` only for the highest-scored product", {
  example_data <- example_best_case_worst_case_transition_risk_profile_product_level()
  out <- best_case_worst_case_impl(example_data,
    col_group_by = col_transition_risk_grouped_by(),
    col_risk = col_transition_risk_category(),
    col_rank = "transition_risk_score"
  )

  only_one_best_case <- 1
  expect_equal(nrow(filter(out, best_case == 1.0)), only_one_best_case)

  only_one_worst_case <- 1
  expect_equal(nrow(filter(out, worst_case == 3.0)), only_one_worst_case)

  # Expected best case for lowest-scored product
  expected_best_case <- 1
  expect_equal(filter(out, transition_risk_score == 1.0)$best_case, expected_best_case)

  # Expected worst case for highest-scored product
  expected_worst_case <- 3
  expect_equal(filter(out, transition_risk_score == 3.0)$worst_case, expected_worst_case)
})

test_that("`NA` in `transition_risk_score` for a single product gives `NA` in `best_case` and `worst_case` for that product", {
  example_data <- example_best_case_worst_case_transition_risk_profile_product_level(
    transition_risk_score = c(1.0, 2.0, NA_real_)
  )
  out <- best_case_worst_case_impl(example_data,
    col_group_by = col_transition_risk_grouped_by(),
    col_risk = col_transition_risk_category(),
    col_rank = "transition_risk_score"
  )

  # Expected best case for NA in `transition_risk_score`
  expected_best_case <- NA_real_
  expect_equal(filter(out, is.na(transition_risk_score))$best_case, expected_best_case)

  # Expected worst case for NA in `transition_risk_score`
  expected_worst_case <- NA_real_
  expect_equal(filter(out, is.na(transition_risk_score))$worst_case, expected_worst_case)
})

test_that("`NA` in `transition_risk_score` for all products gives `NA` in `best_case` and `worst_case` for all products", {
  example_data <- example_best_case_worst_case_transition_risk_profile_product_level(
    transition_risk_score = c(NA_real_, NA_real_, NA_real_)
  )
  out <- best_case_worst_case_impl(example_data,
    col_group_by = col_transition_risk_grouped_by(),
    col_risk = col_transition_risk_category(),
    col_rank = "transition_risk_score"
  )

  # Expected best case for NA in `transition_risk_score`
  expected_best_case <- NA_real_
  expect_equal(unique(filter(out, is.na(transition_risk_score))$best_case), expected_best_case)

  # Expected worst case for NA in `transition_risk_score`
  expected_worst_case <- NA_real_
  expect_equal(unique(filter(out, is.na(transition_risk_score))$worst_case), expected_worst_case)
})

test_that("gives `NA` in `equal_weight` if a company has missing `ep_product`", {
  example_data <- example_best_case_worst_case_transition_risk_profile_product_level(
    transition_risk_category = NA_character_,
    ep_product = NA_character_
  )
  out <- best_case_worst_case_impl(example_data,
    col_group_by = col_transition_risk_grouped_by(),
    col_risk = col_transition_risk_category(),
    col_rank = "transition_risk_score"
  )

  expect_true(all(is.na(out$equal_weight)))
})

test_that("if input to `best_case_worst_case_impl` lacks crucial columns, errors gracefully", {
  example_data <- example_best_case_worst_case_transition_risk_profile_product_level()

  crucial <- col_companies_id()
  bad <- select(example_data, -all_of(crucial))
  expect_error(best_case_worst_case_impl(bad,
    col_group_by = col_transition_risk_grouped_by(),
    col_risk = col_transition_risk_category(),
    col_rank = "transition_risk_score"
  ), crucial)

  crucial <- col_europages_product()
  bad <- select(example_data, -all_of(crucial))
  expect_error(best_case_worst_case_impl(bad,
    col_group_by = col_transition_risk_grouped_by(),
    col_risk = col_transition_risk_category(),
    col_rank = "transition_risk_score"
  ), crucial)

  crucial <- col_transition_risk_grouped_by()
  bad <- select(example_data, -all_of(crucial))
  expect_error(best_case_worst_case_impl(bad,
    col_group_by = col_transition_risk_grouped_by(),
    col_risk = col_transition_risk_category(),
    col_rank = "transition_risk_score"
  ), crucial)

  crucial <- col_transition_risk_category()
  bad <- select(example_data, -all_of(crucial))
  expect_error(best_case_worst_case_impl(bad,
    col_group_by = col_transition_risk_grouped_by(),
    col_risk = col_transition_risk_category(),
    col_rank = "transition_risk_score"
  ), crucial)
})

test_that("`equal_weight` does not count unmatched `ep_product` after grouping by `companies_id` and `benchmark_tr_score`", {
  example_data <- example_best_case_worst_case_transition_risk_profile_product_level(
    companies_id = c("any", "any", "any", "any", "any"),
    ep_product = c("one", "two", "three", "four", "five"),
    benchmark_tr_score = c("1.5C RPS_2030_all", "1.5C RPS_2030_all", "1.5C RPS_2030_all", "1.5C RPS_2050_all", NA_character_),
    transition_risk_category = c("low", "medium", NA_character_, "low", NA_character_),
    transition_risk_score = c(1.0, 2.0, 3.0, 4.0, 5.0)
  )

  out <- best_case_worst_case_impl(example_data,
    col_group_by = col_transition_risk_grouped_by(),
    col_risk = col_transition_risk_category(),
    col_rank = "transition_risk_score"
  )

  out_1.5C_RPS_2030_all <- filter(out, benchmark_tr_score == "1.5C RPS_2030_all")
  expected_equal_weight_1.5C_RPS_2030_all <- 0.5
  expect_equal(unique(out_1.5C_RPS_2030_all$equal_weight), expected_equal_weight_1.5C_RPS_2030_all)

  out_1.5C_RPS_2050_all <- filter(out, benchmark_tr_score == "1.5C RPS_2050_all")
  expected_equal_weight_1.5C_RPS_2050_all <- 1.0
  expect_equal(unique(out_1.5C_RPS_2050_all$equal_weight), expected_equal_weight_1.5C_RPS_2050_all)

  out_NA <- filter(out, is.na(benchmark_tr_score))
  expected_equal_weight_NA <- NA_real_
  expect_equal(unique(out_NA$equal_weight), expected_equal_weight_NA)
})
