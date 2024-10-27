test_that("outputs expected columns at company level", {
  example_data <- example_risk_categories_at_product_level()
  out <- create_risk_categories_at_company_level(example_data)

  expected <- c(
    "companies_id", "benchmark_tr_score_avg",
    "transition_risk_category", "transition_risk_category_share"
  )
  expect_equal(names(out)[seq_along(expected)], expected)
})

test_that("`transition_risk_category_share` sums up to 1 per `benchmark_tr_score_avg`", {
  example_data <- example_risk_categories_at_product_level()
  out <- create_risk_categories_at_company_level(example_data)

  sum <- unique(summarise(out,
    sum = sum(transition_risk_category_share),
    .by = benchmark_tr_score_avg
  )$sum)
  expect_equal(sum, 1)
})

test_that("outputs NA `transition_risk_category_share` for NA in `benchmark_tr_score` and `transition_risk_category`", {
  example_data <- example_risk_categories_at_product_level(
    benchmark_tr_score = NA_character_,
    transition_risk_category = NA_character_
  )
  out <- create_risk_categories_at_company_level(example_data)

  expect_true(is.na(out$transition_risk_category_share))
})
