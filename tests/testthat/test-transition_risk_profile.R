test_that("yields a 'tilt_profile'", {
  restore <- options(list(tiltIndicatorAfter.output_co2_footprint = TRUE))
  toy_emissions_profile_products_ecoinvent <- read_csv(toy_emissions_profile_products_ecoinvent()) |>
    filter(activity_uuid_product_uuid != "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_emissions_profile_any_companies <- read_csv(toy_emissions_profile_any_companies())
  toy_sector_profile_any_scenarios <- read_csv(toy_sector_profile_any_scenarios())
  toy_sector_profile_companies <- read_csv(toy_sector_profile_companies()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_europages_companies <- read_csv(toy_europages_companies())
  toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
  toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
  toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
  toy_isic_name <- read_csv(toy_isic_name())
  toy_all_activities_scenario_sectors <- read_csv(toy_all_activities_scenario_sectors()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")

  toy_emissions_profile <- profile_emissions(
    companies = toy_emissions_profile_any_companies,
    co2 = toy_emissions_profile_products_ecoinvent,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )
  toy_sector_profile <- profile_sector(
    companies = toy_sector_profile_companies,
    scenarios = toy_sector_profile_any_scenarios,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )

  output <- transition_risk_profile(
    emissions_profile = toy_emissions_profile,
    sector_profile = toy_sector_profile,
    co2 = toy_emissions_profile_products_ecoinvent,
    all_activities_scenario_sectors = toy_all_activities_scenario_sectors,
    scenarios = toy_sector_profile_any_scenarios
  )

  expect_s3_class(output, "tilt_profile")
})


test_that("outputs `NA` transition risk category for `NA` transition risk score at product level", {
  restore <- options(list(tiltIndicatorAfter.output_co2_footprint = TRUE))
  toy_emissions_profile_products_ecoinvent <- read_csv(toy_emissions_profile_products_ecoinvent()) |>
    filter(activity_uuid_product_uuid != "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_emissions_profile_any_companies <- read_csv(toy_emissions_profile_any_companies())
  toy_sector_profile_any_scenarios <- read_csv(toy_sector_profile_any_scenarios())
  toy_sector_profile_companies <- read_csv(toy_sector_profile_companies()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_europages_companies <- read_csv(toy_europages_companies())
  toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
  toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
  toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
  toy_isic_name <- read_csv(toy_isic_name())
  toy_all_activities_scenario_sectors <- read_csv(toy_all_activities_scenario_sectors()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")

  toy_emissions_profile <- profile_emissions(
    companies = toy_emissions_profile_any_companies,
    co2 = toy_emissions_profile_products_ecoinvent,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )
  toy_sector_profile <- profile_sector(
    companies = toy_sector_profile_companies,
    scenarios = toy_sector_profile_any_scenarios,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )

  output <- transition_risk_profile(
    emissions_profile = toy_emissions_profile,
    sector_profile = toy_sector_profile,
    co2 = toy_emissions_profile_products_ecoinvent,
    all_activities_scenario_sectors = toy_all_activities_scenario_sectors,
    scenarios = toy_sector_profile_any_scenarios
  ) |>
    unnest_product() |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")

  # Transition risk score is `NA` for uuid "76269c17-78d6-420b-991a-aa38c51b45b7"
  expect_true(is.na(unique(output$transition_risk_score)))
  # `transition_risk_category` is `NA` for `NA` transition risk score
  expect_true(is.na(unique(output$transition_risk_category)))
})

test_that("outputs `NA` in `avg_transition_risk_best_case` and `avg_transition_risk_worst_case` for `NA` at company level if `transition_risk_score` and `transition_risk_category` are `NA` at product level", {
  restore <- options(list(tiltIndicatorAfter.output_co2_footprint = TRUE))
  toy_emissions_profile_products_ecoinvent <- read_csv(toy_emissions_profile_products_ecoinvent()) |>
    filter(activity_uuid_product_uuid != "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_emissions_profile_any_companies <- read_csv(toy_emissions_profile_any_companies())
  toy_sector_profile_any_scenarios <- read_csv(toy_sector_profile_any_scenarios())
  toy_sector_profile_companies <- read_csv(toy_sector_profile_companies()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_europages_companies <- read_csv(toy_europages_companies())
  toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
  toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
  toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
  toy_isic_name <- read_csv(toy_isic_name())
  toy_all_activities_scenario_sectors <- read_csv(toy_all_activities_scenario_sectors()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")

  toy_emissions_profile <- profile_emissions(
    companies = toy_emissions_profile_any_companies,
    co2 = toy_emissions_profile_products_ecoinvent,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )
  toy_sector_profile <- profile_sector(
    companies = toy_sector_profile_companies,
    scenarios = toy_sector_profile_any_scenarios,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )

  output <- transition_risk_profile(
    emissions_profile = toy_emissions_profile,
    sector_profile = toy_sector_profile,
    co2 = toy_emissions_profile_products_ecoinvent,
    all_activities_scenario_sectors = toy_all_activities_scenario_sectors,
    scenarios = toy_sector_profile_any_scenarios
  )

  product_level_output <- output |>
    unnest_product()

  company_level_output <- output |>
    unnest_company()

  # `avg_transition_risk_equal_weight` and `transition_risk_category` are `NA`at product level
  expect_true(is.na(unique(product_level_output$transition_risk_score)))
  expect_true(is.na(unique(product_level_output$transition_risk_category)))
  # `avg_transition_risk_best_case` and `avg_transition_risk_worst_case` are `NA` are `NA` at company level
  expect_true(is.na(unique(company_level_output$avg_transition_risk_best_case)))
  expect_true(is.na(unique(company_level_output$avg_transition_risk_worst_case)))
})

test_that("with `*.output_co2_footprint` unset, `for_webtool = TRUE` yields no error", {
  unset <- NULL
  withr::local_options(list(tiltIndicatorAfter.output_co2_footprint = unset))

  toy_emissions_profile_products_ecoinvent <- read_csv(toy_emissions_profile_products_ecoinvent()) |>
    filter(activity_uuid_product_uuid != "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_emissions_profile_any_companies <- read_csv(toy_emissions_profile_any_companies())
  toy_sector_profile_any_scenarios <- read_csv(toy_sector_profile_any_scenarios())
  toy_sector_profile_companies <- read_csv(toy_sector_profile_companies()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_europages_companies <- read_csv(toy_europages_companies())
  toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
  toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
  toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
  toy_isic_name <- read_csv(toy_isic_name())
  toy_all_activities_scenario_sectors <- read_csv(toy_all_activities_scenario_sectors()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")

  toy_emissions_profile <- profile_emissions(
    companies = toy_emissions_profile_any_companies,
    co2 = toy_emissions_profile_products_ecoinvent,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )
  toy_sector_profile <- profile_sector(
    companies = toy_sector_profile_companies,
    scenarios = toy_sector_profile_any_scenarios,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )

  expect_no_error(
    transition_risk_profile(
      emissions_profile = toy_emissions_profile,
      sector_profile = toy_sector_profile,
      co2 = toy_emissions_profile_products_ecoinvent,
      all_activities_scenario_sectors = toy_all_activities_scenario_sectors,
      scenarios = toy_sector_profile_any_scenarios,
      for_webtool = TRUE
    )
  )
})

test_that("doesn't output `co2_footprint` at product level and `co2e_avg` at company level if `*.output_co2_footprint` unset and `for_webtool = TRUE`", {
  unset <- NULL
  withr::local_options(list(tiltIndicatorAfter.output_co2_footprint = unset))

  toy_emissions_profile_products_ecoinvent <- read_csv(toy_emissions_profile_products_ecoinvent()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_emissions_profile_any_companies <- read_csv(toy_emissions_profile_any_companies())
  toy_sector_profile_any_scenarios <- read_csv(toy_sector_profile_any_scenarios())
  toy_sector_profile_companies <- read_csv(toy_sector_profile_companies()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_europages_companies <- read_csv(toy_europages_companies())
  toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
  toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
  toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
  toy_isic_name <- read_csv(toy_isic_name())
  toy_all_activities_scenario_sectors <- read_csv(toy_all_activities_scenario_sectors()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")

  toy_emissions_profile <- profile_emissions(
    companies = toy_emissions_profile_any_companies,
    co2 = toy_emissions_profile_products_ecoinvent,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )
  toy_sector_profile <- profile_sector(
    companies = toy_sector_profile_companies,
    scenarios = toy_sector_profile_any_scenarios,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )

  output <- transition_risk_profile(
    emissions_profile = toy_emissions_profile,
    sector_profile = toy_sector_profile,
    co2 = toy_emissions_profile_products_ecoinvent,
    all_activities_scenario_sectors = toy_all_activities_scenario_sectors,
    scenarios = toy_sector_profile_any_scenarios,
    for_webtool = TRUE
  )

  expect_false("co2_footprint" %in% names(unnest_product(output)))
  expect_false("co2e_avg" %in% names(unnest_company(output)))
})


test_that("outputs `co2_footprint` at product level and `co2e_avg` at company level if `*.output_co2_footprint = TRUE` and `for_webtool = FALSE`", {
  withr::local_options(list(tiltIndicatorAfter.output_co2_footprint = TRUE))

  toy_emissions_profile_products_ecoinvent <- read_csv(toy_emissions_profile_products_ecoinvent()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_emissions_profile_any_companies <- read_csv(toy_emissions_profile_any_companies())
  toy_sector_profile_any_scenarios <- read_csv(toy_sector_profile_any_scenarios())
  toy_sector_profile_companies <- read_csv(toy_sector_profile_companies()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_europages_companies <- read_csv(toy_europages_companies())
  toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
  toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
  toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
  toy_isic_name <- read_csv(toy_isic_name())
  toy_all_activities_scenario_sectors <- read_csv(toy_all_activities_scenario_sectors()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")

  toy_emissions_profile <- profile_emissions(
    companies = toy_emissions_profile_any_companies,
    co2 = toy_emissions_profile_products_ecoinvent,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )
  toy_sector_profile <- profile_sector(
    companies = toy_sector_profile_companies,
    scenarios = toy_sector_profile_any_scenarios,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )

  output <- transition_risk_profile(
    emissions_profile = toy_emissions_profile,
    sector_profile = toy_sector_profile,
    co2 = toy_emissions_profile_products_ecoinvent,
    all_activities_scenario_sectors = toy_all_activities_scenario_sectors,
    scenarios = toy_sector_profile_any_scenarios
  )

  expect_true("co2_footprint" %in% names(unnest_product(output)))
  expect_true("co2e_avg" %in% names(unnest_company(output)))
})

test_that("removes `co2_footprint` at product level and `co2e_avg` at company level if `*.output_co2_footprint = TRUE` and `for_webtool = TRUE`", {
  withr::local_options(list(tiltIndicatorAfter.output_co2_footprint = TRUE))

  toy_emissions_profile_products_ecoinvent <- read_csv(toy_emissions_profile_products_ecoinvent()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_emissions_profile_any_companies <- read_csv(toy_emissions_profile_any_companies())
  toy_sector_profile_any_scenarios <- read_csv(toy_sector_profile_any_scenarios())
  toy_sector_profile_companies <- read_csv(toy_sector_profile_companies()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_europages_companies <- read_csv(toy_europages_companies())
  toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
  toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
  toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
  toy_isic_name <- read_csv(toy_isic_name())
  toy_all_activities_scenario_sectors <- read_csv(toy_all_activities_scenario_sectors()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")

  toy_emissions_profile <- profile_emissions(
    companies = toy_emissions_profile_any_companies,
    co2 = toy_emissions_profile_products_ecoinvent,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )
  toy_sector_profile <- profile_sector(
    companies = toy_sector_profile_companies,
    scenarios = toy_sector_profile_any_scenarios,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )

  output <- transition_risk_profile(
    emissions_profile = toy_emissions_profile,
    sector_profile = toy_sector_profile,
    co2 = toy_emissions_profile_products_ecoinvent,
    all_activities_scenario_sectors = toy_all_activities_scenario_sectors,
    scenarios = toy_sector_profile_any_scenarios,
    for_webtool = TRUE
  )

  expect_true(!("co2_footprint" %in% names(unnest_product(output))))
  expect_true(!("co2e_avg" %in% names(unnest_company(output))))
})


test_that("At company level the `emission*` column are of type double", {
  withr::local_options(list(tiltIndicatorAfter.output_co2_footprint = TRUE))

  toy_emissions_profile_products_ecoinvent <- read_csv(toy_emissions_profile_products_ecoinvent()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_emissions_profile_any_companies <- read_csv(toy_emissions_profile_any_companies())
  toy_sector_profile_any_scenarios <- read_csv(toy_sector_profile_any_scenarios())
  toy_sector_profile_companies <- read_csv(toy_sector_profile_companies()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_europages_companies <- read_csv(toy_europages_companies())
  toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
  toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
  toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
  toy_isic_name <- read_csv(toy_isic_name())
  toy_all_activities_scenario_sectors <- read_csv(toy_all_activities_scenario_sectors()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")

  toy_emissions_profile <- profile_emissions(
    companies = toy_emissions_profile_any_companies,
    co2 = toy_emissions_profile_products_ecoinvent,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )
  toy_sector_profile <- profile_sector(
    companies = toy_sector_profile_companies,
    scenarios = toy_sector_profile_any_scenarios,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )

  type <- transition_risk_profile(
    emissions_profile = toy_emissions_profile,
    sector_profile = toy_sector_profile,
    co2 = toy_emissions_profile_products_ecoinvent,
    all_activities_scenario_sectors = toy_all_activities_scenario_sectors,
    scenarios = toy_sector_profile_any_scenarios
  ) |>
    unnest_company() |>
    select(matches("emission_")) |>
    lapply(typeof) |>
    unlist() |>
    unique()
  expect_equal(type, "double")
})

test_that("the output at product level has all the new required columns (#189)", {
  toy_emissions_profile_products_ecoinvent <- read_csv(toy_emissions_profile_products_ecoinvent()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_emissions_profile_any_companies <- read_csv(toy_emissions_profile_any_companies())
  toy_sector_profile_any_scenarios <- read_csv(toy_sector_profile_any_scenarios())
  toy_sector_profile_companies <- read_csv(toy_sector_profile_companies()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_europages_companies <- read_csv(toy_europages_companies())
  toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
  toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
  toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
  toy_isic_name <- read_csv(toy_isic_name())
  toy_all_activities_scenario_sectors <- read_csv(toy_all_activities_scenario_sectors()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")

  toy_emissions_profile <- profile_emissions(
    companies = toy_emissions_profile_any_companies,
    co2 = toy_emissions_profile_products_ecoinvent,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )
  toy_sector_profile <- profile_sector(
    companies = toy_sector_profile_companies,
    scenarios = toy_sector_profile_any_scenarios,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )

  product <- transition_risk_profile(
    emissions_profile = toy_emissions_profile,
    sector_profile = toy_sector_profile,
    co2 = toy_emissions_profile_products_ecoinvent,
    all_activities_scenario_sectors = toy_all_activities_scenario_sectors,
    scenarios = toy_sector_profile_any_scenarios
  ) |>
    unnest_product()

  expect_true(any(matches_name(product, "postcode")))
  expect_true(any(matches_name(product, "address")))
  expect_true(any(matches_name(product, "min_headcount")))
  expect_true(any(matches_name(product, "max_headcount")))
  expect_true(any(matches_name(product, "emissions_profile_best_case")))
  expect_true(any(matches_name(product, "emissions_profile_worst_case")))
  expect_true(any(matches_name(product, "transition_risk_profile_best_case")))
  expect_true(any(matches_name(product, "transition_risk_profile_worst_case")))
  expect_true(any(matches_name(product, "isic_4digit")))
  expect_true(any(matches_name(product, "matching_certainty")))
  expect_true(any(matches_name(product, "company_name")))
  expect_true(any(matches_name(product, "emissions_profile_equal_weight")))
  expect_true(any(matches_name(product, "amount_of_distinct_products_matched")))
})

test_that("the output at company level has has all the new required columns (#189, #290)", {
  toy_emissions_profile_products_ecoinvent <- read_csv(toy_emissions_profile_products_ecoinvent()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_emissions_profile_any_companies <- read_csv(toy_emissions_profile_any_companies())
  toy_sector_profile_any_scenarios <- read_csv(toy_sector_profile_any_scenarios())
  toy_sector_profile_companies <- read_csv(toy_sector_profile_companies()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_europages_companies <- read_csv(toy_europages_companies())
  toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
  toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
  toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
  toy_isic_name <- read_csv(toy_isic_name())
  toy_all_activities_scenario_sectors <- read_csv(toy_all_activities_scenario_sectors()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")

  toy_emissions_profile <- profile_emissions(
    companies = toy_emissions_profile_any_companies,
    co2 = toy_emissions_profile_products_ecoinvent,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )
  toy_sector_profile <- profile_sector(
    companies = toy_sector_profile_companies,
    scenarios = toy_sector_profile_any_scenarios,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )

  company <- transition_risk_profile(
    emissions_profile = toy_emissions_profile,
    sector_profile = toy_sector_profile,
    co2 = toy_emissions_profile_products_ecoinvent,
    all_activities_scenario_sectors = toy_all_activities_scenario_sectors,
    scenarios = toy_sector_profile_any_scenarios
  ) |>
    unnest_company()

  expect_true(any(matches_name(company, "postcode")))
  expect_true(any(matches_name(company, "address")))
  expect_true(any(matches_name(company, "min_headcount")))
  expect_true(any(matches_name(company, "max_headcount")))
  expect_true(any(matches_name(company, "company")))
  expect_true(any(matches_name(company, "emission_rank_avg_best_case")))
  expect_true(any(matches_name(company, "emission_rank_avg_worst_case")))
  expect_true(any(matches_name(company, "sector_target_avg_best_case")))
  expect_true(any(matches_name(company, "sector_target_avg_worst_case")))
  expect_true(any(matches_name(company, "cov_transition_risk")))
  expect_true(any(matches_name(company, "cov_emission_rank")))
  expect_true(any(matches_name(company, "cov_sector_target")))
})

test_that("At product level, when either `sector_category` is NA or `emission_category` is NA, tilt and isic sectors are not NA", {
  toy_emissions_profile_products_ecoinvent <- read_csv(toy_emissions_profile_products_ecoinvent()) |>
    filter(activity_uuid_product_uuid != "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_emissions_profile_any_companies <- read_csv(toy_emissions_profile_any_companies())
  toy_sector_profile_any_scenarios <- read_csv(toy_sector_profile_any_scenarios())
  toy_sector_profile_companies <- read_csv(toy_sector_profile_companies()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_europages_companies <- read_csv(toy_europages_companies())
  toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
  toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
  toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
  toy_isic_name <- read_csv(toy_isic_name())
  toy_all_activities_scenario_sectors <- read_csv(toy_all_activities_scenario_sectors()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")

  toy_emissions_profile <- profile_emissions(
    companies = toy_emissions_profile_any_companies,
    co2 = toy_emissions_profile_products_ecoinvent,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )
  toy_sector_profile <- profile_sector(
    companies = toy_sector_profile_companies,
    scenarios = toy_sector_profile_any_scenarios,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )

  output_product <- transition_risk_profile(
    emissions_profile = toy_emissions_profile,
    sector_profile = toy_sector_profile,
    co2 = toy_emissions_profile_products_ecoinvent,
    all_activities_scenario_sectors = toy_all_activities_scenario_sectors,
    scenarios = toy_sector_profile_any_scenarios
  ) |> unnest_product()

  # when `sector_category` is NA then, tilt and isic sectors are not NA
  out_sector_na <- filter(output_product, is.na(sector_category))
  expect_true(nrow(filter(out_sector_na, is.na(out_sector_na$tilt_sector))) == 0)
  expect_true(nrow(filter(out_sector_na, is.na(out_sector_na$tilt_subsector))) == 0)
  expect_true(nrow(filter(out_sector_na, is.na(out_sector_na$isic_4digit))) == 0)

  # when `emission_category` is NA then, tilt and isic sectors are not NA
  out_emissions_na <- filter(output_product, is.na(emission_category))
  expect_true(nrow(filter(out_emissions_na, is.na(out_emissions_na$tilt_sector))) == 0)
  expect_true(nrow(filter(out_emissions_na, is.na(out_emissions_na$tilt_subsector))) == 0)
  expect_true(nrow(filter(out_emissions_na, is.na(out_emissions_na$isic_4digit))) == 0)
})

test_that("`transition_risk_NA_share` is not NA for all cases of benchmark combinations", {
  toy_emissions_profile_products_ecoinvent <- read_csv(toy_emissions_profile_products_ecoinvent()) |>
    filter(activity_uuid_product_uuid != "833caa78-30df-4374-900f-7f88ab44075b")
  toy_emissions_profile_any_companies <- read_csv(toy_emissions_profile_any_companies())
  toy_sector_profile_any_scenarios <- read_csv(toy_sector_profile_any_scenarios())
  toy_sector_profile_companies <- read_csv(toy_sector_profile_companies()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_europages_companies <- read_csv(toy_europages_companies())
  toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
  toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
  toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
  toy_isic_name <- read_csv(toy_isic_name())
  toy_all_activities_scenario_sectors <- read_csv(toy_all_activities_scenario_sectors()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")

  toy_emissions_profile <- profile_emissions(
    companies = toy_emissions_profile_any_companies,
    co2 = toy_emissions_profile_products_ecoinvent,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )
  toy_sector_profile <- profile_sector(
    companies = toy_sector_profile_companies,
    scenarios = toy_sector_profile_any_scenarios,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )

  output <- transition_risk_profile(
    emissions_profile = toy_emissions_profile,
    sector_profile = toy_sector_profile,
    co2 = toy_emissions_profile_products_ecoinvent,
    all_activities_scenario_sectors = toy_all_activities_scenario_sectors,
    scenarios = toy_sector_profile_any_scenarios
  )

  output_product <- output |> unnest_product()
  benchmark_cases <- c("1.5C RPS_2030_all", "NA_NA_all", "NA_NA_NA")
  expect_true(all(benchmark_cases %in% unique(output_product$grouping_transition_risk)))

  out_na <- filter(output_product, is.na(transition_risk_NA_share))
  expect_true(nrow(out_na) == 0)
})

test_that("`transition_risk_NA_share` is not greater than 1 and not less than 0 for all cases of benchmark combinations", {
  toy_emissions_profile_products_ecoinvent <- read_csv(toy_emissions_profile_products_ecoinvent()) |>
    filter(activity_uuid_product_uuid != "833caa78-30df-4374-900f-7f88ab44075b")
  toy_emissions_profile_any_companies <- read_csv(toy_emissions_profile_any_companies())
  toy_sector_profile_any_scenarios <- read_csv(toy_sector_profile_any_scenarios())
  toy_sector_profile_companies <- read_csv(toy_sector_profile_companies()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_europages_companies <- read_csv(toy_europages_companies())
  toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
  toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
  toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
  toy_isic_name <- read_csv(toy_isic_name())
  toy_all_activities_scenario_sectors <- read_csv(toy_all_activities_scenario_sectors()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")

  toy_emissions_profile <- profile_emissions(
    companies = toy_emissions_profile_any_companies,
    co2 = toy_emissions_profile_products_ecoinvent,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )
  toy_sector_profile <- profile_sector(
    companies = toy_sector_profile_companies,
    scenarios = toy_sector_profile_any_scenarios,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )

  output <- transition_risk_profile(
    emissions_profile = toy_emissions_profile,
    sector_profile = toy_sector_profile,
    co2 = toy_emissions_profile_products_ecoinvent,
    all_activities_scenario_sectors = toy_all_activities_scenario_sectors,
    scenarios = toy_sector_profile_any_scenarios
  )

  output_product <- output |> unnest_product()
  benchmark_cases <- c("1.5C RPS_2030_all", "NA_NA_all", "NA_NA_NA")
  expect_true(all(benchmark_cases %in% unique(output_product$grouping_transition_risk)))

  out_na <- unique(output_product$transition_risk_NA_share)
  expect_false(all(out_na < 0))
  expect_false(all(out_na > 1))
})

test_that("is sensitive to `for_webtool`", {
  withr::local_options(list(tiltIndicatorAfter.output_co2_footprint = TRUE))

  toy_emissions_profile_products_ecoinvent <- read_csv(toy_emissions_profile_products_ecoinvent()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_emissions_profile_any_companies <- read_csv(toy_emissions_profile_any_companies())
  toy_sector_profile_any_scenarios <- read_csv(toy_sector_profile_any_scenarios())
  toy_sector_profile_companies <- read_csv(toy_sector_profile_companies()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")
  toy_europages_companies <- read_csv(toy_europages_companies())
  toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
  toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
  toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
  toy_isic_name <- read_csv(toy_isic_name())
  toy_all_activities_scenario_sectors <- read_csv(toy_all_activities_scenario_sectors()) |>
    filter(activity_uuid_product_uuid == "76269c17-78d6-420b-991a-aa38c51b45b7")

  toy_emissions_profile <- profile_emissions(
    companies = toy_emissions_profile_any_companies,
    co2 = toy_emissions_profile_products_ecoinvent,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )
  toy_sector_profile <- profile_sector(
    companies = toy_sector_profile_companies,
    scenarios = toy_sector_profile_any_scenarios,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )

  not_for_webtool <- transition_risk_profile(
    emissions_profile = toy_emissions_profile,
    sector_profile = toy_sector_profile,
    co2 = toy_emissions_profile_products_ecoinvent,
    all_activities_scenario_sectors = toy_all_activities_scenario_sectors,
    scenarios = toy_sector_profile_any_scenarios,
    for_webtool = FALSE
  )

  for_webtool <- transition_risk_profile(
    emissions_profile = toy_emissions_profile,
    sector_profile = toy_sector_profile,
    co2 = toy_emissions_profile_products_ecoinvent,
    all_activities_scenario_sectors = toy_all_activities_scenario_sectors,
    scenarios = toy_sector_profile_any_scenarios,
    for_webtool = TRUE
  )

  expect_equal(names(for_webtool), names(not_for_webtool))

  not_for_webtool_company <- not_for_webtool |>
    unnest_company() |>
    ncol()
  for_webtool_company <- for_webtool |>
    unnest_company() |>
    ncol()
  expect_true(for_webtool_company < not_for_webtool_company)
})

test_that("case 3 companies are identified correctly", {
  # To identify which companies belong to Case 3, please follow this link:
  # https://github.com/2DegreesInvesting/TiltDevProjectMGMT/issues/169#issuecomment-2284344632
  case3_companies <- tribble(
    # styler: off
    ~companies_id, ~product, ~matched_activity_name, ~sector_target,
           "comp",      "a", "market for tap water",            2.0,
           "comp",      "b", "market for tap water",       NA_real_,
           "comp",      "c",          NA_character_,            2.0,
           "comp",      "d",          NA_character_,       NA_real_,
    "case_3_comp",      "a",          NA_character_,       NA_real_,
    "case_3_comp",      "b",          NA_character_,       NA_real_
    # styler: on
  )
  result <- identify_case3_companies(case3_companies)

  expected_case3_company <- "case_3_comp"
  expect_true(unique(result$companies_id) == expected_case3_company)
})

test_that("All columns are rounded off to 4 decimal places", {
  round_cols <- tribble(
    # styler: off
    ~character_col, ~logical_col, ~numerical_col,
            "comp",        TRUE,       2.9393876,
            "comp",       FALSE,        NA_real_,
            "comp",        TRUE,       4.5895959
    # styler: on
  )

  result <- round_off_to_4_decimal_places_impl(round_cols)
  expect_true(all(result$numerical_col == round(result$numerical_col, 4), na.rm = TRUE))
})

