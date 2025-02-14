test_that("outputs expected columns at product level", {
  withr::local_options(list(tiltIndicatorAfter.output_co2_footprint = TRUE))

  emissions_profile_trs <- example_emissions_profile_trs()
  sector_profile_trs <- example_sector_profile_trs()

  out <-
    score_transition_risk(
      emissions_profile_trs,
      sector_profile_trs,
      include_co2 = TRUE
    ) |>
    unnest_product()

  expect_equal(sort(names(out)), sort(relocate_trs_columns_product(include_co2 = TRUE)))
})

test_that("outputs expected columns at company level", {
  withr::local_options(list(tiltIndicatorAfter.output_co2_footprint = TRUE))

  emissions_profile_trs <- example_emissions_profile_trs()
  sector_profile_trs <- example_sector_profile_trs()

  out <-
    score_transition_risk(
      emissions_profile_trs,
      sector_profile_trs,
      include_co2 = TRUE
    ) |>
    unnest_company()

  expect_equal(sort(names(out)), sort(relocate_trs_columns_company(include_co2 = TRUE)))
})

test_that("calculates `transition_risk_score` and `benchmark_tr_score` correctly", {
  emissions_profile_at_product_level <-
    example_emissions_profile_trs() |>
    unnest_product() |>
    filter(
      companies_id %in% c("antimonarchy_canine"),
      benchmark == "all"
    )

  emissions_profile_at_company_level <-
    example_emissions_profile_trs() |>
    unnest_company() |>
    filter(companies_id %in% c("antimonarchy_canine"))

  sector_profile_at_product_level <-
    example_sector_profile_trs() |>
    unnest_product() |>
    filter(
      companies_id %in% c("antimonarchy_canine"),
      scenario == "1.5C RPS",
      year == "2030"
    )

  sector_profile_at_company_level <-
    example_sector_profile_trs() |>
    unnest_company() |>
    filter(companies_id %in% c("antimonarchy_canine"))

  emissions_profile <- nest_levels(emissions_profile_at_product_level, emissions_profile_at_company_level)
  sector_profile <- nest_levels(sector_profile_at_product_level, sector_profile_at_company_level)

  out <-
    unnest_product(
      score_transition_risk(
        emissions_profile,
        sector_profile
      )
    )

  expect_equal(out$benchmark_tr_score, "1.5C RPS_2030_all")
  expect_equal(out$transition_risk_score, 0.59)
})

test_that("calculates `transition_risk_score_avg` correctly", {
  emissions_profile_at_product_level <-
    example_emissions_profile_trs() |>
    unnest_product() |>
    filter(
      companies_id %in% c("nonphilosophical_llama"),
      benchmark == "all"
    )

  emissions_profile_at_company_level <-
    example_emissions_profile_trs() |>
    unnest_company() |>
    filter(companies_id %in% c("nonphilosophical_llama"))

  sector_profile_at_product_level <-
    example_sector_profile_trs() |>
    unnest_product() |>
    filter(
      companies_id %in% c("nonphilosophical_llama"),
      scenario == "1.5C RPS",
      year == "2030"
    )

  sector_profile_at_company_level <-
    example_sector_profile_trs() |>
    unnest_company() |>
    filter(companies_id %in% c("nonphilosophical_llama"))

  emissions_profile <- nest_levels(emissions_profile_at_product_level, emissions_profile_at_company_level)
  sector_profile <- nest_levels(sector_profile_at_product_level, sector_profile_at_company_level)

  out <-
    unnest_company(
      score_transition_risk(
        emissions_profile,
        sector_profile
      )
    )

  expect_equal(round(unique(out$transition_risk_score_avg), 4), 0.2117)
})

test_that(
  "calculates `transition_risk_score_avg` correctly for unmatched `ep_product`
          of a company",
  {
    emissions_profile_at_product_level <-
      example_emissions_profile_trs() |>
      unnest_product() |>
      filter(
        companies_id %in% c("nonphilosophical_llama"),
        ep_product == "surface finishing, galvanic"
      )

    emissions_profile_at_company_level <-
      example_emissions_profile_trs() |>
      unnest_company() |>
      filter(companies_id %in% c("nonphilosophical_llama"))

    sector_profile_at_product_level <-
      example_sector_profile_trs() |>
      unnest_product() |>
      filter(
        companies_id %in% c("nonphilosophical_llama"),
        ep_product == "surface engineering"
      )

    sector_profile_at_company_level <-
      example_sector_profile_trs() |>
      unnest_company() |>
      filter(companies_id %in% c("nonphilosophical_llama"))

    emissions_profile <- nest_levels(emissions_profile_at_product_level, emissions_profile_at_company_level)
    sector_profile <- nest_levels(sector_profile_at_product_level, sector_profile_at_company_level)

    out <-
      unnest_company(
        score_transition_risk(
          emissions_profile,
          sector_profile
        )
      )

    # Both the ep_products are present only in one dataframe which will lead to
    # unmatched results and thereafter Null in `transition_risk_score_avg` column
    expect_equal(unique(out$transition_risk_score_avg), NaN)
  }
)


test_that(
  "`transition_risk_score` and `benchmark_tr_score` has NA due to
          NA in either column `profile_ranking` or `reduction_targets`",
  {
    emissions_profile_at_product_level <-
      example_emissions_profile_trs() |>
      unnest_product() |>
      filter(companies_id %in% c("antimonarchy_canine", "nonphilosophical_llama"))

    emissions_profile_at_company_level <-
      example_emissions_profile_trs() |>
      unnest_company() |>
      filter(companies_id %in% c("antimonarchy_canine", "nonphilosophical_llama"))

    sector_profile_at_product_level <-
      example_sector_profile_trs() |>
      unnest_product() |>
      filter(companies_id %in% c("celestial_lovebird", "nonphilosophical_llama"))

    sector_profile_at_company_level <-
      example_sector_profile_trs() |>
      unnest_company() |>
      filter(companies_id %in% c("celestial_lovebird", "nonphilosophical_llama"))

    emissions_profile <- nest_levels(emissions_profile_at_product_level, emissions_profile_at_company_level)
    sector_profile <- nest_levels(sector_profile_at_product_level, sector_profile_at_company_level)

    out <-
      unnest_product(
        score_transition_risk(
          emissions_profile,
          sector_profile
        )
      )

    tr_score_na <- out |>
      filter(is.na(transition_risk_score))

    benchmark_tr_score_na <- out |>
      filter(is.na(benchmark_tr_score))

    ranking_reduction_na <- out |>
      filter(is.na(profile_ranking) | is.na(reduction_targets))

    expect_equal(tr_score_na, ranking_reduction_na)
    expect_equal(benchmark_tr_score_na, ranking_reduction_na)
  }
)

test_that(
  "product level and company level outputs contain non-null info of all
  matched and unmatched companies after joining dataframes",
  {
    # uncommon companies in either dataframe will give unmatched results
    emissions_profile_at_product_level <-
      example_emissions_profile_trs() |>
      unnest_product() |>
      filter(companies_id %in% c("antimonarchy_canine", "nonphilosophical_llama"))

    emissions_profile_at_company_level <-
      example_emissions_profile_trs() |>
      unnest_company() |>
      filter(companies_id %in% c("antimonarchy_canine", "nonphilosophical_llama"))

    sector_profile_at_product_level <-
      example_sector_profile_trs() |>
      unnest_product() |>
      filter(companies_id %in% c("celestial_lovebird", "nonphilosophical_llama"))

    sector_profile_at_company_level <-
      example_sector_profile_trs() |>
      unnest_company() |>
      filter(companies_id %in% c("celestial_lovebird", "nonphilosophical_llama"))

    emissions_profile <- nest_levels(emissions_profile_at_product_level, emissions_profile_at_company_level)
    sector_profile <- nest_levels(sector_profile_at_product_level, sector_profile_at_company_level)

    trs_product <-
      unnest_product(
        score_transition_risk(
          emissions_profile,
          sector_profile
        )
      )

    trs_company <-
      unnest_company(
        score_transition_risk(
          emissions_profile,
          sector_profile
        )
      )

    # Select common columns of both matched and unmatched companies (except columns
    # computed by `score_transition_risk` function)
    common_cols_product <- trs_product |>
      select(common_columns_emissions_sector_at_product_level())
    common_cols_company <- trs_company |>
      select(common_columns_emissions_sector_at_company_level())

    # These checks ensures that there is not even a single NA in common columns
    # of matched and unmatched companies at both product and company level
    expect_false(any(is.na(common_cols_product)))
    expect_false(any(is.na(common_cols_company)))
  }
)

test_that("limits `transition_risk_score` between 0 and 1", {
  emissions_profile_at_product_level <-
    example_emissions_profile_trs() |>
    unnest_product() |>
    filter(companies_id %in% c("antimonarchy_canine"))

  emissions_profile_at_company_level <-
    example_emissions_profile_trs() |>
    unnest_company() |>
    filter(companies_id %in% c("antimonarchy_canine"))

  sector_profile_at_product_level <-
    example_sector_profile_trs() |>
    unnest_product() |>
    filter(companies_id %in% c("antimonarchy_canine")) |>
    mutate(reduction_targets = c(-100, -100, 100, 100))

  sector_profile_at_company_level <-
    example_sector_profile_trs() |>
    unnest_company() |>
    filter(companies_id %in% c("antimonarchy_canine"))

  emissions_profile <- nest_levels(emissions_profile_at_product_level, emissions_profile_at_company_level)
  sector_profile <- nest_levels(sector_profile_at_product_level, sector_profile_at_company_level)

  out <-
    unnest_product(
      score_transition_risk(
        emissions_profile,
        sector_profile
      )
    )

  # Due to large positive and negative `reduction_targets` values,
  # `transition_risk_score` should not be more than 1 and less than 0.
  expected_values <- c(0, 1)
  expect_equal(unique(out$transition_risk_score), expected_values)
})

test_that("with `*.output_co2_footprint` unset, `include_co2 = TRUE` yields an error", {
  unset <- NULL
  withr::local_options(list(tiltIndicatorAfter.output_co2_footprint = unset))

  toy_emissions_profile_products_ecoinvent <- read_csv(toy_emissions_profile_products_ecoinvent())
  toy_emissions_profile_any_companies <- read_csv(toy_emissions_profile_any_companies())
  toy_sector_profile_any_scenarios <- read_csv(toy_sector_profile_any_scenarios())
  toy_sector_profile_companies <- read_csv(toy_sector_profile_companies())
  toy_europages_companies <- read_csv(toy_europages_companies())
  toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
  toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
  toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
  toy_isic_name <- read_csv(toy_isic_name())

  emissions_profile <- profile_emissions(
    companies = toy_emissions_profile_any_companies,
    co2 = toy_emissions_profile_products_ecoinvent,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )

  sector_profile <- profile_sector(
    companies = toy_sector_profile_companies,
    scenarios = toy_sector_profile_any_scenarios,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )

  expect_error(
    score_transition_risk(emissions_profile, sector_profile, include_co2 = TRUE),
    "tiltIndicatorAfter.output_co2_footprint"
  )
})

test_that("with `*.output_co2_footprint` unset, `include_co2 = FALSE` yields no error", {
  unset <- NULL
  withr::local_options(list(tiltIndicatorAfter.output_co2_footprint = unset))

  toy_emissions_profile_products_ecoinvent <- read_csv(toy_emissions_profile_products_ecoinvent())
  toy_emissions_profile_any_companies <- read_csv(toy_emissions_profile_any_companies())
  toy_sector_profile_any_scenarios <- read_csv(toy_sector_profile_any_scenarios())
  toy_sector_profile_companies <- read_csv(toy_sector_profile_companies())
  toy_europages_companies <- read_csv(toy_europages_companies())
  toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
  toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
  toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
  toy_isic_name <- read_csv(toy_isic_name())

  emissions_profile <- profile_emissions(
    companies = toy_emissions_profile_any_companies,
    co2 = toy_emissions_profile_products_ecoinvent,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )

  sector_profile <- profile_sector(
    companies = toy_sector_profile_companies,
    scenarios = toy_sector_profile_any_scenarios,
    europages_companies = toy_europages_companies,
    ecoinvent_activities = toy_ecoinvent_activities,
    ecoinvent_europages = toy_ecoinvent_europages,
    isic = toy_isic_name
  )

  expect_no_error(
    score_transition_risk(
      emissions_profile,
      sector_profile
    )
  )
})
