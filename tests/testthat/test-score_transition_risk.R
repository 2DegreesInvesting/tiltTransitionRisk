test_that("outputs expected columns at product level", {
  emissions_profile_at_product_level <-
    example_emissions_profile_at_product_level() |>
    filter(companies_id == "antimonarchy_canine")
  sector_profile_at_product_level <-
    example_sector_profile_at_product_level() |>
    filter(companies_id == "antimonarchy_canine")

  out <-
    score_transition_risk(
      emissions_profile_at_product_level,
      sector_profile_at_product_level
    ) |>
    unnest_product()

  expect_equal(sort(names(out)), sort(trs_product_output_columns()))
})

test_that("outputs expected columns at company level", {
  emissions_profile_at_product_level <-
    example_emissions_profile_at_product_level() |>
    filter(companies_id == "antimonarchy_canine")
  sector_profile_at_product_level <-
    example_sector_profile_at_product_level() |>
    filter(companies_id == "antimonarchy_canine")

  out <-
    score_transition_risk(
      emissions_profile_at_product_level,
      sector_profile_at_product_level
    ) |>
    unnest_company()

  expect_equal(sort(names(out)), sort(trs_company_output_columns()))
})

test_that("calculates `transition_risk_score` and `benchmark_tr_score` correctly", {
  emissions_profile_at_product_level <-
    example_emissions_profile_at_product_level() |>
    filter(
      companies_id %in% c("antimonarchy_canine"),
      benchmark == "all"
    )
  sector_profile_at_product_level <-
    example_sector_profile_at_product_level() |>
    filter(
      companies_id %in% c("antimonarchy_canine"),
      scenario == "1.5C RPS",
      year == "2030"
    )

  out <-
    unnest_product(
      score_transition_risk(
        emissions_profile_at_product_level,
        sector_profile_at_product_level
      )
    )

  expect_equal(out$benchmark_tr_score, "1.5C RPS_2030_all")
  expect_equal(out$transition_risk_score, 0.59)
})

test_that("calculates `transition_risk_score_avg` correctly", {
  emissions_profile_at_product_level <-
    example_emissions_profile_at_product_level() |>
    filter(
      companies_id %in% c("nonphilosophical_llama"),
      benchmark == "all"
    )
  sector_profile_at_product_level <-
    example_sector_profile_at_product_level() |>
    filter(
      companies_id %in% c("nonphilosophical_llama"),
      scenario == "1.5C RPS",
      year == "2030"
    )

  out <-
    unnest_company(
      score_transition_risk(
        emissions_profile_at_product_level,
        sector_profile_at_product_level
      )
    )

  expect_equal(round(out$transition_risk_score_avg, 4), 0.2117)
})

test_that(
  "calculates `transition_risk_score_avg` correctly for unmatched `ep_product`
          of a company",
  {
    emissions_profile_at_product_level <-
      example_emissions_profile_at_product_level() |>
      filter(
        companies_id %in% c("nonphilosophical_llama"),
        ep_product == "surface finishing, galvanic"
      )
    sector_profile_at_product_level <-
      example_sector_profile_at_product_level() |>
      filter(
        companies_id %in% c("nonphilosophical_llama"),
        ep_product == "surface engineering"
      )

    out <-
      unnest_company(
        score_transition_risk(
          emissions_profile_at_product_level,
          sector_profile_at_product_level
        )
      )

    # Both the ep_products are present only in one dataframe which will lead to
    # unmatched results and thereafter Null in `transition_risk_score_avg` column
    expect_equal(out$transition_risk_score_avg, NaN)
  }
)


test_that(
  "`transition_risk_score` and `benchmark_tr_score` has NA due to
          NA in either column `profile_ranking` or `reduction_targets`",
  {
    emissions_profile_at_product_level <-
      example_emissions_profile_at_product_level() |>
      filter(companies_id %in% c("antimonarchy_canine", "nonphilosophical_llama"))
    sector_profile_at_product_level <-
      example_sector_profile_at_product_level() |>
      filter(companies_id %in% c("celestial_lovebird", "nonphilosophical_llama"))

    out <-
      score_transition_risk(
        emissions_profile_at_product_level,
        sector_profile_at_product_level
      ) |>
      unnest_product()

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
      example_emissions_profile_at_product_level() |>
      filter(companies_id %in% c("antimonarchy_canine", "nonphilosophical_llama"))
    sector_profile_at_product_level <-
      example_sector_profile_at_product_level() |>
      filter(companies_id %in% c("celestial_lovebird", "nonphilosophical_llama"))

    trs_product <-
      score_transition_risk(
        emissions_profile_at_product_level,
        sector_profile_at_product_level
      ) |>
      unnest_product()

    trs_company <-
      score_transition_risk(
        emissions_profile_at_product_level,
        sector_profile_at_product_level
      ) |>
      unnest_company()

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

test_that("characterize commit 663d12", {
  emissions_profile_at_product_level <-
    example_emissions_profile_at_product_level() |>
    filter(companies_id == "antimonarchy_canine")
  sector_profile_at_product_level <-
    example_sector_profile_at_product_level() |>
    filter(companies_id == "antimonarchy_canine")

  out <-
    score_transition_risk(
      emissions_profile_at_product_level,
      sector_profile_at_product_level
    )

  out |>
    unnest_product() |>
    hasName("benchmark_tr_score") |>
    expect_true()

  out |>
    unnest_company() |>
    hasName("benchmark_tr_score_avg") |>
    expect_true()
})

test_that("limits `transition_risk_score` between 0 and 1", {
  emissions_profile_at_product_level <-
    example_emissions_profile_at_product_level() |>
    filter(companies_id == "antimonarchy_canine")
  sector_profile_at_product_level <-
    example_sector_profile_at_product_level() |>
    filter(companies_id == "antimonarchy_canine") |>
    mutate(reduction_targets = c(-100, -100, 100, 100))

  out <-
    score_transition_risk(
      emissions_profile_at_product_level,
      sector_profile_at_product_level
    ) |>
    unnest_product()

  # Due to large positive and negative `reduction_targets` values,
  # `transition_risk_score` should not be more than 1 and less than 0.
  expected_values <- c(0, 1)
  expect_equal(unique(out$transition_risk_score), expected_values)
})
