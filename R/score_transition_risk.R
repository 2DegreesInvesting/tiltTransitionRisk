#' Transition Risk Score
#'
#' Calulate Transition Risk Score at product level and company level
#'
#' @param emissions_profile Nested data frame. The output of
#'   `profile_emissions()`.
#' @param sector_profile Nested data frame. The output of `profile_sector()`.
#' @param include_co2 Logical. Include `co2_*` columns ?
#'
#' @return A data frame with the column `companies_id`, and the nested
#'   columns`product` and `company` holding the outputs at product and company
#'   level.
#'
#' @family top-level functions
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' library(readr, warn.conflicts = FALSE)
#' library(dplyr, warn.conflicts = FALSE)
#' library(tiltToyData, warn.conflicts = FALSE)
#' library(tiltIndicator)
#' library(tiltIndicatorAfter)
#'
#' set.seed(123)
#' restore <- options(list(
#'   readr.show_col_types = FALSE,
#'   tiltIndicatorAfter.output_co2_footprint = TRUE
#' ))
#'
#' toy_emissions_profile_products_ecoinvent <- read_csv(toy_emissions_profile_products_ecoinvent())
#' toy_emissions_profile_any_companies <- read_csv(toy_emissions_profile_any_companies())
#' toy_sector_profile_any_scenarios <- read_csv(toy_sector_profile_any_scenarios())
#' toy_sector_profile_companies <- read_csv(toy_sector_profile_companies())
#' toy_europages_companies <- read_csv(toy_europages_companies())
#' toy_ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
#' toy_ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
#' toy_ecoinvent_inputs <- read_csv(toy_ecoinvent_inputs())
#' toy_isic_name <- read_csv(toy_isic_name())
#'
#' emissions_profile <- profile_emissions(
#'   companies = toy_emissions_profile_any_companies,
#'   co2 = toy_emissions_profile_products_ecoinvent,
#'   europages_companies = toy_europages_companies,
#'   ecoinvent_activities = toy_ecoinvent_activities,
#'   ecoinvent_europages = toy_ecoinvent_europages,
#'   isic = toy_isic_name
#' )
#'
#' sector_profile <- profile_sector(
#'   companies = toy_sector_profile_companies,
#'   scenarios = toy_sector_profile_any_scenarios,
#'   europages_companies = toy_europages_companies,
#'   ecoinvent_activities = toy_ecoinvent_activities,
#'   ecoinvent_europages = toy_ecoinvent_europages,
#'   isic = toy_isic_name
#' )
#'
#' result <- score_transition_risk(emissions_profile,
#'   sector_profile,
#'   include_co2 = TRUE
#' )
#'
#' result |> unnest_product()
#'
#' result |> unnest_company()
#'
#' # Cleanup
#' options(restore)
score_transition_risk <- function(emissions_profile,
                                  sector_profile,
                                  include_co2 = FALSE) {
  if (include_co2) {
    hint <- "Do you need `options(tiltIndicatorAfter.output_co2_footprint = TRUE)`?"
    unnest_product(emissions_profile) |> check_col("co2_footprint", hint)
  }

  emissions_profile_at_product_level <- unnest_product(emissions_profile)
  sector_profile_at_product_level <- unnest_product(sector_profile)

  union_emissions_sector_rows <-
    get_rows_union_for_common_cols(
      emissions_profile_at_product_level,
      sector_profile_at_product_level
    )

  trs_emissions <-
    prepare_trs_emissions(emissions_profile_at_product_level, include_co2)
  trs_sector <-
    prepare_trs_sector(sector_profile_at_product_level)

  trs_product <-
    full_join_emmissions_sector(trs_emissions, trs_sector) |>
    add_transition_risk_score(
      col_ranking = col_ranking(),
      col_target = "reduction_targets"
    ) |>
    create_benchmarks_tr_score() |>
    limit_transition_risk_score_between_0_and_1() |>
    left_join(
      union_emissions_sector_rows,
      by = c("companies_id", "ep_product", "activity_uuid_product_uuid"),
      relationship = "many-to-many"
    ) |>
    relocate(relocate_trs_columns_product(include_co2)) |>
    distinct()

  emissions_profile_at_company_level <- unnest_company(emissions_profile) |>
    select(c(
      "companies_id",
      "benchmark",
      "emission_profile",
      "emission_profile_share",
      "profile_ranking_avg",
      if (include_co2) "co2_avg"
    ))

  sector_profile_at_company_level <- unnest_company(sector_profile) |>
    select(c(
      "companies_id",
      "sector_profile",
      "sector_profile_share",
      "scenario",
      "year",
      "reduction_targets_avg"
    ))

  process_transition_risk_company <- trs_product |>
    select(common_columns_emissions_sector_at_company_level(), "benchmark_tr_score", product_level_trs_column()) |>
    distinct() |>
    create_trs_average() |>
    select(-product_level_trs_column()) |>
    rename(benchmark_tr_score_avg = "benchmark_tr_score") |>
    distinct()

  trs_company <- emissions_profile_at_company_level |>
    left_join(
      sector_profile_at_company_level,
      relationship = "many-to-many",
      by = c("companies_id")
    ) |>
    add_benchmark_tr_score_avg() |>
    left_join(
      process_transition_risk_company,
      by = c("companies_id", "benchmark_tr_score_avg")
    ) |>
    relocate(relocate_trs_columns_company(include_co2)) |>
    distinct()

  nest_levels(trs_product, trs_company)
}

create_benchmarks_tr_score <- function(data) {
  mutate(
    data,
    benchmark_tr_score = ifelse(
      is.na(.data$profile_ranking) | is.na(.data$reduction_targets),
      NA_character_,
      paste(.data$scenario, .data$year, .data$benchmark, sep = "_")
    )
  )
}

create_trs_average <- function(data) {
  mutate(
    data,
    transition_risk_score_avg = ifelse(
      is.na(.data$benchmark_tr_score),
      NA_real_,
      mean(.data$transition_risk_score, na.rm = TRUE)
    ),
    .by = c("companies_id", "benchmark_tr_score")
  )
}

limit_transition_risk_score_between_0_and_1 <- function(data) {
  mutate(data, transition_risk_score = pmin(pmax(data$transition_risk_score, 0), 1))
}

add_benchmark_tr_score_avg <- function(data) {
  mutate(
    data,
    benchmark_tr_score_avg = ifelse(
      is.na(.data$profile_ranking_avg) | is.na(.data$reduction_targets_avg),
      NA_character_,
      paste(.data$scenario, .data$year, .data$benchmark, sep = "_")
    )
  )
}
