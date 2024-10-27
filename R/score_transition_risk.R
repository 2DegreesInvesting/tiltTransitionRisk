#' Transition Risk Score
#'
#' Calulate Transition Risk Score at product level and company level
#'
#' @param emissions_profile_at_product_level Dataframe. Emissions profile product level output
#' @param sector_profile_at_product_level Dataframe. Sector profile product level output
#'
#' @family top-level functions
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' library(dplyr)
#' library(readr, warn.conflicts = FALSE)
#' library(tiltToyData)
#' library(tiltIndicator)
#' library(tiltIndicatorAfter)
#'
#' restore <- options(readr.show_col_types = FALSE)
#'
#' emissions_companies <- read_csv(toy_emissions_profile_any_companies())
#' products <- read_csv(toy_emissions_profile_products_ecoinvent())
#' europages_companies <- read_csv(toy_europages_companies())
#' ecoinvent_activities <- read_csv(toy_ecoinvent_activities())
#' ecoinvent_europages <- read_csv(toy_ecoinvent_europages())
#' isic_name <- read_csv(toy_isic_name())
#'
#' emissions_profile_at_product_level <- profile_emissions(
#'   companies = emissions_companies,
#'   co2 = products,
#'   europages_companies = europages_companies,
#'   ecoinvent_activities = ecoinvent_activities,
#'   ecoinvent_europages = ecoinvent_europages,
#'   isic = isic_name
#' ) |> unnest_product()
#'
#' sector_companies <- read_csv(toy_sector_profile_companies())
#' scenarios <- read_csv(toy_sector_profile_any_scenarios())
#'
#' sector_profile_at_product_level <- profile_sector(
#'   companies = sector_companies,
#'   scenarios = scenarios,
#'   europages_companies = europages_companies,
#'   ecoinvent_activities = ecoinvent_activities,
#'   ecoinvent_europages = ecoinvent_europages,
#'   isic = isic_name
#' ) |> unnest_product()
#'
#' result <- score_transition_risk(emissions_profile_at_product_level, sector_profile_at_product_level)
#'
#' result |> unnest_product()
#'
#' result |> unnest_company()
#'
#' # Cleanup
#' options(restore)
score_transition_risk <-
  function(emissions_profile_at_product_level,
           sector_profile_at_product_level) {
    union_emissions_sector_rows <-
      get_rows_union_for_common_cols(
        emissions_profile_at_product_level,
        sector_profile_at_product_level
      )
    trs_emissions <-
      prepare_trs_emissions(emissions_profile_at_product_level)
    trs_sector <-
      prepare_trs_sector(sector_profile_at_product_level)

    trs_product <-
      full_join_emmissions_sector(trs_emissions, trs_sector) |>
      create_tr_benchmarks_tr_score() |>
      limit_transition_risk_score_between_0_and_1() |>
      select(-all_of(c("scenario_year", "benchmark"))) |>
      left_join(
        union_emissions_sector_rows,
        by = c("companies_id", "ep_product", "activity_uuid_product_uuid"),
        relationship = "many-to-many"
      ) |>
      relocate(
        relocate_trs_columns(product_level_trs_column()),
        "profile_ranking",
        "reduction_targets"
      ) |>
      distinct()

    trs_company <- trs_product |>
      select(common_columns_emissions_sector_at_company_level(), "benchmark_tr_score", product_level_trs_column()) |>
      create_trs_average() |>
      select(-product_level_trs_column()) |>
      relocate(relocate_trs_columns(company_level_trs_avg_column())) |>
      rename(benchmark_tr_score_avg = "benchmark_tr_score") |>
      distinct()

    nest_levels(trs_product, trs_company)
  }

create_tr_benchmarks_tr_score <- function(data) {
  mutate(
    data,
    transition_risk_score = ifelse(
      is.na(.data$profile_ranking) | is.na(.data$reduction_targets),
      NA,
      (.data$profile_ranking + .data$reduction_targets) / 2
    ),
    benchmark_tr_score = ifelse(
      is.na(.data$profile_ranking) | is.na(.data$reduction_targets),
      NA,
      paste(.data$scenario_year, .data$benchmark, sep = "_")
    )
  )
}

create_trs_average <- function(data) {
  mutate(
    data,
    transition_risk_score_avg = mean(.data$transition_risk_score, na.rm = TRUE),
    .by = c("companies_id", "benchmark_tr_score")
  )
}

limit_transition_risk_score_between_0_and_1 <- function(data) {
  mutate(data, transition_risk_score = pmin(pmax(data$transition_risk_score, 0), 1))
}
