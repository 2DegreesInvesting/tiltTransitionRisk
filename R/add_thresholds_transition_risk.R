#' Adds low and high thresholds for transition risk score
#'
#' @param emissions_profile_products A dataframe
#' @param all_uuids_scenario_sectors A dataframe
#' @param scenarios A dataframe
#'
#' @keywords internal
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' library(tiltToyData)
#' library(readr)
#' library(dplyr)
#' options(readr.show_col_types = FALSE)
#'
#' emissions_profile_products <- read_csv(toy_emissions_profile_products_ecoinvent())
#' all_uuids_scenario_sectors <- read_csv(toy_sector_profile_companies()) |>
#'   select(-c("companies_id", "company_name", "clustered")) |>
#'   distinct()
#' scenarios <- read_csv(toy_sector_profile_any_scenarios())
#'
#' output <- add_thresholds_transition_risk(
#'   emissions_profile_products,
#'   all_uuids_scenario_sectors,
#'   scenarios
#' )
#' output
add_thresholds_transition_risk <- function(emissions_profile_products,
                                           all_uuids_scenario_sectors,
                                           scenarios) {
  epa_profile_ranking <- epa_compute_profile_ranking(emissions_profile_products) |>
    select_crucial_ranking()

  spa_reduction_targets <- spa_compute_profile_ranking(
    all_uuids_scenario_sectors,
    scenarios
  ) |>
    select_crucial_target()

  full_join(
    epa_profile_ranking,
    spa_reduction_targets,
    by = c("activity_uuid_product_uuid"),
    relationship = "many-to-many"
  ) |>
    add_transition_risk_score("profile_ranking", "reductions") |>
    add_benchmark_tr_score("profile_ranking", "reductions") |>
    distinct() |>
    add_low_high_transition_risk_thresholds() |>
    select(-c("scenario", "year"))
}

#' Calulate `transition_risk_score` column
#'
#' @param data Dataframe.
#' @param profile_ranking Dataframe column.
#' @param reduction_targets Dataframe column.
#' @keywords internal
#' @export
add_transition_risk_score <- function(data,
                                      col_ranking = "profile_ranking",
                                      col_target = "reductions") {
  mutate(
    data,
    transition_risk_score = ifelse(
      is.na(data[[col_ranking]]) | is.na(data[[col_target]]),
      NA,
      (data[[col_ranking]] + data[[col_target]]) / 2
    )
  )
}

#' Calulate `benchmark_tr_score` column
#'
#' @param data Dataframe.
#' @param profile_ranking Dataframe column.
#' @param reduction_targets Dataframe column.
#' @keywords internal
#' @export
add_benchmark_tr_score <- function(data,
                                   col_ranking = "profile_ranking",
                                   col_target = "reductions") {
  mutate(
    data,
    benchmark_tr_score = ifelse(
      is.na(data[[col_ranking]]) | is.na(data[[col_target]]),
      NA,
      paste(.data$scenario, .data$year, .data$grouped_by, sep = "_")
    )
  )
}

add_low_high_transition_risk_thresholds <- function(data, .by) {
  mutate(data,
    transition_risk_low_threshold = quantile_distribute(.data$transition_risk_score, 1),
    transition_risk_high_threshold = quantile_distribute(.data$transition_risk_score, 2),
    .by = "benchmark_tr_score"
  )
}

quantile_distribute <- function(x, which) {
  out <- quantile(x, probs = c(1 / 3, 2 / 3), na.rm = TRUE)
  out[[which]]
}

select_crucial_target <- function(data) {
  data |>
    select(c("activity_uuid_product_uuid", "scenario", "year", "reductions"))
}

select_crucial_ranking <- function(data) {
  data |>
    select(c("activity_uuid_product_uuid", "grouped_by", "profile_ranking"))
}
