#' Adds low and high thresholds for transition risk score
#'
#' @param co2 A dataframe
#' @param all_activities_scenario_sectors A dataframe
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
#' co2 <- read_csv(toy_emissions_profile_products_ecoinvent())
#' all_activities_scenario_sectors <- read_csv(toy_all_uuids_scenario_sectors())
#' scenarios <- read_csv(toy_sector_profile_any_scenarios())
#'
#' output <- add_thresholds_transition_risk(
#'   co2,
#'   all_activities_scenario_sectors,
#'   scenarios
#' )
#' output
add_thresholds_transition_risk <- function(co2,
                                           all_activities_scenario_sectors,
                                           scenarios) {
  check_crucial_cols(co2, c(col_uuid(), col_co2_footprint()))
  check_crucial_cols(all_activities_scenario_sectors, c(
    col_uuid(), col_type(),
    col_sector(), col_subsector()
  ))
  check_crucial_cols(scenarios, c(
    col_type(), col_sector(), col_subsector(),
    col_year(), col_scenario(), col_targets()
  ))

  epa_profile_ranking <- epa_compute_profile_ranking(co2) |>
    select_crucial_ranking()

  spa_reduction_targets <- spa_compute_profile_ranking(
    all_activities_scenario_sectors,
    scenarios
  ) |>
    select_crucial_target()

  full_join(
    epa_profile_ranking,
    spa_reduction_targets,
    by = col_uuid(),
    relationship = "many-to-many"
  ) |>
    add_transition_risk_score(col_ranking(), col_targets()) |>
    add_benchmark_tr_score(col_ranking(), col_targets()) |>
    distinct() |>
    add_low_high_transition_risk_thresholds() |>
    select(-c(col_scenario(), col_year()))
}

#' Calulate `transition_risk_score` column
#'
#' @param data Dataframe.
#' @param profile_ranking Dataframe column.
#' @param reduction_targets Dataframe column.
#' @keywords internal
#' @export
add_transition_risk_score <- function(data,
                                      col_ranking = col_ranking(),
                                      col_target = col_targets()) {
  mutate(
    data,
    transition_risk_score = ifelse(
      is.na(.data[[col_ranking]]) | is.na(.data[[col_target]]),
      NA,
      (.data[[col_ranking]] + .data[[col_target]]) / 2
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
                                   col_ranking = col_ranking(),
                                   col_target = col_targets()) {
  mutate(
    data,
    benchmark_tr_score = ifelse(
      is.na(.data[[col_ranking]]) | is.na(.data[[col_target]]),
      NA,
      paste(.data$scenario, .data$year, .data$grouped_by, sep = "_")
    )
  )
}

add_low_high_transition_risk_thresholds <- function(data) {
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
  select(data, all_of(c(col_uuid(), col_scenario(), col_year(), col_targets())))
}

select_crucial_ranking <- function(data) {
  select(data, all_of(c(col_uuid(), "grouped_by", col_ranking())))
}

check_crucial_cols <- function(data, crucial_cols) {
  walk(crucial_cols, ~ check_matches_name(data, .x))
}
