#' DRAFT of transition risk
#'
#' @param products,products_scenarios,scenarios  TODO Data frames.
#' @param low_threshold,high_threshold  TODO Expressions.
#'
#' @return TODO A data frame
#' @export
#'
#' @examples
#' library(tiltToyData)
#' library(readr, warn.conflicts = FALSE)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' restore <- options(readr.show_col_types = FALSE)
#'
#' products <- read_csv(toy_emissions_profile_products_ecoinvent())
#' # TODO: Submit a PR to tiltToyData?
#' products_scenarios <- read_csv(toy_sector_profile_companies()) |>
#'   select(-c("companies_id", "company_name", "clustered")) |>
#'   distinct()
#' scenarios <- read_csv(toy_sector_profile_any_scenarios())
#'
#' transition_risk_profile(products, products_scenarios, scenarios)
#'
#' options(restore)
transition_risk_profile <- function(products,
                                    # TODO: Do we need a new dataset in tiltToyData?
                                    products_scenarios,
                                    scenarios,
                                    low_threshold = tercile(.data$transition_risk_score, 1),
                                    high_threshold = tercile(.data$transition_risk_score, 2)) {
  score_transition_risk(products, products_scenarios, scenarios) |>
    # Add thresholds
    mutate(
      transition_risk_low_threshold = !!rlang::enquo(low_threshold),
      transition_risk_high_threshold = !!rlang::enquo(high_threshold),
      .by = "benchmark_tr_score"
    ) |>
    # ... do something else?
    # Polish
    select(-c("scenario", "year"))
}

tercile <- function(x, which) {
  out <- quantile(x, probs = c(1 / 3, 2 / 3), na.rm = TRUE)
  out[[which]]
}

score_transition_risk <- function(products,
                                  products_scenarios,
                                  scenarios) {
  ranking <- epa_compute_profile_ranking(products)

  target <- spa_compute_profile_ranking(
    products_scenarios,
    scenarios
  )

  ranking_target <- full_join(
    ranking |> select_crucial_ranking(),
    target |> select_crucial_target(),
    by = c("activity_uuid_product_uuid"),
    relationship = "many-to-many"
  )

  ranking_target |>
    add_transition_risk_score("profile_ranking", "reductions") |>
    add_benchmark_tr_score() |>
    set_na_benchmark_tr_score() |>
    distinct()
}

select_crucial_target <- function(data) {
  data |>
    select(c("activity_uuid_product_uuid", "scenario", "year", "reductions")) |>
    # These two steps seem to belong closer to the edge
    mutate(scenario_year = paste(.data$scenario, .data$year, sep = "_"))
}

select_crucial_ranking <- function(data) {
  data |>
    select(c("activity_uuid_product_uuid", "grouped_by", "profile_ranking"))
}

add_transition_risk_score <- function(data,
                                      col_ranking = "profile_ranking",
                                      col_target = "reductions") {
  transition_risk <- function(x, target) {
    ifelse(is.na(x) | is.na(target), NA, (x + target) / 2)
  }

  x <- data[[col_ranking]]
  target <- data[[col_target]]
  mutate(data, transition_risk_score = transition_risk(x, target))
}

add_benchmark_tr_score <- function(data) {
  data |>
    mutate(benchmark_tr_score = paste(
      .data$scenario,
      .data$year,
      .data$grouped_by,
      sep = "_"
    ))
}

set_na_benchmark_tr_score <- function(data,
                                      col_ranking = "profile_ranking",
                                      col_target = "reductions") {
  data |>
    mutate(
      benchmark_tr_score = ifelse(
        is.na(.data[[col_ranking]]) | is.na(.data[[col_target]]),
        NA,
        .data$benchmark_tr_score
      )
    )
}
