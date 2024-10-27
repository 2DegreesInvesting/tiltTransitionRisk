#' Add transition risk score and polish the output for delivery
#'
#' @param emissions_profile Nested data frame. The output of
#'   `profile_emissions()`.
#' @param sector_profile Nested data frame. The output of `profile_sector()`.
#' @param include_co2 Logical. Include `co2_*` columns ?
#'
#' @return A data frame with the column `companies_id`, and the nested
#'   columns`product` and `company` holding the outputs at product and company
#'   level.
#' @export
#' @keywords internal
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
#' result <- score_transition_risk_and_polish(emissions_profile,
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
score_transition_risk_and_polish <- function(emissions_profile,
                                             sector_profile,
                                             include_co2 = FALSE) {
  transition_risk_score <- score_transition_risk(
    unnest_product(emissions_profile),
    unnest_product(sector_profile)
  )

  if (include_co2) {
    hint <- "Do you need `options(tiltIndicatorAfter.output_co2_footprint = TRUE)`?"
    unnest_product(emissions_profile) |> check_col("co2_footprint", hint)
  }

  select_emissions_profile_product <- unnest_product(emissions_profile) |>
    select(
      c(
        "companies_id",
        "country",
        "main_activity",
        "ep_product",
        "postcode",
        "address",
        "activity_uuid_product_uuid",
        "matched_activity_name",
        "matched_reference_product",
        "unit",
        "co2e_lower",
        "co2e_upper",
        "emission_profile",
        "benchmark",
        "profile_ranking",
        "tilt_sector",
        "tilt_subsector",
        "min_headcount",
        "max_headcount",
        "emissions_profile_best_case",
        "emissions_profile_worst_case",
        "isic_4digit",
        "matching_certainty",
        "company_name",
        "emissions_profile_equal_weight",
        if (include_co2) "co2_footprint"
      )
    )
  select_sector_profile_product <- unnest_product(sector_profile) |>
    select(
      c(
        "companies_id",
        "ep_product",
        "sector_profile",
        "scenario",
        "year",
        "reduction_targets",
        "tilt_sector",
        "tilt_subsector",
        "isic_4digit",
        "sector_profile_best_case",
        "sector_profile_worst_case",
        "sector_profile_equal_weight"
      )
    )

  select_transition_risk_score_product <- unnest_product(transition_risk_score) |>
    select(c(
      "companies_id",
      "ep_product",
      "benchmark_tr_score",
      "transition_risk_score"
    ))

  out_product <- select_emissions_profile_product |>
    left_join(
      select_sector_profile_product,
      relationship = "many-to-many",
      by = c("companies_id", "ep_product"),
      suffix = c(".emissions", ".sector")
    ) |>
    coalesce_common_col("tilt_sector", "emissions", "sector") |>
    coalesce_common_col("tilt_subsector", "emissions", "sector") |>
    coalesce_common_col("isic_4digit", "emissions", "sector") |>
    mutate(benchmark_tr_score = ifelse(
      is.na(.data$profile_ranking) | is.na(.data$reduction_targets),
      NA_character_,
      paste(.data$scenario, .data$year, .data$benchmark, sep = "_")
    )) |>
    left_join(
      select_transition_risk_score_product,
      by = c("companies_id", "ep_product", "benchmark_tr_score"),
      relationship = "many-to-many"
    ) |>
    distinct()

  select_emissions_profile_company <- unnest_company(emissions_profile) |>
    select(
      c(
        "companies_id",
        "company_name",
        "country",
        "main_activity",
        "postcode",
        "address",
        "benchmark",
        "min_headcount",
        "max_headcount",
        "emission_profile",
        "emission_profile_share",
        "profile_ranking_avg",
        if (include_co2) "co2_avg"
      )
    )

  select_sector_profile_company <- unnest_company(sector_profile) |>
    select(c("companies_id", "sector_profile", "sector_profile_share", "scenario", "year", "reduction_targets_avg"))

  select_transition_risk_score_company <- unnest_company(transition_risk_score) |>
    select(
      c(
        "companies_id",
        "benchmark_tr_score_avg",
        "transition_risk_score_avg"
      )
    )

  out_company <- select_emissions_profile_company |>
    left_join(
      select_sector_profile_company,
      relationship = "many-to-many",
      by = c("companies_id")
    ) |>
    add_benchmark_tr_score_avg() |>
    left_join(
      select_transition_risk_score_company,
      by = c("companies_id", "benchmark_tr_score_avg")
    ) |>
    distinct()

  nest_levels(out_product, out_company)
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

coalesce_common_col <- function(data, col, suffix1, suffix2) {
  col_suffix1 = paste(col, suffix1, sep = ".")
  col_suffix2 = paste(col, suffix2, sep = ".")
  data |>
    mutate({{ col }} := coalesce(.data[[col_suffix1]], .data[[col_suffix2]])) |>
    select(-all_of(c(col_suffix1, col_suffix2)))
}
