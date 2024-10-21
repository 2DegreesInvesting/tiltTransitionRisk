coefficient_of_variation_transition_risk_profile <- function(data) {
  product <- data |>
    unnest_product()

  company <- data |>
    unnest_company() |>
    add_coefficient_of_variation_transition_risk() |>
    polish_coefficient_of_variation_transition_risk() |>
    add_coefficient_of_variation_emission_rank() |>
    polish_coefficient_of_variation_emission_rank() |>
    add_coefficient_of_variation_sector_target() |>
    polish_coefficient_of_variation_sector_target()

  tilt_profile(nest_levels(product, company))
}

add_coefficient_of_variation_transition_risk <- function(data) {
  data |>
    add_mean_cov(
      "mean_transition_risk",
      "avg_transition_risk_equal_weight",
      "avg_transition_risk_best_case",
      "avg_transition_risk_worst_case"
    ) |>
    add_standard_deviation_cov(
      "standard_deviation_transition_risk",
      "mean_transition_risk",
      "avg_transition_risk_equal_weight",
      "avg_transition_risk_best_case",
      "avg_transition_risk_worst_case"
    ) |>
    add_coefficient_of_variation(
      "cov_transition_risk",
      "standard_deviation_transition_risk",
      "mean_transition_risk"
    )
}

polish_coefficient_of_variation_transition_risk <- function(data) {
  data |>
    select(-c("mean_transition_risk", "standard_deviation_transition_risk"))
}

add_coefficient_of_variation_emission_rank <- function(data) {
  data |>
    add_mean_cov(
      "mean_emission_rank",
      "profile_ranking_avg",
      "avg_profile_ranking_best_case",
      "avg_profile_ranking_worst_case"
    ) |>
    add_standard_deviation_cov(
      "standard_deviation_emission_rank",
      "mean_emission_rank",
      "profile_ranking_avg",
      "avg_profile_ranking_best_case",
      "avg_profile_ranking_worst_case"
    ) |>
    add_coefficient_of_variation(
      "cov_emission_rank",
      "standard_deviation_emission_rank",
      "mean_emission_rank"
    )
}

polish_coefficient_of_variation_emission_rank <- function(data) {
  data |>
    select(-c("mean_emission_rank", "standard_deviation_emission_rank"))
}

add_coefficient_of_variation_sector_target <- function(data) {
  data |>
    add_mean_cov(
      "mean_sector_target",
      "reduction_targets_avg",
      "avg_reduction_targets_best_case",
      "avg_reduction_targets_worst_case"
    ) |>
    add_standard_deviation_cov(
      "standard_deviation_sector_target",
      "mean_sector_target",
      "reduction_targets_avg",
      "avg_reduction_targets_best_case",
      "avg_reduction_targets_worst_case"
    ) |>
    add_coefficient_of_variation(
      "cov_sector_target",
      "standard_deviation_sector_target",
      "mean_sector_target"
    )
}

polish_coefficient_of_variation_sector_target <- function(data) {
  data |>
    select(-c("mean_sector_target", "standard_deviation_sector_target"))
}

add_coefficient_of_variation <- function(data, cov_col, sd_col, mean_col) {
  mutate(
    data,
    {{ cov_col }} := case_when(
      is.na(.data[[mean_col]]) ~ NA_real_,
      .data[[mean_col]] == 0.0 ~ NA_real_,
      TRUE ~ (.data[[sd_col]] / .data[[mean_col]]) * 100
    )
  )
}

add_mean_cov <- function(data, mean_col, col1, col2, col3) {
  mutate(
    data,
    {{ mean_col }} := ifelse(is.na(.data[[col1]]),
      NA_real_,
      (.data[[col1]] + .data[[col2]] + .data[[col3]]) / 3
    )
  )
}

# Denominator is `n` and not `n-1` because we are not calculating the standard
# deviation of a sample from whole population.
add_standard_deviation_cov <- function(data, sd_col, mean_col, col1, col2, col3) {
  mutate(
    data,
    {{ sd_col }} := ifelse(is.na(.data[[mean_col]]),
      NA_real_,
      round(sqrt(
        ((.data[[col1]] - .data[[mean_col]])^2 +
          (.data[[col2]] - .data[[mean_col]])^2 +
          (.data[[col3]] - .data[[mean_col]])^2) / 3
      ), 8)
    )
  )
}
