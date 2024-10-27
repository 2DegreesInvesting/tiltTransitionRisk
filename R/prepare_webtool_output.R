prepare_webtool_output <- function(data,
                                   for_webtool = FALSE,
                                   include_co2 = FALSE) {
  if (for_webtool) {
    prepare_webtool_output_impl(data, include_co2 = include_co2)
  }
  else {
    data
  }
}

prepare_webtool_output_impl <- function(data, include_co2 = FALSE) {
  product <- data |>
    unnest_product() |>
    select_webtool_cols_at_product_level()

  company <- data |>
    unnest_company() |>
    select_webtool_cols_at_company_level_wide() |>
    rename_webtool_cols_at_company_level_wide()

  if (include_co2) {
    product <- product |>
      select(-all_of(c("co2_footprint")))

    company <- company |>
      select(-all_of(c("co2e_avg")))
  }

  tilt_profile(nest_levels(product, company))
}

polish_transition_risk_profile <- function(data) {
  product <- data |>
    unnest_product() |>
    rename_transition_risk_profile_cols_product()

  company <- data |>
    unnest_company() |>
    rename_transition_risk_profile_cols_company()

  tilt_profile(nest_levels(product, company))
}
