round_off_to_4_decimal_places <- function(data) {
  product <- data |>
    unnest_product() |>
    round_off_to_4_decimal_places_impl()

  company <- data |>
    unnest_company() |>
    round_off_to_4_decimal_places_impl()

  tilt_profile(nest_levels(product, company))
}

round_off_to_4_decimal_places_impl <- function(data) {
  data |>
    mutate(across(where(is.numeric), ~ round(.x, 4)))
}
