#' Adds toy data for `all_activities_scenario_sectors`
#'
#' @keywords internal
#' @export
toy_all_activities_scenario_sectors <- function() {
  # Created with
  # readr::read_csv(tiltToyData::toy_sector_profile_companies()) |>
  #   dplyr::select(-c("companies_id", "company_name", "clustered")) |>
  #   dplyr::distinct() |>
  #   datapasta::tribble_paste()

  file <- tempfile(fileext = ".csv")

  tribble(
    # styler: on
    ~activity_uuid_product_uuid, ~isic_4digit,   ~tilt_sector,            ~tilt_subsector, ~type,     ~sector,       ~subsector,
    "76269c17-78d6-420b-991a-aa38c51b45b7",     "'4100'", "construction", "construction residential", "ipr", "buildings",               NA,
    "76269c17-78d6-420b-991a-aa38c51b45b7",     "'4100'", "construction", "construction residential", "weo",     "total",    "residential",
    "833caa78-30df-4374-900f-7f88ab44075b",     "'2591'",       "metals",             "other metals", "ipr",  "industry", "other industry",
    "833caa78-30df-4374-900f-7f88ab44075b",     "'2591'",       "metals",             "other metals", "weo",     "total",       "industry",
    "bf94b5a7-b7a2-46d1-bb95-84bc560b12fb",     "'2410'",       "metals",             "iron & steel", "ipr",  "industry", "iron and steel",
    "bf94b5a7-b7a2-46d1-bb95-84bc560b12fb",     "'2410'",       "metals",             "iron & steel", "weo",     "total", "iron and steel"
    # styler: off
  ) |>
    write.csv(file, row.names = FALSE)

  file
}
