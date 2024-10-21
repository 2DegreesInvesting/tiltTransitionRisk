#' Excluding irrelevant columns and duplicates, then pivot from long to wide
#'
#' @param data A data frame to pivot.
#' @param ... Arguments passed to [tidyr::pivot_wider()].
#' @param exclude_cols A character vector giving regular expressions matching
#'   column names to exclude. If lengh > 1, the union is taken.
#' @param avoid_list_cols Logical. Avoid all list-columns, duplicates, and the
#'   associated warning?
#'
#' @return A data frame giving the result you get from [tidyr::pivot_wider()] if
#'   `data` lacks the excluded columns and the resulting duplicates.
#' @export
#' @keywords internal
#'
#' @examples
#' library(tidyr, warn.conflicts = FALSE)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # styler: off
#' data <- tribble(
#'   ~to_exclude,  ~id, ~name,  ~value,
#'             1, "id",   "a",       1,
#'             2, "id",   "a",       1,
#'             1, "id",   "b",       2,
#'             2, "id",   "b",       2,
#' )
#' # styler: on
#'
#' # `exclude_cols_then_pivot_wider()` excludes columns and duplicates
#' data |> exclude_cols_then_pivot_wider(exclude_cols = "exclude")
#'
#' # Why is this useful?
#' # `pivot_wider()` defaults to using all columns
#' data |> pivot_wider()
#'
#' # You may specify relevant columns but the result may still have duplicates
#' data |>
#'   pivot_wider(id_cols = id, names_from = "name", values_from = "value") |>
#'   unnest(c(a, b))
#'
#' # styler: off
#' data <- tribble(
#'    ~id, ~name, ~value, ~to_exclude, ~yields_duplicates,
#'   "id",   "a",      1,           1,                  1,
#'   "id",   "a",      1,           2,                  2
#' )
#' # styler: on
#'
#' # `data` may have columns that yield duplicates and thus list-columns
#' with_list_cols <- exclude_cols_then_pivot_wider(
#'   data,
#'   exclude_cols = "to_exclude",
#'   id_cols = "id"
#' )
#' # You can handle it after the fact
#' with_list_cols |>
#'   tidyr::unnest(everything()) |>
#'   distinct()
#'
#' # But also you can avoid it with `avoid_list_cols = TRUE`
#' exclude_cols_then_pivot_wider(
#'   data,
#'   exclude_cols = "to_exclude",
#'   id_cols = "id",
#'   avoid_list_cols = TRUE
#' )
exclude_cols_then_pivot_wider <- function(data,
                                          ...,
                                          exclude_cols = NULL,
                                          avoid_list_cols = FALSE) {
  pruned <- data |>
    select(-matches(exclude_cols)) |>
    distinct()

  if (!avoid_list_cols) {
    pruned |> pivot_wider(...)
  } else {
    check_values_fn(...)

    pruned |>
      pivot_wider(..., values_fn = list) |>
      tidyr::unchop(tidyselect::everything()) |>
      distinct()
  }
}

check_values_fn <- function(...) {
  if (hasName(list(...), "values_fn")) {
    abort("`values_fn` should not be used when `avoid_list_cols = TRUE`.")
  }
}
