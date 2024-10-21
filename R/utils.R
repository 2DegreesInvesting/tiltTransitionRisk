#' @examples
#' this_data <- tibble(x = 1)
#' this_data |> check_col("y", hint = "Did you forget something?")
#' @noRd
check_col <- function(data, col, hint = NULL) {
  if (!hasName(data, col)) {
    label <- deparse(substitute(data))
    abort(c(glue("`{label}` must have the column `{col}`."), i = hint))
  }

  invisible(data)
}
