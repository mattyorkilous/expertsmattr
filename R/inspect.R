#' Inspect a row of a data frame in the global environment.
#'
#' `inspect_at()` and `inspect_where()` are designed to help debug calls to
#' `mutate_rowwise()`. Send the contents of a single data frame row to the
#' global environment for inspection, choosing the row using either
#' `dplyr::slice()` syntax (`inspect_at()`) or `dplyr::filter()` syntax
#' (`inspect_where()`).
#'
#' @param .data `.data` argument from `dplyr::filter()` or `dplyr::slice()`.
#' @param ... data-masking arguments from `dplyr::filter()` or
#' `dplyr::slice()`.
#'
#' @return `.GlobalEnv`
#' @rdname inspect
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' data("starwars")
#'
#' starwars |>
#'   inspect_at(1)
#'
#' starwars |>
#'   inspect_where(name == "Darth Vader")
inspect_at <- function(.data, ...) {
  row <- dplyr::slice(dplyr::ungroup(.data), ...)

  send_row_to_globalenv(row)
}

#' @rdname inspect
#' @export
inspect_where <- function(.data, ...) {
  row <- dplyr::filter(dplyr::ungroup(.data), ...)

  send_row_to_globalenv(row)
}

send_row_to_globalenv <- function(row) {
  stopifnot(nrow(row) == 1)

  row_list <- purrr::map(row, 1)

  list2env(row_list, .GlobalEnv)
}
