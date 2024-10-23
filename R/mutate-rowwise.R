#' Mutate (rowwise)
#'
#' A shortcut to save typing. Performs mutate operations "rowwise" and
#' `ungroup()`s the data frame afterwards.
#'
#' @param .data `.data` argument from `dplyr::mutate()`.
#' @param ... data-masking name-value pairs from `dplyr::mutate()`.
#'
#' @return An (ungrouped) object of the same type as `.data`.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#'
#' data("starwars")
#'
#' starwars |>
#'   tidyr::nest(.by = sex) |>
#'   mutate_rowwise(
#'     model = list(
#'       lm(mass ~ height, data)
#'     )
#'   )
mutate_rowwise <- function(.data, ...) {
  df_rowwise <- dplyr::rowwise(.data)

  df_mutated <- dplyr::mutate(df_rowwise, ...)

  out <- dplyr::ungroup(df_mutated)

  out
}
