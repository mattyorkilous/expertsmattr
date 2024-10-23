#' Bind one or many summary rows to a data frame
#'
#' `bind_summary_row()` handles the simple case of adding a total row to a data
#' frame, but with much greater flexibility. Specify one or many summary rows
#' using your own custom summary functions with `fns`, control placement of
#' labels with `labels_to` and `below`, or add a summary row for each group with
#' `by`.
#'
#' @param data A data frame
#' @param cols A tidy-select selection of columns to summarize.
#' @param fns A named list of functions, where the name gives the label in the
#' column determined by `labels_to`.
#' @param labels_to A character vector of length one specifying the column to
#' (create if necessary and) put the labels in.
#' @param by A tidy-select selection of columns to summarize by.
#' @param below Default is `TRUE`. Binds total rows below the data. If `FALSE`,
#' binds total rows above the data.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' library(dplyr)
#' data("starwars")
#'
#' starwars |>
#'   select(name, height, mass) |>
#'   bind_summary_row(
#'     cols = c(height, mass),
#'     fns = list(
#'       "Total" = \(x) sum(x, na.rm = TRUE)
#'     ),
#'     labels_to = "name",
#'     below = FALSE
#'   )
#'
#' starwars |>
#'   select(sex, name, height, mass) |>
#'   bind_summary_row(
#'     cols = c(height, mass),
#'     fns = list(
#'       "Maximum" = \(x) max(x, na.rm = TRUE),
#'       "Total" = \(x) sum(x, na.rm = TRUE)
#'     ),
#'     labels_to = " ",
#'     by = sex,
#'     below = FALSE
#'   )
#' @importFrom rlang .data `:=`
bind_summary_row <- function(data,
                             cols,
                             fns,
                             labels_to = NULL,
                             by = NULL,
                             below = TRUE) {
  if (is.null(labels_to)) {
    stop("Must choose a column for labels")
  }

  data_augmented <- apply_fns_to_cols(data, {{ cols }}, fns, {{ by }})

  cols_old <- colnames(data)

  cols_new <- setdiff(colnames(data_augmented), cols_old)

  data_nested <- tidyr::nest(
    data_augmented,
    .by = c({{ by }}, tidyr::all_of(cols_new))
  )

  summaries_nested <- nest_all_summary_cols(data_nested, fns, cols_new)

  summaries_formatted <- format_all_summaries(
    summaries_nested,
    fns,
    labels_to
  )

  if (below) {
    nested_ordered <- pivot_longer_order(
      summaries_formatted,
      tidyr::all_of(c("data", names(fns)))
    )
  } else {
    nested_ordered <- pivot_longer_order(
      summaries_formatted,
      tidyr::all_of(c(names(fns), "data"))
    )
  }

  out <- unnest_relocate(nested_ordered, {{ by }}, labels_to, cols_old)

  out
}

apply_fns_to_cols <- function(data, cols, fns, by) {
  data_augmented <- dplyr::mutate(
    data,
    dplyr::across(
      {{ cols }},
      fns,
      .names = "{.col}_{.fn}"
    ),
    .by = {{ by }}
  )

  data_augmented
}

nest_all_summary_cols <- function(data_nested, fns, cols_new) {
  summaries_nested <- purrr::reduce(
    names(fns),
    \(data, fn_name) nest_summary_cols(data, fn_name, cols_new),
    .init = data_nested
  )

  summaries_nested
}

nest_summary_cols <- function(data, fn_name, cols_new) {
  cols_to_nest <- stringr::str_subset(
    cols_new,
    stringr::str_glue("_{fn_name}$")
  )

  summary_nested <- tidyr::nest(
    data,
    "{fn_name}" := tidyr::all_of(cols_to_nest)
  )

  summary_nested
}

format_all_summaries <- function(summaries_nested, fns, labels_to) {
  summaries_formatted <- mutate_rowwise(
    summaries_nested,
    dplyr::across(
      tidyr::all_of(names(fns)),
      \(df) format_summary(df, dplyr::cur_column(), labels_to)
    )
  )

  summaries_formatted
}

format_summary <- function(df, fn_name, labels_to) {
  df_renamed <- dplyr::rename_with(df, \(col) strip_suffix(col, fn_name))

  df_renamed_labeled <- dplyr::mutate(df_renamed, "{labels_to}" := fn_name)

  df_out <- list(df_renamed_labeled)

  df_out
}

strip_suffix <- function(col, fn_name) {
  suffix_stripped <- stringr::str_remove(col, stringr::str_c("_", fn_name, "$"))

  suffix_stripped
}

pivot_longer_order <- function(summaries_nested_cleaned, order) {
  nested_ordered <- tidyr::pivot_longer(
    summaries_nested_cleaned,
    {{ order }},
    names_to = "..name",
    values_to = "..value"
  )

  nested_ordered
}

unnest_relocate <- function(nested_ordered, by, labels_to, cols_old) {
  selection <- dplyr::select(nested_ordered, !tidyr::all_of("..name"))

  unnested <- tidyr::unnest(selection, tidyr::all_of("..value"))

  out <- dplyr::relocate(
    unnested,
    {{ by }},
    tidyr::all_of(labels_to),
    tidyr::any_of(cols_old)
  )

  out
}
