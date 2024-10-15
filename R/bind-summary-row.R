bind_summary_row <- function(data,
                             cols,
                             fns,
                             labels_to = NULL,
                             by = NULL,
                             below = TRUE) {
  by_cols <- colnames(dplyr::select(data, {{ by }}))

  if (is.null(labels_to)) {
    stop("Must choose a column for labels")
  }

  out <- data |>
    dplyr::mutate(
      dplyr::across(
        {{ cols }},
        fns,
        .names = "{.col}_{.fn}"
      ),
      .by = {{ by }}
    )

  new_cols <- tidyr::expand_grid(
    col = colnames(dplyr::select(data, {{ cols }})),
    fn = names(fns)
  ) |>
    mutate_rowwise(
      new_col = stringr::str_glue("{col}_{fn}")
    )

  out <- out |>
    tidyr::nest(
      .by = c(
        {{ by }},
        tidyr::all_of(
          purrr::pluck(new_cols, "new_col")
        )
      )
    )

  out <- purrr::reduce(
    names(fns),
    \(data, fn_name) nest_summary_cols(data, fn_name, new_cols),
    .init = out
  )

  out <- out |>
    mutate_rowwise(
      dplyr::across(
        tidyr::all_of(names(fns)),
        \(x) list(
          x |>
            dplyr::rename_with(
              \(col) stringr::str_remove(
                string = col,
                pattern = stringr::str_c("_", dplyr::cur_column(), "$")
              )
            ) |>
            dplyr::mutate(
              "{labels_to}" := dplyr::cur_column()
            )
        )
      )
    )

  if (below) {
    out <- out |>
      tidyr::pivot_longer(
        c(data, tidyr::all_of(names(fns))),
        names_to = "..name",
        values_to = "..value"
      )
  } else {
    out <- out |>
      tidyr::pivot_longer(
        c(tidyr::all_of(names(fns)), data),
        names_to = "..name",
        values_to = "..value"
      )
  }

  out <- out |>
    dplyr::select(!..name) |>
    tidyr::unnest(..value) |>
    dplyr::relocate(any_of(by_cols), all_of(labels_to)) |>
    dplyr::relocate({{ cols  }}, .after = dplyr::last_col())

  out
}

nest_summary_cols <- function(data, fn_name, new_cols) {
  new_col <- dplyr::filter(new_cols, fn == fn_name) |>
    purrr::pluck("new_col")

  data |>
    tidyr::nest("{fn_name}" := tidyr::all_of(new_col))
}
