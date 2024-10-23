library(dplyr)

data(starwars)

test_that("sums_are_accurate", {
    data <- starwars |>
      select(name, height)

    out <- data |>
      bind_summary_row(
        cols = height,
        fns = list("Total" = \(x) sum(x, na.rm = TRUE)),
        labels_to = "name",
        below = FALSE
      )

    sum_bsr <- dplyr::filter(out, .data[["name"]] == "Total") |>
      purrr::pluck("height", 1)

    sum_manual <- sum(data$height, na.rm = TRUE)

    expect_equal(sum_bsr, sum_manual)

    data_by <- starwars |>
      select(sex, name, height)

    out_by <- data_by |>
      bind_summary_row(
        cols = height,
        fns = list("Total" = \(x) sum(x, na.rm = TRUE)),
        labels_to = "name",
        by = sex,
        below = FALSE
      )

    sum_bsr_male <- dplyr::filter(
      out_by,
      .data[["name"]] == "Total",
      .data[["sex"]] == "male"
    ) |>
      purrr::pluck("height", 1)

    sum_manual_male <- sum(
      dplyr::filter(
        data_by,
        .data[["sex"]] == "male"
      )$height,
      na.rm = TRUE
    )

    expect_equal(sum_bsr_male, sum_manual_male)
})
