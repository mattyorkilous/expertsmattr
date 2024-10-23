#' Read all the sheets in an excel file
#'
#' A wrapper around `readxl::read_excel()` that reads all the sheets in an
#' excel file.
#'
#' @param path Path to the xls/xlsx file.
#' @param .names A character vector of names to assign to the resulting list
#' of tables, or a function to apply to the workbook's sheet names. Default
#' is `readxl::excel_sheets(path)`
#' @param range As in `readxl::read_excel()`, but optionally a vector the same
#' size as the number of sheets in `path`.
#' @param col_names As in `readxl::read_excel()`, but optionally a vector the
#' same size as the number of sheets in `path`.
#' @param col_types As in `readxl::read_excel()`, but optionally a vector the
#' same size as the number of sheets in `path`.
#' @param na As in `readxl::read_excel()`, but optionally a vector the same
#' size as the number of sheets in `path`.
#' @param trim_ws As in `readxl::read_excel()`, but optionally a vector the same
#' size as the number of sheets in `path`.
#' @param skip As in `readxl::read_excel()`, but optionally a vector the same
#' size as the number of sheets in `path`.
#' @param n_max As in `readxl::read_excel()`, but optionally a vector the same
#' size as the number of sheets in `path`.
#' @param guess_max As in `readxl::read_excel()`, but optionally a vector the
#' same size as the number of sheets in `path`.
#' @param progress As in `readxl::read_excel()`, but optionally a vector the
#' same size as the number of sheets in `path`.
#' @param .name_repair As in `readxl::read_excel()`, but optionally a vector the
#' same size as the number of sheets in `path`.
#'
#' @return A named list of `data.frame`s
#' @export
#'
#' @examples
#' \dontrun{
#' read_all_sheets("my-workbook.xlsx")
#' }
read_all_sheets <- function(path,
                            .names = "sheet_names",
                            range = list(NULL),
                            col_names = TRUE,
                            col_types = list(NULL),
                            na = "",
                            trim_ws = TRUE,
                            skip = 0,
                            n_max = Inf,
                            guess_max = min(1000, n_max),
                            progress = readxl::readxl_progress(),
                            .name_repair = "unique") {
  sheet_names <- readxl::excel_sheets(path)

  stopifnot(identical(.names, "sheet_names") | is.function(.names))

  names <- if (is.function(.names)) .names(sheet_names) else sheet_names

  params <- list(
    path,
    sheet_names,
    range,
    col_names,
    col_types,
    na,
    trim_ws,
    skip,
    n_max,
    guess_max,
    progress,
    .name_repair
  )

  tables <- purrr::pmap(params, readxl::read_excel)

  out <- purrr::set_names(tables, names)

  out
}
