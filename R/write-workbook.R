#' Write one or many tables to an excel workbook, with optional NERA-styling
#'
#' `write_workbook()` allows you to write a list of tables to different sheets
#' of the same excel workbook via a single function call. Use additional
#' arguments to control formatting details and add headers.
#'
#' @param tables A list of \code{data.frame}s or a single \code{data.frame}.
#' @param file_name The name of the file to write to.
#' @param sheet_names A character vector of sheet names, or a function to apply
#' to names(tables)
#' @param headers A character vector, one element for each header line. Use
#' \code{NULL} for no headers.
#' @param font_name A font name to use as the default in the workbook.
#' @param font_size A font size to use as the default in the workbook.
#' @param bold_column_names If \code{TRUE} column names are bold.
#' @param add_filters If \code{TRUE} adds filters to columns.
#' @param auto_fit If \code{TRUE} auto-fits columns (imperfectly).
#' @param min_width Minimum column width for auto-fitting.
#' @param max_width Maximum column width for auto-fitting.
#' @param freeze_panes If \code{TRUE} freezes table at row below column names.
#' @param overwrite If \code{TRUE} overwrites file.
#'
#' @return NULL
#' @export
#'
#' @examples
#' df1 <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6))
#' df2 <- data.frame(C = c(1, 2, 3), D = c(4, 5, 6))
#' \dontrun{
#' write_workbook(
#'   list(df1, df2),
#'   c("Table 1", "Table 2"),
#'   "my-workbook"
#' )
#' }
write_workbook <- function(tables,
                           file_name,
                           sheet_names = names(tables),
                           headers = c(
                             "Privileged and Confidential",
                             "Prepared at the Request of Counsel"
                           ),
                           font_name = "Times New Roman",
                           font_size = 11,
                           bold_column_names = TRUE,
                           add_filters = TRUE,
                           auto_fit = TRUE,
                           min_width = 8.43,
                           max_width = 25.29,
                           freeze_panes = TRUE,
                           overwrite = TRUE) {
  wb <- openxlsx::createWorkbook()

  if (inherits(tables, "data.frame")) {
    tables <- list(tables)
  }

  if (is.function(sheet_names)) {
    sheet_names <- sheet_names(names(tables))
  }

  if (is.null(sheet_names)) {
    sheet_names <- paste0("Sheet", seq_along(tables))
  }

  params <- list(
    tables,
    sheet_names,
    wb,
    file_name,
    headers,
    font_name,
    font_size,
    bold_column_names,
    add_filters,
    auto_fit,
    min_width,
    max_width,
    freeze_panes
  )

  purrr::pwalk(params, write_to_worksheet)

  openxlsx::saveWorkbook(wb, file_name, overwrite = overwrite)
}

write_to_worksheet <- function(table,
                               sheet_name,
                               wb,
                               file_name,
                               headers,
                               font_name,
                               font_size,
                               bold_column_names,
                               add_filters,
                               auto_fit,
                               min_width,
                               max_width,
                               freeze_panes) {
  openxlsx::modifyBaseFont(wb, fontSize = font_size, fontName = font_name)

  bold_style <- openxlsx::createStyle(textDecoration = "bold")

  start_row <- if (length(headers) == 0) 1 else length(headers) + 2

  openxlsx::addWorksheet(wb, sheet_name)

  openxlsx::writeData(
    wb,
    sheet_name,
    table,
    startRow = start_row,
    withFilter = add_filters
  )

  params_headers <- list(wb, sheet_name, headers, seq_along(headers))

  purrr::pwalk(params_headers, write_header)

  last_bold_row <- if (bold_column_names) start_row else start_row - 1

  if (last_bold_row > 0) {
    openxlsx::addStyle(
      wb,
      sheet_name,
      bold_style,
      rows = 1:last_bold_row,
      cols = 1:ncol(table),
      gridExpand = TRUE
    )
  }

  if (auto_fit) {
    handle_auto_fit(
      table,
      bold_column_names,
      add_filters,
      min_width,
      max_width,
      wb,
      sheet_name
    )
  }

  if (freeze_panes) {
    openxlsx::freezePane(
      wb,
      sheet_name,
      firstActiveRow = start_row + 1,
      firstActiveCol = 1
    )
  }
}

write_header <- function(wb, sheet_name, header, start_row) {
  openxlsx::writeData(wb, sheet_name, header, startRow = start_row)
}

handle_auto_fit <- function(table,
                            bold_column_names,
                            add_filters,
                            min_width,
                            max_width,
                            wb,
                            sheet_name) {
  widths_data <- purrr::map_dbl(table, get_data_width)

  bf <- if (bold_column_names) 1.05 else 1 # Bold is a bit wider

  params_widths_names <- list(colnames(table), bf, add_filters)

  widths_names <- purrr::pmap_dbl(params_widths_names, get_name_width)

  widths <- pmax(widths_data, widths_names, min_width)

  widths <- pmin(widths, max_width)

  openxlsx::setColWidths(
    wb,
    sheet_name,
    cols = 1:ncol(table),
    widths
  )
}

get_data_width <- function(col) {
  data_width <- max(nchar(as.character(col)), na.rm = TRUE) + 1

  data_width
}

get_name_width <- function(colname, bf, add_filters) {
  name_width <- (nchar(as.character(colname)) * bf)
    + (2 * as.integer(add_filters)) # Filters make columns wider

  name_width
}
