includeSchedule <- function(df = NULL, path = NULL, template = NULL, ...) {

  stopifnot("Specify either df or path, not both." =
              xor(is.null(df), is.null(path)))

  if (!is.null(path)) df <- readxl::read_excel(path)

  cols <- c("start_date", "start_time", "end_date", "end_time",
            "todo", "detail")
  matched <- match(cols, names(df))

  if (!all(!is.na(matched))) {
    message("Missing columns: ", paste(cols[is.na(matched)], collapse = ","))
    stop("Unsupported Excel file.")
  }

  df$start_date <- formatDate(as.Date(df$start_date), ...)
  df$end_date <- formatDate(as.Date(df$end_date), ...)

  df$date_period <-
    dplyr::case_when(
      is.na(df$start_date) & is.na(df$end_date) ~ "",
      is.na(df$end_date) ~ df$start_date,
      .default = paste0(df$start_date, "<br>", "\u2193", "<br>", df$end_date)
    )

  df$time_period <-
    dplyr::case_when(
      is.na(df$start_time) & is.na(df$end_time) ~ "",
      is.na(df$end_time) ~ df$start_time,
      .default = paste0(df$start_time, "<br>", "", "<br>", df$end_time)
    )

  df$todo <- stringr::str_replace_all(df$todo, "\n", "<br>")

  df$detail <- stringr::str_replace_all(df$detail, "\n", "<br>")
  df$detail <- dplyr::if_else(is.na(df$detail), "", df$detail)

  dat <- unname(whisker::rowSplit(df))

  if (is.null(template)) {
    template <- system.file("xml/schedule.html", package = .packageName)
  }
  template_text <- paste(readLines(template), collapse = "\n")
  writeLines(whisker::whisker.render(template_text))
}
