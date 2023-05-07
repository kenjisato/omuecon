#' Insert text into a document.
#'
#' @param path Path to the child document.
#' @param tmpfile Output name of an intermediate file created by `knitr::knit`.
#'   If unspecified, [tempfile()] prepares it and remove it when done.
#' @param data Environment or list used to knit the child document.
#' @param quiet Passed to [knitr::knit()].
#'
#' @return NULL
#' @export
#'
#' @examples
#' template <- tempfile()
#' cat("Hello, `r name`.\n", file = template)
#' includeText(template, data = list(name = "Alice"))
#' file.remove(template)
#'
includeText <- function(path, tmpfile = NULL,
                        data = parent.frame(), quiet = TRUE) {
  if (is.null(tmpfile)) {
    tmpfile <- tempfile()
    on.exit(file.remove(tmpfile))
  }
  knitr::knit(path, tmpfile, envir = data, quiet = quiet)
  rendered_text <- readLines(tmpfile)
  knitr::asis_output(rendered_text)
}


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
    template <- system.file("xml/schedule.html", package = PKG)
  }
  template_text <- paste(readLines(template), collapse = "\n")
  writeLines(whisker::whisker.render(template_text))
}


#' Include Summary/Details block.
#'
#' @param summary character. Text to show, typically, question string.
#' @param detail character. Texts to hide, typically, answer string.
#' @param summary_label character. Label for the open block.
#' @param detail_label character. Label for the hidden block.
#' @param ... character. Concatenated with the detail argument.
#'
#' @return HTML5 summary/details block is inserted.
#' @export
includeQuestion <- function(summary, detail,
                            summary_label = "Quiz",
                            detail_label = "Answer", ...) {
  detail <- paste0("<p>", c(detail, ...), "</p>")
  result <- c(
    "<details>",
    paste0("<summary><span class = 'summary-label'>", summary_label, "</span>"),
    "<p>",
    summary,
    "</p></summary>",
    "<section>",
    paste0("<p><strong>", detail_label, "</strong><br>"),
    detail,
    "</section>",
    "</details>"
  )
  knitr::asis_output(result)
}

#' Include Audio media.
#'
#' @param url character. URL of the audio. The media must not be gated.
#'
#' @return HTML5 audio tag is inserted into the document.
#' @export
#'
includeAudio <- function(url) {
  x <- paste0("<audio controls src=\"", url, "\"></audio>")
  knitr::asis_output(x)
}

#' Include graphic media.
#'
#' This function is a wrapper for [knitr::include_graphics()].
#' Since omuecon creates intermediate files in a temporary directory,
#' calling [knitr::include_graphics()] does not include local images.
#' This function copies the media into the temporary directory and
#' calls [knitr::include_graphics()].
#'
#' @param src character. Path to the image.
#' @param ... Parameters passed to [knitr::include_graphics()], other than path.
#'
#' @return The result of [knitr::include_graphics()]
#' @export
#'
includeGraphics <- function(src, ...) {
  file.copy(src, file.path(tempdir(), "moodle_html_from_md"))
  knitr::include_graphics(path = src, ...)
}


#' Include YouTube video.
#'
#' @param url character. URL of the YouTube video.
#' @param .class character. The resulting iframe tag is surrounded by div block.
#'   You can specify its class attribute to modify its styling.
#'
#' @return An embed code of a YouTube video is inserted.
#' @export
#'
includeYT <- function(url, .class = 'includeYT') {
  id <- sub("https://www.youtube.com/watch?v=", "", url, fixed = TRUE)
  id <- sub("https://youtu.be/", "", id, fixed = TRUE)

  div <- paste0('<div class = "', .class, '">')
  iframe <- paste0(
    '<iframe width="560" height="315" src="https://www.youtube.com/embed/',
    id,
    '" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>'
  )

  x <- paste(div, iframe, '</div>', collapse = "\n")
  knitr::asis_output(x)
}



