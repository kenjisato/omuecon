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

