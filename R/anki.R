
#' Hide/Show text
#'
#' This function inserts JavaScript code for anki texts.
#' This command should be run at the end of the markdown document.
#'
#' @param background character. Background color of the blank box.
#' @param color character. Font color of the answer of the blank box.
#' @param border character. Border color of the blank box.
#' @param padding integer. Space between the answer text and the border.
#' @param margin integer. Left and right margin around the border.
#' @param cursor character.
#'
#' @return character. CSS/JavaScript code for anki texts.
#' @export
#'
anki_setup <- function(background = "yellow", color = "blue", border = "slateblue",
                       padding = 3, margin = 3, cursor = "pointer"){

  jq <- jquery()

  css <- glue::glue("<style>
  .anki-marker {
    background-color: (background);
    padding: (padding)px;
    margin-left: (margin)px;
    margin-right: (margin)px;
    border: 1px solid (border);
    cursor: (cursor);
  }
  .anki-marker .anki-text {
    color: (color);
    opacity: 0;
  }
  </style>", .open = "(", .close = ")")

  js <- '<script src="https://kenjisato.github.io/omuecon/inst/js/anki.js"></script>'

  x <- paste(css, jq, js, sep = "\n")
  knitr::asis_output(x)
}


#' Hide/Show text
#'
#' This function inserts HTML snippet to help students memorize important concepts.
#' To make this function to work, you need to run `anki_setup()` at the end of
#' the markdown document.
#'
#' @param text character. Text to toggle.
#'
#' @return HTML code span.
#' @export
#'
anki <- function(text) {
  x <- glue::glue('<span class = "anki-marker"><span class = "anki-text">{text}</span></span>')
  knitr::asis_output(x)
}
