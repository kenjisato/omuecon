#' Insert text into a document.
#'
#' @param path Path to the child document.
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
includeText <- function(path, data = parent.frame(), quiet = TRUE) {
  path <- if (is.null(the$root.dir)) path else file.path(the$root.dir, path)
  rendered_text <- knitr::knit(text = readLines(path), envir = data, quiet = quiet)
  knitr::asis_output(rendered_text)
}


#' Include Audio media.
#'
#' @param url character. URL of the audio. The media must not be gated.
#' @param preload character. One of metadata, auto and none.
#' @param alt character. Message displayed when the browser does not support
#'   the audio tag.
#'
#' @return HTML audio tag is inserted into the document.
#' @export
#'
includeAudio <- function(url, preload = c("metadata", "auto", "none"),
                         alt = "Your browser does not support the audio element.") {
  preload <- match.arg(preload)
  x <- paste0('<audio controls src="', url,
              '" preload="', preload, '">', alt, '</audio>')
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
  src <- if (is.null(the$root.dir)) src else file.path(the$root.dir, src)
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



