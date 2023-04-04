
#' Create HTML block with inline CSS.
#'
#' @param file Path to the source HTML/Markdown file. s
#' @param dir Path to the output directory. If NULL, saved to the same directory
#'   as the source data.
#' @param clip logical. If TRUE (default), the result will be copied to
#'   the clipboard.
#' @param ... Paramters passed to [moodle_html_from_md()].
#'
#' @return character vector representing the resulting document (HTML fragment).
#' @export
#'
moodle_html <- function(file, dir = NULL, clip = TRUE, ...) {
  ext <- tolower(tools::file_ext(file))
  res <- switch(ext,
        html = moodle_html_from_html(file, dir),
        md = moodle_html_from_md(file, dir, ...),
        rmd = moodle_html_from_md(file, dir, ...),
        stop(paste0("Extension ", ext, " is not supported.")))

  if (clip) {
    clipr::write_clip(res, breaks = "\n")
    message("HTML code has been copied to the clipboard. Now you can paste it to Moodle.")
  }

  invisible(res)
}

#' Convert a standalone HTML file into an HTML block to be copied and
#' pasted for Moodle.
#'
#' @param file Path to a standalone HTML file.
#' @param dir Path to the directory to save the resulting HTML file.
#'
#' @return Invisibly returns a character vector.
moodle_html_from_html <- function(file, dir = NULL) {
  orig_html <- paste(readLines(file), collapse = "\n")

  inlined_html <-
    orig_html |>
    juicyjuice::css_inline() |>
    rvest::read_html()

  body <- rvest::html_element(inlined_html, "body")

  scripts <- rvest::html_elements(body, "script")
  for (script in scripts) {
    xml2::xml_remove(script)
  }

  body_attr <- rvest::html_attrs(body)
  body_children <- rvest::html_children(body)

  article <-
    rvest::read_html("<article></article>") |>
    rvest::html_element("article")

  xml2::xml_attr(article, "style") <- body_attr

  for (child in body_children) {
    xml2::xml_add_child(article, child)
  }

  if (is.null(dir)) dir <- dirname(file)

  output <- with_ext(file, "html", dir)

  # Input file may be identical to the output target...
  if (file.exists(output)) {
    in_file <- tools::file_path_as_absolute(file)
    out_file <- tools::file_path_as_absolute(output)
    if (in_file == out_file) {
      output <- paste0(tools::file_path_sans_ext(output), "-out.html")
    }
  }

  xml2::write_html(article, output)

  invisible(strsplit(as.character(article), "\n")[[1]])
}


#' Convert a Markdown file into an HTML block to be copied and
#' pasted for Moodle.
#'
#' @param file File path to a markdown document.
#' @param dir Path to the output directory.
#' @param stylesheet Path to a CSS file.
#' @param template Path to an HTML template.
#'
#' @return character vector representing the resulting document (HTML fragment).
moodle_html_from_md <- function(file, dir = NULL, stylesheet = NULL,
                                template = NULL) {

  knitr::opts_chunk$set(eval = TRUE, echo = FALSE)

  tdir <- file.path(tempdir(), "moodle_html_from_md")
  dir.create(tdir, showWarnings = FALSE)
  intermediate_md <- tempfile(tmpdir = dirname(file), fileext = ".md")

  intermediate_html <- with_ext(file, "html", tdir)
  knitr::knit(file, intermediate_md, quiet = TRUE)

  # Render markdown
  if (is.null(stylesheet))
    stylesheet <- system.file("css/style.css", package = PKG)

  if (is.null(template)) {
    template <- system.file("xml/template.html", package = PKG)
  } else {
    template <- TRUE
  }
  out <- markdown::mark(intermediate_md, format = "html", template = template,
                 meta = list(css = stylesheet))

  out <- gsub("<p>$$", "<p class=\"math\">$$", out, fixed = TRUE)
  writeLines(out, intermediate_html)

  # Clean Up
  file.remove(intermediate_md)

  # Style Inliner
  if (is.null(dir)) dir <- dirname(file)
  ret <- moodle_html_from_html(intermediate_html, dir)

  # Clean Up
  unlink(tdir, recursive = TRUE)

  invisible(ret)
}

