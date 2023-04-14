
#' Create HTML block with inline CSS.
#'
#' @param file Path to the source HTML/Markdown file. If unspecified,
#'   [file.choose()] helps you to choose one.
#' @param dir Path to the output directory. If NULL, saved to the same directory
#'   as the source data.
#' @param clip logical. If TRUE (default), the result will be copied to
#'   the clipboard.
#' @param ... Paramters passed to [moodle_html_from_md()].
#'
#' @return character vector representing the resulting document (HTML fragment).
#' @export
#'
moodle_html <- function(file = NULL, dir = NULL, clip = TRUE, ...) {

  if (is.null(file)) {
    file <- file.choose()
  }

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

  xml2::write_html(article, output,
                   options = c("format_whitespace", "as_html"))

  invisible(strsplit(as.character(article), "\n")[[1]])
}


#' Convert a Markdown file into an HTML block to be copied and
#' pasted for Moodle.
#'
#' @param file File path to a markdown document.
#' @param dir Path to the output directory.
#' @param stylesheet Path to a CSS file.
#' @param template Path to an HTML template.
#' @param debug logical. If set to True, intermediate md file is kept.
#'
#' @return character vector representing the resulting document (HTML fragment).
moodle_html_from_md <- function(file, dir = NULL, stylesheet = NULL,
                                template = NULL, debug = FALSE) {

  tdir <- file.path(tempdir(), "moodle_html_from_md")
  dir.create(tdir, showWarnings = FALSE)
  intermediate_md <- tempfile(tmpdir = tdir, fileext = ".md")
  intermediate_html <- with_ext(file, "html", tdir)

  oopts_knit <- knitr::opts_knit$get()
  on.exit(knitr::opts_knit$set(oopts_knit))

  oopts_chunk <- knitr::opts_chunk$get()
  on.exit(knitr::opts_chunk$set(oopts_chunk), add = TRUE)

  knitr::opts_knit$set(base.dir = tdir)
  knitr::opts_chunk$set(eval = TRUE, echo = FALSE, fig.path = "figures/")

  knitr::knit(file, intermediate_md, quiet = TRUE)

  # Merge stylesheets and resolve CSS variables for :root.
  if (is.null(stylesheet))
    stylesheet <- css_find("style.css")
  else
    stylesheet <- css_find(stylesheet)

  css_str <- sapply(stylesheet, css_resolve)
  stylesheet <- tempfile(tmpdir = tdir, fileext = ".css")
  cat(css_str, file = stylesheet, sep = "\n")

  if (is.null(template)) {
    template <- system.file("xml", "template.html", package = .packageName)
  }

  out <- markdown::mark(file = intermediate_md,
                        output = NULL, format = "html",
                        template = template, meta = list(css = stylesheet))

  out <- gsub("<p>$$", "<p class=\"math\">$$", out, fixed = TRUE)
  writeLines(out, intermediate_html)

  # Style Inliner
  if (is.null(dir)) dir <- dirname(file)
  ret <- moodle_html_from_html(intermediate_html, dir)

  # Clean Up
  if (!debug) {
    unlink(tdir, recursive = TRUE)
  } else {
    message("Location for the intermediate files: \n", tdir)
  }

  invisible(ret)
}

