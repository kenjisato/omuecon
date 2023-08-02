
#' Wrappers for [moodle_html()]
#'
#' Convert markdown file to HTML fragment ready to be copy-pasted
#' for Moodle page or label, etc.
#'
#' The output of [moodle_page()] is meant for Moodle pages. It
#' creates `article` tag block that makes the main part of a single
#' page. [moodle_label()] creates a more smaller part of the entire
#' Moodle sites, contained in a `div` block. It is used typically
#' for labels, in which more tighter style may be appropriate.
#'
#' @param file character. Path to the markdown file.
#' @param dir character. Path to the output dir.
#' @param ... Arguments passed on to [moodle_html()]
#'
#' @return character vector representing the resulting document (HTML fragment).
#' @export
#'
moodle_page <-function(file = NULL, dir = dirname(file), ...) {
  stylesheet <- "style.css"
  moodle_html(file, dir, stylesheet = stylesheet, tag = "article", ...)
}


#' @describeIn moodle_page Wrapper for making snippets for Moodle labels.
#' @export
moodle_label <-function(file = NULL, dir = dirname(file), ...) {
  stylesheet <- "label.css"
  moodle_html(file, dir, stylesheet = stylesheet, tag = "div", ...)
}


#' Create HTML block with inline CSS.
#'
#' @param file Path to the source HTML/Markdown file. If unspecified,
#'   [file.choose()] helps you to choose one.
#' @param dir Path to the output directory. If NULL, saved to the same directory
#'   as the source data.
#' @param clip logical. If TRUE (default), the result will be copied to
#'   the clipboard. When full_html is TRUE, the copying will be skipped.
#' @param ... Paramters passed to [moodle_html_from_html()] and [moodle_html_from_md()].
#'
#' @return character vector representing the resulting document (HTML fragment).
#' @export
#'
moodle_html <- function(file = NULL, dir = dirname(file), clip = TRUE, ...) {

  if (is.null(file)) {
    file <- file.choose()
  }

  ext <- tolower(tools::file_ext(file))
  res <- switch(ext,
          html = moodle_html_from_html(file, dir, ...),
          md = moodle_html_from_md(file, dir, ...),
          rmd = moodle_html_from_md(file, dir, ...),
          stop(paste0("Extension ", ext, " is not supported."))
          )

  if (clip) {
    if (!is.null(full_html <- list(...)[["full_html"]]) && full_html) {
      message("Not copied to the clipboard because full_html is set to TRUE.")
    } else {
      clipr::write_clip(res, breaks = "\n")
      message("HTML code has been copied to the clipboard. Now you can paste it to Moodle.")
    }
  }

  invisible(res)
}

#' Convert a standalone HTML file into an HTML block to be copied and
#' pasted for Moodle.
#'
#' @param file character. Path to a standalone HTML file.
#' @param dir character. Path to the directory to save the resulting HTML file.
#' @param tag character. Outer-most tag for the resulting HTML snippet.
#' @param id character. id attribute for the outer-most tag.
#' @param full_html logical. If TRUE, produce complete html output,
#'   convenient when drafting a document. When this option is enabled, tag and
#'   id options are silently ignored.
#' @param remove_script logical. If TRUE, script tags are all stripped out.
#'
#' @return character. HTML block.
moodle_html_from_html <- function(
    file, dir = dirname(file), tag = "article", id = NULL, full_html = FALSE,
    remove_script = FALSE) {
  orig_html <- paste(readLines(file), collapse = "\n")

  inlined_html <-
    orig_html %>%
    juicyjuice::css_inline() %>%
    rvest::read_html()

  if (full_html) {
    container_tag <- inlined_html
  } else {
    body <- rvest::html_element(inlined_html, "body")

    if (remove_script) {
      scripts <- rvest::html_elements(body, "script")
      for (script in scripts) {
        xml2::xml_remove(script)
      }
    }

    body_attr <- rvest::html_attrs(body)
    body_children <- rvest::html_children(body)

    container_tag <-
      rvest::read_html(str_glue("<{tag}></{tag}>")) %>%
      rvest::html_element(tag)

    if (!is.null(id)) xml2::xml_attr(container_tag, "id") <- id
    xml2::xml_attr(container_tag, "style") <- body_attr

    for (child in body_children) {
      xml2::xml_add_child(container_tag, child)
    }
  }

  output <- with_ext(file, "html", dir)

  # Input file may be identical to the output target...
  if (file.exists(output)) {
    in_file <- tools::file_path_as_absolute(file)
    out_file <- tools::file_path_as_absolute(output)
    if (in_file == out_file) {
      output <- paste0(tools::file_path_sans_ext(output), "-out.html")
    }
  }

  xml2::write_html(container_tag, output,
                   options = c("format_whitespace", "as_html"))

  invisible(strsplit(as.character(container_tag), "\n")[[1]])
}


#' Convert a Markdown file into an HTML block to be copied and
#' pasted for Moodle.
#'
#' @param file File path to a markdown document.
#' @param dir Path to the output directory.
#' @param stylesheet Path to a CSS file.
#' @param template Path to an HTML template.
#' @param debug logical. If set to True, intermediate md file is kept.
#' @param ... parameters passed on to [moodle_html_from_html()]
#'
#' @return character vector representing the resulting document (HTML fragment).
moodle_html_from_md <- function(file, dir = dirname(file),
                                stylesheet = getOption("omuecon.stylesheet", "style.css"),
                                template = NULL, debug = FALSE, ...) {

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

  # tweaks for math equations
  correct_equations(intermediate_md)

  # Merge stylesheets and resolve CSS variables for :root.
  stylesheet <- css_find(stylesheet)

  css_str <- sapply(stylesheet, css_resolve)
  combined_stylesheet <- tempfile(tmpdir = tdir, fileext = ".css")
  for (css_i in css_str) {
    if (file.exists(css_i)) {
      cat(readLines(css_i), file = combined_stylesheet, sep = "\n", append = TRUE)
    } else {
      cat(css_i, file = combined_stylesheet, append = TRUE)
    }
  }

  if (is.null(template)) {
    template <- system.file("xml", "template.html", package = .packageName)
  }

  out <- markdown::mark(file = intermediate_md,
                        output = NULL, format = "html",
                        template = template,
                        meta = list(css = combined_stylesheet),
                        options = "-smartypants")

  out <- gsub("<p>$$", "<p class=\"math\">$$", out, fixed = TRUE)

  writeLines(na.omit(out), intermediate_html)

  # Style Inliner
  ret <- moodle_html_from_html(intermediate_html, dir, ...)

  # Clean Up
  if (!debug) {
    unlink(tdir, recursive = TRUE)
  } else {
    message("Location for the intermediate files: \n", tdir)
  }

  invisible(ret)
}

