#' Compose Moodle page from (R)Markdown file
#'
#' This function facilitates writing Moodle pages with (R) Markdown.
#' It basically does three things:
#' 1. it converts (R) Markdown file into a HTML file with
#'    [knitr::knit()] + [markdown::mark()],
#' 2. inlines style information with [juicyjuice::css_inline()], and then
#' 3. extracts article/div element that is ready for copying and pasting into
#'    Moodle's source editor.
#'
#' @param file character. Path to the (R)markdown file.
#' @param dir character. Output directory.
#' @param tag character. Surrounding tag for the HTML block, "article" and "div"
#'      are allowed.
#' @param id character. ID attribute for the surrounding tag.
#' @param clip logical. Whether or not copy the result to clipboard. Ignored
#'    (set to FALSE) if `full_html = TRUE`. Default is TRUE.
#' @param full_html logical. Produce the complete HTML or HTML block only?
#' @param remove_script logical. Whether or not remove script tags. Ignored
#'    (set to FALSE) if `full_html = TRUE`. Default is FALSE
#' @param stylesheet character. Paths to the CSS files used in markdown::mark()
#' @param template character. Path to the template used in markdown::mark()
#'
#' @return Invisibly returns a character vector identical to the result file.
#' @export
#'
convert <- function(file = NULL, dir = NULL, tag = c("article", "div"),
                    id = NULL, clip = TRUE, full_html = FALSE,
                    remove_script = FALSE, stylesheet = NULL, template = NULL) {

  # Input file and output directory.
  if (length(file) > 1) {
    stop(str_glue("{sQuote('convert()')} can handle only one file at a time."))
  }
  file <- getd(file, file.choose())
  dir <- getd(dir, dirname(file))

  # Clear 'the' internal data store.
  rm(list = ls(the), envir = the)
  the$tempdir <- tempfile(pattern = "dir")
  dir.create(the$tempdir)
  on.exit(unlink(the$tempdir, recursive = TRUE), add = TRUE)

  # Conversion options
  the$file <- file
  the$root.dir <- dirname(file)
  the$dir <- dir
  the$tag <- match.arg(tag)
  the$id <- id
  the$full_html <- full_html
  the$remove_script <- remove_script

  the$stylesheet <- getd(stylesheet, getOption(str_glue("omu.{the$tag}.css")))
  the$template <- getd(template, getOption(str_glue("omu.{the$tag}.template")))

  # Conversion
  text <- readLines(file, warn = FALSE)
  html <- if (grepl("r?md", tolower(tools::file_ext(file)))) {
    ## knitr::knit() & markdown::mark()
    convert_markdown2html(text)
  } else {
    text
  }

  # Full HTML to HTML Fragment for Moodle
  moodle <- convert_html2moodle(html)

  # Copy to Clipboard
  if (clip && full_html) {
    message("Not copied to the clipboard because full_html is set to TRUE.")
  } else if (clip && !full_html) {
    clipr::write_clip(moodle, breaks = "\n")
    message("HTML code has been copied to the clipboard. Now you can paste it to Moodle.")
  }

  out_file <- with_ext(file, ext = "html", dir = dir)
  out_file <- dodge_name(out_file, file)

  writeLines(moodle, out_file)
  invisible(moodle)
}

