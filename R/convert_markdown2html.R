convert_markdown2html <- function(
    in_text, template = the$template, stylesheet = the$stylesheet) {

  template <- getd(template, getOption("omuecon.template"))
  stylesheet <- getd(stylesheet, getOption("omuecon.article.css"))

  tdir <- getd(the$tempdir, tempdir())
  dir <- getd(the$dir, tdir)

  oopts_knit <- knitr::opts_knit$get()
  knitr::opts_knit$set(upload.fun = knitr::image_uri)
  on.exit(knitr::opts_knit$set(oopts_knit), add = TRUE)

  oopts_chunk <- knitr::opts_chunk$get()
  knitr::opts_chunk$set(eval = TRUE, echo = FALSE, fig.path = "figures/",
                        fig.cap = "")
  on.exit(knitr::opts_chunk$set(oopts_chunk), add = TRUE)

  intermediate_text <- knitr::knit(text = in_text, quiet = TRUE)

  # tweaks for math equations
  intermediate_text <- correct_equations_(intermediate_text)

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

  out <- suppressWarnings(
    markdown::mark(
      text = intermediate_text,
      output = NULL, format = "html",
      template = template,
      meta = list(css = combined_stylesheet),
      options = "-smartypants"
    )
  )

  out <- gsub("<p>$$", "<p class=\"math\">$$", out, fixed = TRUE)
  out
}

