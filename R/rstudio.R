
preview <- function(stylesheet = NULL, ...) {
  css <- css_find(c(stylesheet, ...))

  function(input, ...) {
    omuecon::moodle_html(file = input, clip = FALSE, full_html = TRUE,
                         stylesheet = css)
    htmlFile <- paste0(tools::file_path_sans_ext(input), ".html")
    message("Preview output created: ", htmlFile, "\n",
            "Please verify the final result on your Moodle site."
    )
    message("To produce the final result, run omuecon::moodle_html() ",
            "in the console.")

    message("CSS files used: \n", paste(css, collapse = "\n"))
  }
}
