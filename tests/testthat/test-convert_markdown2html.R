test_that("multiplication works", {
  css <- tempfile(pattern = "file", fileext = ".css")
  on.exit(unlink(css), add = TRUE)
  cat("html {}\n", file = css)

  out <- convert_markdown2html("# hello", stylesheet = css, template = FALSE)
  exp <- "<h1 id=\"hello\">hello</h1>\n"
  expect_equal(out, exp)
})
