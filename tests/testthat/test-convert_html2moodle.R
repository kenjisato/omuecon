test_that("convert_html2moodle", {
  res <- convert_html2moodle(
    in_text = readLines(pkg_file("samples/html-sample-1.html"), warn = FALSE),
    full_html = FALSE, tag = "article", remove_script = FALSE)
  expected_result <- c(
    "<article><h1 style=\"color: blue;\">Hello, omuecon</h1>",
    "<p class=\"big\" style=\"font-size: 200%;\">This is a big paragraph.</p>",
    "<p class=\"small\" style=\"font-size: 70%;\">Here's the small paragraph.</p>",
    "<h2 style=\"color: gray;\">Bye</h2></article>"
  )
  expect_equal(res, expected_result)
})
